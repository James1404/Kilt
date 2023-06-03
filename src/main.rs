use core::panic;
use std::{cell::RefCell, fs::*, io, io::prelude::*, process::Command, borrow::Borrow};

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    None,

    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Identifier(String),
    Plus, Minus, Multiply, Divide,
    Equal,
    Colon, Semicolon,

    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,

    Let, Const,
    If, Else, For, While,

    Comma, Dot,
    
    Eof
}

#[derive(Debug, Clone, PartialEq)]
struct Token {
    line: u64,
    location: usize,
    kind: TokenType
}

macro_rules! error {
    ($t:expr, $($arg:tt)*) => {{
        print!("Error [Line: {}, Loc: {}]: ", $t.line, $t.location);
        println!($($arg)*);
    }};
}

macro_rules! error_ast {
    ($t:expr, $($arg:tt)*) => {{
        match $t {
            AstNode::Ident(value) => {
                print!("Error [Line: {}, Loc: {}]: ", value.line, value.location);
            },
            _ => {}
        }
        println!($($arg)*);
    }};
}

fn to_keyword(id: &str) -> TokenType {
    match id {
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "for" => TokenType::For,
        "while" => TokenType::While,
        "true" => TokenType::Bool(true),
        "false" => TokenType::Bool(false),
        "let" => TokenType::Let,
        "const" => TokenType::Const,
        _ => TokenType::Identifier(id.to_string())
    }
}

fn lexer(source: &str) -> Vec<Token> {
    let mut token_stream: Vec<Token> = Vec::new();

    let mut line = 1 as u64;
    let mut location = 0 as usize;

    let mut current = 0 as usize;
    loop {
        if current >= source.len() {
            break;
        }

        let start = current;
        let mut kind = TokenType::None;

        let advance = | current: &mut usize, location: &mut usize | { if *current < source.len() { *current += 1; *location += 1; } };
        let current_char = move | current: usize | -> char { source.chars().nth(current).unwrap() };
        let peek = move | current: usize | -> char { source.chars().nth(current + 1).unwrap() };

        match current_char(current) {
            '+' => kind = TokenType::Plus,
            '-' => kind = TokenType::Minus,
            '*' => kind = TokenType::Multiply,
            '/' => kind = TokenType::Divide,
            '=' => kind = TokenType::Equal,
            ',' => kind = TokenType::Comma,
            '.' => kind = TokenType::Dot,
            '(' => kind = TokenType::LParen,
            ')' => kind = TokenType::RParen,
            '[' => kind = TokenType::LBracket,
            ']' => kind = TokenType::RBracket,
            '{' => kind = TokenType::LBrace,
            '}' => kind = TokenType::RBrace,
            ':' => kind = TokenType::Colon,
            ';' => kind = TokenType::Semicolon,
            '"' => {
                while peek(current) != '"' {
                    advance(&mut current, &mut location);
                }

                advance(&mut current, &mut location);
                kind = TokenType::String(source[start+1..current].to_string());
            }
            '\n' => {
                line += 1;
                location = 0;

                advance(&mut current, &mut location);
                continue;
            },
            ' ' | '\t' | '\r' => {
                advance(&mut current, &mut location);
                continue;
            },
            _ => {
                if current_char(current).is_numeric() {
                    let mut is_float = false;
                    while peek(current).is_numeric() {
                        advance(&mut current, &mut location);
                        if current_char(current) == '.' {
                            advance(&mut current, &mut location);
                            is_float = true;
                        }
                    }

                    kind = if is_float {
                        TokenType::Float(source[start..=current].parse::<f64>().unwrap())
                    } else {
                        TokenType::Int(source[start..=current].parse::<i64>().unwrap())
                    };
                }
                else if current_char(current).is_alphabetic() {
                    while matches!(peek(current), 'A'..='Z' | 'a'..='z' | '_') {
                        advance(&mut current, &mut location);
                    }

                    kind = to_keyword(&source[start..=current]);
                }
            }
        }

        let t = Token {
            line: line,
            location: location,
            kind: kind
        };

        token_stream.push(t);
        advance(&mut current, &mut location);
    }

    token_stream.push(Token {
        line: u64::MAX,
        location: usize::MAX,
        kind: TokenType::Eof
    });

    return token_stream;
}

#[derive(Debug, Clone, PartialEq)]
enum VarType {
    Const,
    Let
}

#[derive(Debug, Clone, PartialEq)]
enum AstNode {
    None,

    Ident(Token),
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Sequence(Vec<Box<AstNode>>),
    VarDecl(VarType, Box<AstNode>, Box<AstNode>),
    InferredDecl(VarType, Box<AstNode>),
    Assign(Box<AstNode>, Box<AstNode>),
    Binary(Box<AstNode>, Token, Box<AstNode>),
    Function(Box<AstNode>, Vec<Box<AstNode>>, Box<AstNode>),
    Call(Box<AstNode>, Vec<Box<AstNode>>),
    If(Box<AstNode>, Box<AstNode>),
    For(Box<AstNode>, Box<AstNode>, Box<AstNode>, Box<AstNode>),
    While(Box<AstNode>, Box<AstNode>),
}

struct Parser {
    current: usize,
    token_stream: RefCell<Vec<Token>>
}

fn precedence(token: &Token) -> i64 {
    match token.kind {
        TokenType::Plus => 2,
        TokenType::Minus => 3,
        TokenType::Multiply => 4,
        TokenType::Divide => 5,
        _ => -1
    }
}

impl Parser {
    fn advance(&mut self) {
        if self.current < self.token_stream.borrow().len() {
            self.current += 1;
        }
    }

    fn expect(&mut self, expected: TokenType) -> bool {
        let r = self.token_stream.borrow()[self.current].kind == expected;

        if r {
            self.advance();
        }

        return r;
    }

    fn current(&mut self) -> Token {
        self.token_stream.borrow()[self.current].clone()
    }

    fn expression(&mut self) -> Box<AstNode> {
        let literal = self.literal();
        self.number(literal, 0)
    }

    fn number(&mut self, mut left: Box<AstNode>, min_precedence: i64) -> Box<AstNode> {
        let mut start = self.current();

        while precedence(&start) >= min_precedence {
            let op = start;
            self.advance();
            let mut right = self.literal();
            start = self.current();
            while precedence(&start) > precedence(&op) {
                let current = &self.current();
                right = self.number(right, precedence(&op) +
                    if precedence(current) > precedence(&op) { 1 } else { 0 });
                start = self.current();
            }
            left = Box::new(AstNode::Binary(left, op, right));
        }

        left
    }

    fn literal(&mut self) -> Box<AstNode> {
        let start = self.current();
        self.advance();
        Box::new(
            match start.clone().kind {
                TokenType::Identifier(value) => {
                    let id = Box::new(AstNode::Ident(start));
                    match *id.clone() {
                        AstNode::Ident(t) => {
                            if self.expect(TokenType::LParen) {
                                let args: Vec<Box<AstNode>> = Vec::new();
                                if self.expect(TokenType::RParen) {
                                    AstNode::Call(id, args)
                                } else {
                                    error_ast!(id.borrow(), "Function call requires closing parenthesis");
                                    AstNode::None
                                }
                            }
                            else {
                                AstNode::None
                            }
                        },
                        _ => AstNode::None
                    }
                },
                TokenType::Int(value) => AstNode::Int(value),
                TokenType::Float(value) => AstNode::Float(value),
                TokenType::String(value) => AstNode::String(value),
                TokenType::Bool(value) => AstNode::Bool(value),
                TokenType::LParen => {
                    let expr = self.expression();
                    if self.expect(TokenType::RParen) {
                        *expr
                    }
                    else {
                        error!(start, "Grouped expression must have a closing parenthesis");
                        AstNode::None
                    }
                },
                _ => AstNode::None
            }
        )
    }

    fn statement(&mut self) -> Box<AstNode> {
        match self.current().kind {
            TokenType::If => {
                self.advance();

                let cond =  self.expression();
                let body = self.scope();

                return Box::new(AstNode::If(cond, body));
            },
            TokenType::For => {
                self.advance();

                let init = self.variable_decleration();
                let cond = self.expression();
                let incr = self.expression();
                let body = self.scope();
            
                return Box::new(AstNode::For(init, cond, incr, body));
            },
            TokenType::While => {
                self.advance();
                let cond = self.expression();
                let body = self.scope();
            
                return Box::new(AstNode::While(cond, body));
            },
            TokenType::Let | TokenType::Const => {
                let decl = self.variable_decleration();
                if self.expect(TokenType::Semicolon) {
                    return decl;
                }
            }
            _ => {
                let assign = self.variable_assignment();
                match assign.borrow() {
                    AstNode::None => {},
                    _ => return assign
                }

                let expr = self.expression();
                match expr.borrow() {
                    AstNode::None => {},
                    _ => return expr
                }

                return self.variable_assignment();
            }
        }

        return Box::new(AstNode::None);
    }

    fn ident(&mut self) -> Box<AstNode> {
        let start = self.current();
        
        match start.clone().kind {
            TokenType::Identifier(value) => {
                if self.expect(start.clone().kind) {
                    return Box::new(AstNode::Ident(start))
                }
            },
            _ => {}
        }

        Box::new(AstNode::None)
    }

    fn variable_assignment(&mut self) -> Box<AstNode> {
        let id = self.ident();
        if self.expect(TokenType::Equal) {
            let value = self.expression();
            return Box::new(AstNode::Assign(id, value));
        }

        Box::new(AstNode::None)
    }

    fn variable_decleration(&mut self) -> Box<AstNode> {
        let start = self.current();
        if self.expect(TokenType::Let) || self.expect(TokenType::Const) {
            let id = self.ident();
            if self.expect(TokenType::Colon) {
                let kind = self.ident();

                if self.expect(TokenType::Equal) {
                    let value = self.expression();
    
                    let mut seq: Vec<Box<AstNode>> = Vec::new();

                    let vartype: VarType;
                    match start.kind {
                        TokenType::Let => vartype = VarType::Let,
                        TokenType::Const => vartype = VarType::Const,
                        _ => vartype = VarType::Let
                    }

                    seq.push(Box::new(AstNode::VarDecl(vartype, id.clone(), kind.clone())));
                    seq.push(Box::new(AstNode::Assign(id.clone(), value.clone())));

                    return Box::new(AstNode::Sequence(seq));
                }
            }
        }

        Box::new(AstNode::None)
    }
    
    fn sequence(&mut self) -> Box<AstNode> {
        let mut list: Vec<Box<AstNode>> = Vec::new();

        while {
            let stmt = self.statement();
            if *stmt != AstNode::None { list.push(stmt.clone()); }
            *stmt != AstNode::None
        } {}

        Box::new(AstNode::Sequence(list))
    }

    fn scope(&mut self) -> Box<AstNode> {
        let start = self.current();
        if self.expect(TokenType::LBrace) {
            let body = self.sequence();
            
            if self.expect(TokenType::RBrace) {
                return body;
            }
            else {
                error!(start, "Scope requires closing brace");
            }
        }

        Box::new(AstNode::None)
    }

    fn new(token_stream: Vec<Token>) -> Box<AstNode> {
        let mut p = Parser {
            current: 0,
            token_stream: RefCell::new(token_stream)
        };

        p.sequence()
    }
}

fn print_ast(tree: Box<AstNode>, mut tab: u64) {
    macro_rules! println_tab {
        ($tab:expr, $($arg:tt)*) => {
            {
                for _ in 0..$tab {
                    print!("\t");
                }
    
                println!($($arg)*);
            }
        };
    }

    match tree.as_ref() {
        AstNode::None => println_tab!(tab, "Invalid node"),

        AstNode::Ident(value) => println_tab!(tab, "{:?}", value.kind),
        AstNode::Int(value) => println_tab!(tab, "{}", value),
        AstNode::Float(value) => println_tab!(tab, "{}", value),
        AstNode::Bool(value) => println_tab!(tab, "{}", value),
        AstNode::String(value) => println_tab!(tab, "\"{}\"", value),

        AstNode::Sequence(children) => {
            println_tab!(tab, "Sequence");
            tab += 1;
            for node in children {
                print_ast(node.to_owned(), tab);
            }
        },

        AstNode::VarDecl(kind, id, valuekind) => {
            println_tab!(tab, "Decl");
            tab += 1;
            match kind {
                VarType::Const => {
                    println_tab!(tab, "Const");
                },
                VarType::Let => {
                    println_tab!(tab, "Let");
                },
            }
            print_ast(id.to_owned(), tab);
            print_ast(valuekind.to_owned(), tab);
        },
        AstNode::InferredDecl(kind, id) => {
            println_tab!(tab, "Infereed Decl");
            tab += 1;
            match kind {
                VarType::Const => {
                    println_tab!(tab, "Const");
                },
                VarType::Let => {
                    println_tab!(tab, "Let");
                },
            }
            print_ast(id.to_owned(), tab);
        },

        AstNode::Assign(id, value) => {
            println_tab!(tab, "Assign");
            tab += 1;
            print_ast(id.to_owned(), tab);
            print_ast(value.to_owned(), tab);
        },

        AstNode::Binary(left, op, right) => {
            println_tab!(tab, "Binary");
            tab += 1;
            println_tab!(tab, "{:?}", op.kind);
            print_ast(left.to_owned(), tab);
            print_ast(right.to_owned(), tab);
        },

        AstNode::Function(id, args, body) => {
            println_tab!(tab, "Function");
            tab += 1;
            for node in args {
                print_ast(node.to_owned(), tab);
            }
            print_ast(id.to_owned(), tab);
            print_ast(body.to_owned(), tab);
        },
        AstNode::Call(id, args) => {
            println_tab!(tab, "Call");
            tab += 1;

            print_ast(id.to_owned(), tab);
            for node in args {
                print_ast(node.to_owned(), tab);
            }
        },

        AstNode::If(cond, body) => {
            println_tab!(tab, "If");
            tab += 1;
            print_ast(cond.to_owned(), tab);
            print_ast(body.to_owned(), tab);
        },
        AstNode::For(init, cond, incr, body) => {
            println_tab!(tab, "For");
            tab += 1;
            print_ast(init.to_owned(), tab);
            print_ast(cond.to_owned(), tab);
            print_ast(incr.to_owned(), tab);
            print_ast(body.to_owned(), tab);
        },
        AstNode::While(cond, body) => {
            println_tab!(tab, "While");
            tab += 1;
            print_ast(cond.to_owned(), tab);
            print_ast(body.to_owned(), tab);
        }
    }
}

fn emit_c_code(tree: Box<AstNode>) -> String {
    let mut r = "".to_string();

    match tree.as_ref() {
        AstNode::None => println!("Invalid node"),

        AstNode::Ident(value) => {
            if let TokenType::Identifier(value) = value.to_owned().kind {
                r.push_str(value.as_str());
            }
        },
        AstNode::Int(value) => r.push_str(value.to_string().as_str()),
        AstNode::Float(value) => r.push_str(value.to_string().as_str()),
        AstNode::Bool(value) => {
            r.push_str(
                if *value {
                    "true"
                } else {
                    "false"
                }
            );
        },
        AstNode::String(value) => {
            r.push('"');
            r.push_str(value.to_string().as_str());
            r.push('"');
        },

        AstNode::Sequence(children) => {
            for node in children {
                r.push_str(emit_c_code(node.to_owned()).as_str());
                r.push('\n');
            }
        },

        AstNode::VarDecl(kind, id, valuekind) => {
            if let VarType::Const = kind {
                r.push_str("const ");
            }

            r.push_str(emit_c_code(valuekind.to_owned()).as_str());
            r.push(' ');
            r.push_str(emit_c_code(id.to_owned()).as_str());
            r.push(';');
        },
        AstNode::InferredDecl(kind, id) => {

        },

        AstNode::Assign(id, value) => {
            r.push_str(emit_c_code(id.to_owned()).as_str());
            r.push_str("=");
            r.push_str(emit_c_code(value.to_owned()).as_str());
            r.push_str(";");
        },

        AstNode::Binary(left, op, right) => {
            r.push_str(emit_c_code(left.to_owned()).as_str());

            match op.kind {
                TokenType::Plus => r.push_str("+"),
                TokenType::Minus => r.push_str("-"),
                TokenType::Multiply => r.push_str("*"),
                TokenType::Divide => r.push_str("/"),
                _ => {}
            };
            r.push_str(emit_c_code(right.to_owned()).as_str());
        },

        AstNode::Function(id, args, body) => {
            
        },
        AstNode::Call(id, args) => {
            r.push_str(emit_c_code(id.to_owned()).as_str());

            r.push('(');
            for arg in args {
                r.push_str(emit_c_code(arg.to_owned()).as_str());
            }
            r.push(')');
        },

        AstNode::If(cond, body) => {
            r.push_str("if (");
            r.push_str(emit_c_code(cond.to_owned()).as_str());
            r.push_str(")");
            r.push_str(emit_c_code(body.to_owned()).as_str());
        },
        AstNode::For(init, cond, incr, body) => {
            r.push_str("for (");
            r.push_str(emit_c_code(init.to_owned()).as_str());
            r.push_str(";");
            r.push_str(emit_c_code(cond.to_owned()).as_str());
            r.push_str(";");
            r.push_str(emit_c_code(incr.to_owned()).as_str());
            r.push_str(")");
            r.push_str(emit_c_code(body.to_owned()).as_str());
        },
        AstNode::While(cond, body) => {
            r.push_str("while (");
            r.push_str(emit_c_code(cond.to_owned()).as_str());
            r.push_str(")");
            r.push_str(emit_c_code(body.to_owned()).as_str());
        }
    }

    r
}

fn compile_c_code(input_files: String, tree: Box<AstNode>) -> io::Result<()> {
    let code = format!(r"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
typedef char* string;

int main() {{
    {}
    return 0;
}}
", emit_c_code(tree).as_str());

    println!("{}", code);

    std::fs::create_dir_all("build/temp")?;

    let output_name = format!("{}_temp.c", input_files);

    let mut file = File::create(output_name.clone())?;
    file.write_all(code.as_bytes())?;

    let output = Command::new("gcc")
                            .arg(output_name.clone())
                            .args(["-o", format!("{}", output_name.clone()).as_str()])
                            .output();
    
    if !output.as_ref().unwrap().status.success() {
        panic!("C Error: {}", String::from_utf8_lossy(&output.unwrap().stderr));
    }
    
    //std::fs::remove_dir_all("build/temp")?;
    //std::fs::remove_dir_all("build")?;

    Ok(())
}

enum BytecodeInstruction {
    None = 0,

    Push, Pop,
    Store, Load,

    Add, Sub, Multiply, Divide,

    CompEqual, CompNotEqual, CompLess, CompGreater, CompLessEqual, CompGreaterEqual,

    Jump, JumpIfTrue, JumpIfFalse,
    StartSubroutine, JumpSubroutine, ReturnSubroutine
}

fn emit_bytecode(tree: &mut Box<AstNode>) {
    todo!();
}

fn main() -> io::Result<()> {
    let source = read_to_string("test/test.kilt").unwrap();
    
    let token_stream = lexer(&source.as_str());
    
    let ast = Parser::new(token_stream);
    print_ast(ast.clone(), 0);

    compile_c_code(String::from("test/test.kilt"), ast)?;

    Ok(())
}