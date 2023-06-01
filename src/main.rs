use std::{cell::RefCell, ops::Deref};

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
    ($t:expr, $($arg:tt)*) => {
        print!("Error [Line: {}, Loc: {}]", $t.line, $t.location);
        println!($($arg)*);
    };
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

    let mut current = 0 as usize;
    loop {
        if current >= source.len() {
            break;
        }

        let start = current;
        let mut kind = TokenType::None;

        let advance = | current: &mut usize | { if *current < source.len() { *current += 1; } };
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
            '"' => {
                while peek(current) != '"' {
                    advance(&mut current);
                }

                advance(&mut current);
                kind = TokenType::String(source[start+1..current].to_string());
            }
            '\n' => {
                line += 1;
                advance(&mut current);
                continue;
            },
            ' ' | '\t' | '\r' => {
                advance(&mut current);
                continue;
            },
            _ => {
                if current_char(current).is_numeric() {
                    let mut is_float = false;
                    while current_char(current).is_numeric() {
                        advance(&mut current);
                        if current_char(current) == '.' {
                            advance(&mut current);
                            is_float = true;
                        }
                    }

                    kind = if is_float {
                        TokenType::Float(source[start..current].parse::<f64>().unwrap())
                    } else {
                        TokenType::Int(source[start..current].parse::<i64>().unwrap())
                    };
                }
                else if current_char(current).is_alphabetic() {
                    while matches!(current_char(current), 'A'..='Z' | 'a'..='z' | '_') {
                        advance(&mut current);
                    }

                    kind = to_keyword(&source[start..current]);
                }
            }
        }

        let t = Token {
            line: line,
            location: current,
            kind: kind
        };

        token_stream.push(t);
        advance(&mut current);
    }

    token_stream.push(Token {
        line: u64::MAX,
        location: usize::MAX,
        kind: TokenType::Eof
    });

    return token_stream;
}

#[derive(Debug, Clone, PartialEq)]
enum AstNode {
    None,

    Ident(String),
    IntNode(i64),
    FloatNode(f64),
    BoolNode(bool),
    StringNode(String),
    SequenceNode(Vec<Box<AstNode>>),
    LetDecl(Box<AstNode>, Box<AstNode>),
    ConstDecl(Box<AstNode>, Box<AstNode>),
    BinaryNode(Box<AstNode>, Token, Box<AstNode>),
    IfNode(Box<AstNode>, Box<AstNode>),
    FunctionNode(Box<AstNode>, Vec<Box<AstNode>>, Box<AstNode>),
    ForNode(Box<AstNode>, Box<AstNode>, Box<AstNode>, Box<AstNode>),
    WhileNode(Box<AstNode>, Box<AstNode>),
}

struct Parser {
    current: usize,
    token_stream: RefCell<Vec<Token>>
}

fn precedence(token: Token) -> i64 {
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
        Box::new(AstNode::None)
    }

    fn number(&mut self, mut left: Box<AstNode>, min_precedence: i64) -> Box<AstNode> {
        let mut start = self.current().clone();

        while precedence(start) >= min_precedence {
            let op = start;
            self.advance();
            let mut right = self.literal();
            start = self.current();
            while precedence(start) > precedence(op) {
                right = self.number(right, precedence(op) +
                    if precedence(self.current()) > precedence(op) { 1 } else { 0 });
                start = self.current();
            }
            left = Box::new(AstNode::BinaryNode(left, op, right));
        }

        left
    }

    fn literal(&mut self) -> Box<AstNode> {
        let start = self.current();
        Box::new(
            match start.kind {
                TokenType::Identifier(value) => AstNode::Ident(value),
                TokenType::Int(value) => AstNode::IntNode(value),
                TokenType::Float(value) => AstNode::FloatNode(value),
                TokenType::LParen => {
                    let expr = self.expression();
                    if self.expect(TokenType::RParen) {
                        *expr
                    }
                    else {
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
                let cond =  self.expression();
                let body = self.scope();

                return Box::new(AstNode::IfNode(cond, body));
            },
            TokenType::For => {
                let init = self.variable_decleration();
                let cond = self.expression();
                let incr = self.expression();
                let body = self.scope();
            
                return Box::new(AstNode::ForNode(init, cond, incr, body));
            },
            TokenType::While => {
                let cond = self.expression();
                let body = self.scope();
            
                return Box::new(AstNode::WhileNode(cond, body));
            },
            _ => {}
        }

        Box::new(AstNode::None)
    }

    fn ident(&mut self) -> Box<AstNode> {
        let start = self.current();
        
        match start.kind.clone() {
            TokenType::Identifier(value) => {
                if self.expect(start.kind) {
                    return Box::new(AstNode::Ident(value))
                }
            },
            _ => {}
        }

        Box::new(AstNode::None)
    }

    fn variable_decleration(&mut self) -> Box<AstNode> {
        let start = self.current();
        if self.expect(TokenType::Let) || self.expect(TokenType::Const) {
            let id = self.ident();
            if self.expect(TokenType::Equal) {
                let value = self.expression();

                match start.kind {
                    TokenType::Let => return Box::new(AstNode::LetDecl(id, value)),
                    TokenType::Const => return Box::new(AstNode::ConstDecl(id, value)),
                    _ => {}
                }
            }
        }

        Box::new(AstNode::None)
    }
    
    fn sequence(&mut self) -> Box<AstNode> {
        let mut list: Vec<Box<AstNode>> = Vec::new();

        while {
            list.push(self.statement());
            *list.last().unwrap().deref() == AstNode::None
        } {}

        list.pop();

        Box::new(AstNode::SequenceNode(list))
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

        p.scope()
    }
}

fn print_ast(tree: &Box<AstNode>, mut tab: u64) {
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
        AstNode::Ident(value) => println_tab!(tab, "{}", value),
        AstNode::IntNode(value) => println_tab!(tab, "{}", value),
        AstNode::FloatNode(value) => println_tab!(tab, "{}", value),
        AstNode::BoolNode(value) => println_tab!(tab, "{}", value),
        AstNode::StringNode(value) => println_tab!(tab, "{}", value),
        AstNode::SequenceNode(children) => {
            println_tab!(tab, "Sequence");
            tab += 1;
            for node in children {
                print_ast(node, tab);
            }
        },

        AstNode::LetDecl(id, value) => {
            println_tab!(tab, "Let");
            tab += 1;
            print_ast(id, tab);
            print_ast(value, tab);
        },
        AstNode::ConstDecl(id, value) => {
            println_tab!(tab, "Const");
            tab += 1;
            print_ast(id, tab);
            print_ast(value, tab);
        },
        AstNode::BinaryNode(left, op, right) => {
            println_tab!(tab, "Binary");
            tab += 1;
            print_ast(left, tab);
            println_tab!(tab, "{:?}", op.kind);
            print_ast(right, tab);
        },
        AstNode::IfNode(cond, body) => {
            println_tab!(tab, "If");
            tab += 1;
            print_ast(cond, tab);
            print_ast(body, tab);
        },
        AstNode::FunctionNode(id, args, body) => {
            println_tab!(tab, "Function");
            tab += 1;
            for node in args {
                print_ast(node, tab);
            }
            print_ast(id, tab);
            print_ast(body, tab);
        },
        AstNode::ForNode(init, cond, incr, body) => {
            println_tab!(tab, "For");
            tab += 1;
            print_ast(init, tab);
            print_ast(cond, tab);
            print_ast(incr, tab);
            print_ast(body, tab);
        },
        _ => println_tab!(tab, "Invalid Node Types")
    }
}

fn main() {
    let token_stream = lexer(r#"
    if true {

    }

    "#);
    
    let ast = Parser::new(token_stream);
    print_ast(&ast, 0);
}