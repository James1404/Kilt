#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <fstream>
#include <sstream>
#include <filesystem>
#include <optional>
#include <variant>

/* --- Language Grammar ---
<statement> ::= "if" <condition> <statement-block> ("else" <statement-block>)?
			  | "loop" <statement-block>
			  | "for" (<variable-decleration> | <expression>) ',' <condition> ',' <expression> <statement-block>
			  | "func" <identifier> "(" <argument-list-decleration> ")" (":" <identifier)? <statement-block>?
			  | "return" <expression> ';'
              | "break" ';'
              | "continue" ';'
			  | <expression> ';'
              | <variable-decleration> ';'
			  | <statement-block>

<statement-list> ::= <statement>*
<statement-block> :: "{" <statement-list> "}"

<variable-decleration> ::= <identifier> <identifier> ("=" <expression>)?

<argument-list-decleration> ::= <variable-decleration> (',' <variable-decleration>)*
<argument-list> ::= <expression> (',' <expression>)*

<condition> ::= <expression> ("==" | "!=" | ">=" | "<=" | ">" | "<" |) <expression>
<expression> ::= string | integer | float | identifier

<function-call> ::= <identifier> '(' <argument-list> ')'
*/

bool error = false;
void LogError(std::string msg)
{
	std::cout << "Error: " << msg << '\n';
}

void LogError(std::string msg, int line, int location)
{
	std::cout << "Error: [Line: " << line << ", Location: " << location << "] " << msg << '\n';
}

template<typename T>
class Stack
{
private:
	std::vector<T> data;
public:
	bool empty() const { return data.empty(); }
	size_t size() const { return data.size(); }

	void push(T t) { data.push_back(t); }
	void pop() { data.pop_back(); }
	T& top() { return data.back(); }

	T& at(size_t depth) { return data.at(size() - depth); }
};

bool IsLetter(char c)
{
	return (c >= 'a' && c <= 'z') ||
		   (c >= 'A' && c <= 'Z');
}

bool IsNumber(char c)
{
	return c >= '0' && c <= '9';
}

enum TokenType
{
	TOKEN_NONE,

	TOKEN_IDENTIFIER,
	TOKEN_LITERAL,

	TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
	TOKEN_LEFT_BRACKET, TOKEN_RIGHT_BRACKET,
	TOKEN_LEFT_PARENTHESIS, TOKEN_RIGHT_PARENTHESIS,
	TOKEN_SEMICOLON, TOKEN_DOT, TOKEN_COMMA, TOKEN_COLON,

	TOKEN_IF, TOKEN_ELSE, TOKEN_FUNC, TOKEN_FOR, TOKEN_LOOP,
	TOKEN_RETURN, TOKEN_BREAK, TOKEN_CONTINUE,

    TOKEN_TRUE, TOKEN_FALSE,

	TOKEN_PLUS, TOKEN_MINUS, TOKEN_DIVIDE, TOKEN_MULTIPLY,

	TOKEN_EQUAL, TOKEN_NOT,

	TOKEN_EQUAL_EQUAL, TOKEN_NOT_EQUAL,
	TOKEN_LESS, TOKEN_GREATER, TOKEN_LESS_EQUAL, TOKEN_GREATER_EQUAL
};

struct Token
{
	TokenType type;
	std::string text;

	int location = 0,
		size = 0,
		line = 0;

    Token() = default;
    Token(const Token& other) = default;
    Token(TokenType type_) : type(type_) {}
};

Token CreateTokenFromString(std::string v) { Token t; t.text = v; return t; }

std::map<std::string, TokenType> keywords = {
	{ "if", TOKEN_IF },
	{ "else", TOKEN_ELSE },
	{ "func", TOKEN_FUNC },
	{ "for", TOKEN_FOR },
	{ "loop", TOKEN_LOOP },
	{ "return", TOKEN_RETURN },
	{ "break", TOKEN_BREAK },
	{ "continue", TOKEN_CONTINUE },

    { "true", TOKEN_TRUE },
    { "false", TOKEN_FALSE },
};

enum ValueType
{
    VALUE_NONE,

    VALUE_INT, VALUE_REAL, VALUE_STR, VALUE_BOOL,
    VALUE_ID
} type;

class Value
{
private:
	ValueType type;
    std::variant<
        int64_t,double,bool,std::string
    > values;

    void ParseStr(Token token)
    {
        values = "";

        char b = token.text.front(), e = token.text.back();
        if(b == '"' && e == '"')
        {
            std::string r = token.text;
            r.erase(0,1);
            r.erase(r.size(),1);

            values = r;
            type = VALUE_STR;
        }
    }

    void ParseInt(Token token)
    {
        for(int i = 0; i < token.text.size(); i++)
        {
            char c = token.text.at(i);
            if(!IsNumber(c))
            {
                return;
            }
        }

        type = VALUE_INT;
        values = std::stoll(token.text);
    }

    void ParseReal(Token token)
    {
        bool dot_used = false;
        for(int i = 0; i < token.text.size(); i++)
        {
            char c = token.text.at(i);
            if(c == '.')
            {
                if(dot_used)
                {
                    LogError("Real number cannot contain two decimal points", token.line, token.location);
                    return;
                }

                dot_used = true;
                continue;
            }

            if(!IsNumber(c))
            {
                return;
            }
        }

        type = VALUE_REAL;
        values = std::stod(token.text);
    }

    void ParseBool(Token token)
    {
        if(token.type == TOKEN_TRUE)
        {
            type = VALUE_BOOL;
            values = true;
        }
        else if(token.type == TOKEN_FALSE)
        {
            type = VALUE_BOOL;
            values = false;
        }
    }
    
    void ParseIdentifier(Token token)
    {
        values = "";

        char b = token.text.front(), e = token.text.back();
        if(b == '"' && e == '"')
        {
            return;
        }

        values = token.text;
        type = VALUE_ID;
    }
public:
    bool IsInt() const { return type == VALUE_INT; }
    bool IsReal() const { return type == VALUE_REAL; }
    bool IsStr() const { return type == VALUE_STR; }
    bool IsBool() const { return type == VALUE_BOOL; }
    bool IsId() const { return type == VALUE_ID; }

    ValueType GetType() const { return type; }

    int64_t& GetInt() { return std::get<int64_t>(values); }
    double& GetReal() { return std::get<double>(values); }
    std::string& GetStr() { return std::get<std::string>(values); }
    bool& GetBool() { return std::get<bool>(values); }
    std::string& GetId() { return std::get<std::string>(values); }

    void Negative()
    {
        if(IsInt())
        {
            GetInt() = -GetInt();
        }
        else if(IsReal())
        {
            GetReal() = -GetReal();
        }
        else
        {
            LogError("Cannot negetorize type '" + GetTypeAsString() + "'");
        }
    }

    Value() = default;
	Value& operator=(Value& other) = default;

    Value(Token& other)
    {
        ParseStr(other);
        ParseIdentifier(other);
        ParseReal(other);
        ParseInt(other);
        ParseBool(other);
    }

	Value(int64_t other)
	{
		type = VALUE_INT;
		values = other;
	}

	Value(double other)
	{
		type = VALUE_REAL;
		values = other;
	}

	Value(std::string other)
	{
        ParseStr(CreateTokenFromString(other));
        ParseIdentifier(CreateTokenFromString(other));
	}

	Value(bool other)
	{
		type = VALUE_BOOL;
		values = other;
	}

    Value& operator=(Token& other)
    {
        ParseStr(other);
        ParseIdentifier(other);
        ParseReal(other);
        ParseInt(other);
        ParseBool(other);
        return *this;
    }

	Value& operator=(int64_t other)
	{
		type = VALUE_INT;
		values = other;
        return *this;
	}

	Value& operator=(double other)
	{
		type = VALUE_REAL;
		values = other;
        return *this;
	}

	Value& operator=(const char* other)
	{
		type = VALUE_STR;
		values = other;
        return *this;
	}

	Value& operator=(std::string other)
	{
        ParseStr(CreateTokenFromString(other));
        ParseIdentifier(CreateTokenFromString(other));
        return *this;
	}
private:
    Token linked;

    friend bool CompareValueTypes(Value l, Value r);
public:
    void LinkToken(const Token t)
    {
        linked = t;
    }

    int GetLoc() const { return linked.location; }
    int GetLine() const { return linked.line; }

    std::string GetTypeAsString() const
    {
        if(IsInt()) return "int";
        if(IsReal()) return "float";
        if(IsStr()) return "string";
        if(IsBool()) return "bool";
        if(IsId()) return "identifier";
        return "";
    }
};

bool CompareValueTypes(Value l, Value r)
{
    return l.type == r.type;
}

Value StringToValue(std::string type)
{
    if(type == "string") return Value("");
    if(type == "int") return Value((int64_t)0);
    if(type == "float") return Value(0.0);
    if(type == "bool") return Value(true);

    return Value();
}

struct SymbolTable
{
	std::map<std::string, std::string> typetable;

	void SetType(std::string name, std::string type)
	{
		typetable[name] = type;
	}

	std::string FindType(std::string name)
	{
		auto it = typetable.find(name);
		if (it != typetable.end()) return it->second;

		return "";
	}
};

static SymbolTable table;

TokenType GetKeyword(std::string text)
{
	auto it = keywords.find(text);
	if (it != keywords.end()) return it->second;
	return TOKEN_IDENTIFIER;
}

using TokenStream = std::vector<Token>;
struct Lexer
{
	TokenStream stream;
	std::string source;

	int location = 0;

	void Advance()
	{
		if (location + 1 > source.size())
		{
			return;
		}

		location++;
	}

	char Peek()
	{
		if (location + 1 > source.size())
		{
			return ' ';
		}

		return source.at(location + 1);
	}

	bool Match(char c)
	{
		if (Peek() == c)
		{
			Advance();
			return true;
		}

		return false;
	}

	char GetCurrent()
	{
		return source.at(location);
	}

	Lexer(std::string source_)
		: source(source_)
	{
		int line = 0;
		for (location = 0; location < source.size(); location++)
		{
			char c = source.at(location);

			Token token;
			token.location = location;
			token.size = 1;
			token.line = 0;

			token.text = source.substr(token.location, token.size);

			switch (c)
			{
			case '(': { token.type = TOKEN_LEFT_PARENTHESIS; } break;
			case ')': { token.type = TOKEN_RIGHT_PARENTHESIS; } break;
			case '{': { token.type = TOKEN_LEFT_BRACE; } break;
			case '}': { token.type = TOKEN_RIGHT_BRACE; } break;
			case '[': { token.type = TOKEN_LEFT_BRACKET; } break;
			case ']': { token.type = TOKEN_RIGHT_BRACKET; } break;

			case ';': { token.type = TOKEN_SEMICOLON; } break;
			case ':': { token.type = TOKEN_COLON; } break;
			case '.': { token.type = TOKEN_DOT; } break;
			case ',': { token.type = TOKEN_COMMA; } break;

			case '+': { token.type = TOKEN_PLUS; } break;
			case '-': { token.type = TOKEN_MINUS; } break;
			case '*': { token.type = TOKEN_MULTIPLY; } break;
			case '/':
			{
				// TODO: Implement multiline nested comments
				if (Peek() == '/')
				{
					while (Peek() != '\n')
					{
						Advance();
					}

					line++;
					continue;
				}
				else
				{
					token.type = TOKEN_DIVIDE;
				}
			} break;

			case '=':
			{
				token.type = Match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL;
			} break;
			case '!':
			{
				token.type = Match('=') ? TOKEN_NOT_EQUAL : TOKEN_NOT_EQUAL;
			} break;
			case '<':
			{
				token.type = Match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS;
			} break;
			case '>':
			{
				token.type = Match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER;
			} break;

            case '"':
            {
                do
                {
                    token.size++;
                    Advance();
                }
                while(GetCurrent() != '"');

                token.text = source.substr(token.location, token.size);
                token.type = TOKEN_LITERAL;
            } break;

			default:
			{
				if (c == ' ' ||
					c == '\r' ||
					c == '\t')
				{
					continue;
				}

				if (c == '\n')
				{
					line++;
					continue;
				}

				if (IsNumber(c))
				{
					while (IsNumber(Peek()) || Peek() == '.')
					{
						token.size++;
						Advance();
					}

					token.text = source.substr(token.location, token.size);
					token.type = TOKEN_LITERAL;
				}
				else if (IsLetter(c))
				{
					while (IsLetter(Peek()) || IsNumber(Peek()) || Peek() == '_')
					{
						token.size++;
						Advance();
					}

					token.text = source.substr(token.location, token.size);
					token.type = GetKeyword(token.text);
				}
				else
				{
					continue;
				}
			} break;
			}

			stream.push_back(token);
		}
	}
};

struct Node;
struct SequenceNode;
struct BinaryNode;
struct ValueNode;
struct VariableDeclNode;
struct AssignmentNode;
struct FunctionNode;
struct CallNode;
struct IfNode;
struct ForNode;
struct LoopNode;
struct ReturnNode;
struct BreakNode;
struct ContinueNode;

struct NodeVisitor
{
	virtual void visit(SequenceNode* node) = 0;
	virtual void visit(BinaryNode* node) = 0;
	virtual void visit(ValueNode* node) = 0;
	virtual void visit(VariableDeclNode* node) = 0;
	virtual void visit(AssignmentNode* node) = 0;
	virtual void visit(FunctionNode* node) = 0;
	virtual void visit(CallNode* node) = 0;
	virtual void visit(IfNode* node) = 0;
	virtual void visit(ForNode* node) = 0;
	virtual void visit(LoopNode* node) = 0;
	virtual void visit(ReturnNode* node) = 0;
	virtual void visit(BreakNode* node) = 0;
	virtual void visit(ContinueNode* node) = 0;
};

#define SHOW_FREED_MEMORY false
struct Node
{
	virtual void visit(NodeVisitor* visitor) = 0;

    ~Node()
	{
#if SHOW_FREED_MEMORY
		std::cout << "Freed memory " << this << '\n';
#endif
	}
};

#define VISIT_ virtual void visit(NodeVisitor* v) { v->visit(this); } 

struct SequenceNode : Node
{
	std::vector<Node*> lst;

	SequenceNode(std::vector<Node*> lst_)
		: lst(lst_)
	{}

	VISIT_
};

struct BinaryNode : Node
{
	Node* left, * right;
	Token op;

	BinaryNode(Node* left_, Token op_, Node* right_)
		: left(left_), right(right_), op(op_)
	{}

	VISIT_
};

struct ValueNode : Node
{
    Value value;

	ValueNode(Token token_, bool negative = false)
    {
        value = token_;
        if(negative) value.Negative();
    }

	VISIT_
};

struct VariableDeclNode : Node
{
	Token id, type;

	VariableDeclNode(Token id_, Token type_)
		: id(id_), type(type_)
	{}
    
	VISIT_
};

struct AssignmentNode : Node
{
	Token id;
    Node* value;

	AssignmentNode(Token id_, Node* value_)
		: id(id_), value(value_)
	{}

	VISIT_
};

struct FunctionNode : Node
{
	Token id, returntype;
	Node* block;
    std::vector<Node*> args;

	FunctionNode(Token id_, Token returntype_, std::vector<Node*> args_, Node* block_)
		: id(id_), returntype(returntype_), args(args_), block(block_)
	{}

	VISIT_
};

struct CallNode : Node
{
	Token id;
    std::vector<Node*> args;

	CallNode(Token id_, std::vector<Node*> args_)
		: id(id_), args(args_)
	{}
	VISIT_
};

struct IfNode : Node
{
	Node* cond, * block, * elseblock;

	IfNode(Node* cond_, Node* block_, Node* elseblock_)
		: cond(cond_), block(block_), elseblock(elseblock_)
	{}

	VISIT_
};

struct ForNode : Node
{
	Node* init, * cond, * incr, * block;

	ForNode(Node* init_, Node* cond_, Node* incr_, Node* block_)
		: init(init_), cond(cond_), incr(incr_), block(block_)
	{}

	VISIT_
};

struct LoopNode : Node
{
	Node* block;

	LoopNode(Node* block_)
		: block(block_)
	{}
	
	VISIT_
};

struct ReturnNode : Node
{
    Node* expr;

    ReturnNode(Node* expr_)
        : expr(expr_)
    {}

    VISIT_
};

struct BreakNode : Node
{
    Token id;

    BreakNode(Token id_)
        : id(id_)
    {}

    VISIT_
};

struct ContinueNode : Node
{
    Token id;

    ContinueNode(Token id_)
        : id(id_)
    {}

    VISIT_
};

std::map<TokenType, int> operatorprecendence =
{
    { TOKEN_PLUS,0 },
    { TOKEN_MINUS,1 },
    { TOKEN_MULTIPLY,2 },
    { TOKEN_DIVIDE,3 }
};

int GetPrecedence(Token t)
{
    if(t.type < TOKEN_PLUS || t.type > TOKEN_MULTIPLY) return -1;
    return operatorprecendence.at(t.type);
}

struct Parser
{
	TokenStream stream;
	int location = 0;

	Token current;

	Node* tree = NULL;

	void Advance(int n = 1)
	{
		if (location < stream.size() - n)
		{
            location += n;
            current = stream.at(location);
		}
	}

    Token Peek()
    {
        if(location < stream.size() - 1)
            return stream.at(location + 1);

        return Token(TOKEN_NONE);
    }

    bool AdvanceIfMatch(TokenType match)
    {
        if(current.type == match)
        {
            Advance();
            return true;
        }

        return false;
    }

	Parser(TokenStream input)
		: stream(input)
	{
        current = stream.at(0);
		tree = StatementList();
	}

	Node* StatementList()
	{
        std::vector<Node*> sequence;

        Node* arg = NULL;
        do
        {
            arg = Statement();
            if(arg != NULL) sequence.push_back(arg);
        }
        while(arg != NULL);

		return new SequenceNode(sequence);
	}

	Node* StatementBlock()
	{
		if (AdvanceIfMatch(TOKEN_LEFT_BRACE))
		{
			Node* lst = StatementList();

			if (AdvanceIfMatch(TOKEN_RIGHT_BRACE))
			{
				return lst;
			}
			else
			{
				LogError("Statement block must end with a closing brace", current.line, current.location);
			}
		}

		return NULL;
	}

	Node* Statement()
	{
        if (AdvanceIfMatch(TOKEN_IF))
		{
			Node* cond = Condition();
			Node* block = StatementBlock();
			Node* elseblock = NULL;

			if (current.type == TOKEN_ELSE)
			{
				elseblock = StatementBlock();
			}

			return new IfNode(cond, block, elseblock);
		}
        else if(AdvanceIfMatch(TOKEN_LOOP))
        {
            Node* block = StatementBlock();
            return new LoopNode(block);
        }
        else if(AdvanceIfMatch(TOKEN_FOR))
        {
            if(AdvanceIfMatch(TOKEN_LEFT_PARENTHESIS))
            {
                Node* init = VariableDecleration();
                if(init == NULL) init = Expression();

                if(AdvanceIfMatch(TOKEN_COMMA))
                {
                    Node* cond = Condition();
                    if(AdvanceIfMatch(TOKEN_COMMA))
                    {
                        Node* incr = Expression();

                        if(AdvanceIfMatch(TOKEN_RIGHT_PARENTHESIS))
                        {
                            Node* block = StatementBlock();

                            return new ForNode(init, cond, incr, block);
                        }
                    }
                }
            }
        }
		else if (AdvanceIfMatch(TOKEN_FUNC))
		{
			Token id = current;
			Advance();

			if (AdvanceIfMatch(TOKEN_LEFT_PARENTHESIS))
			{
				auto args_decl = ArgumentListDecleration();

				if (AdvanceIfMatch(TOKEN_RIGHT_PARENTHESIS))
				{
					Token returntype;
					if (AdvanceIfMatch(TOKEN_COLON))
					{
						returntype = current;
                        Advance();
					}

					Node* block = StatementBlock();

					return new FunctionNode(id, returntype, args_decl, block);
				}
			}
		}
        else if(AdvanceIfMatch(TOKEN_RETURN))
        {
            Node* expr = Expression();
            if(AdvanceIfMatch(TOKEN_SEMICOLON))
            {
                return new ReturnNode(expr);
            }
            else
            {
                LogError("Return must end with a semicolon", current.line, current.location);
            }
        }
        else if(AdvanceIfMatch(TOKEN_BREAK))
        {
            if(AdvanceIfMatch(TOKEN_SEMICOLON))
            {
                return new BreakNode(current);
            }
            else
            {
                LogError("Break must end with a semicolon", current.line, current.location);
            }
        }
        else if(AdvanceIfMatch(TOKEN_CONTINUE))
        {
            if(AdvanceIfMatch(TOKEN_SEMICOLON))
            {
                return new ContinueNode(current);
            }
            else
            {
                LogError("Break must end with a semicolon", current.line, current.location);
            }
        }

        if(Node* n = VariableDecleration(); n != NULL)
        {
            if(AdvanceIfMatch(TOKEN_SEMICOLON))
            {
                return n;
            }
        }

        if(Node* n = VariableAssignment(); n != NULL)
        {
            if(AdvanceIfMatch(TOKEN_SEMICOLON))
            {
                return n;
            }
        }

        if(Node* n = Expression(); n != NULL)
        {
            if(AdvanceIfMatch(TOKEN_SEMICOLON))
            {
                return n;
            }
        }


		return StatementBlock();
	}

    Node* VariableDecleration()
    {
        
        if(Token id = current; id.type == TOKEN_IDENTIFIER)
        {
            if(Peek().type == TOKEN_COLON)
            {
                Advance(2);

                if(current.type == TOKEN_IDENTIFIER)
                {
                    Token type = current;
                    Advance();

                    if(AdvanceIfMatch(TOKEN_EQUAL))
                    {
                        if(Node* value = Expression(); value != NULL)
                        {
                            Node* decl = new VariableDeclNode(id, type);
                            Node* assgn = new AssignmentNode(id, value);
                            return new SequenceNode({decl,assgn});
                        }
                    }
                    else
                    {
                        return new VariableDeclNode(id, type);
                    }
                }
            }
        }

        return NULL;
    }

    Node* VariableAssignment()
    {
        if(Token id = current; id.type == TOKEN_IDENTIFIER)
        {
            if(Peek().type == TOKEN_EQUAL)
            {
                Advance(2);
                return new AssignmentNode(id, Expression());
            }
        }

        return NULL;
    }

    std::vector<Node*> ArgumentListDecleration()
	{
        std::vector<Node*> sequence;

        Node* arg = NULL;
        do
        {
            arg = VariableDecleration();
            if(arg != NULL) sequence.push_back(arg);
        }
        while(arg != NULL);

		return sequence;
	}

	std::vector<Node*> ArgumentList()
	{
        std::vector<Node*> sequence;

        Node* arg = NULL;
        do
        {
            arg = Expression();
            if(arg != NULL) sequence.push_back(arg);
        }
        while(arg != NULL);

		return sequence;
	}

	Node* Condition()
	{
		// TODO: Implement TokenNot '!'
		Node* l = Expression();
		Token op = current;

		if(op.type == TOKEN_EQUAL_EQUAL ||
		   op.type == TOKEN_NOT_EQUAL ||
		   op.type == TOKEN_LESS ||
		   op.type == TOKEN_LESS_EQUAL ||
		   op.type == TOKEN_GREATER ||
		   op.type == TOKEN_GREATER_EQUAL)
		{
			Advance();
			Node* r = Expression();
			if (r != NULL)
			{
				return new BinaryNode(l, op, r);
			}

			LogError("The right hand side of the comparison is an invalid expression", current.line, current.location);
		}
		else
		{
			LogError("Invalid operator for a comparison", op.line, op.location);
		}

		// TODO: Implement && and ||
		return NULL;
	}

	Node* Expression()
	{
		return Number(Literal(), 0);
	}

    Node* FunctionCall()
    {
        if(current.type == TOKEN_IDENTIFIER)
        {
            if(Peek().type == TOKEN_LEFT_PARENTHESIS)
            {
                Token id = current;
                Advance(2);

                auto arg_list = ArgumentList();

                if(current.type == TOKEN_RIGHT_PARENTHESIS)
                {
                    Advance();
                    return new CallNode(id, arg_list);
                }
                else
                {
                    LogError("Function call args must be closed with a right-parenthesis", id.line, id.location);
                }
            }
        }

        return NULL;
    }

    Node* Number(Node* lhs, int min_precedence)
    {
        Token t = current;
        while(GetPrecedence(current) >= min_precedence)
        {
            Token op = t;
            Advance();
            Node* rhs = Literal();
            t = current;
            while(GetPrecedence(t) > GetPrecedence(op))
            {
                rhs = Number(rhs, GetPrecedence(op) + (GetPrecedence(current) > GetPrecedence(op) ? 1 : 0));
                t = current;
            }
            lhs = new BinaryNode(lhs, op, rhs);
        }

        return lhs;
    }

	Node* Literal()
	{
        Token t = current;
        if(AdvanceIfMatch(TOKEN_LITERAL))
        {
            return new ValueNode(t);
        }
        else if(t.type == TOKEN_MINUS)
        {
            t = Peek();
            if(t.type == TOKEN_LITERAL)
            {
                Advance(2);
                return new ValueNode(t, true);
            }
        }
        else if(AdvanceIfMatch(TOKEN_LEFT_PARENTHESIS))
        {
            Node* expr = Expression();
            if(AdvanceIfMatch(TOKEN_RIGHT_PARENTHESIS))
            {
                return expr;
            }
        }
        else if(AdvanceIfMatch(TOKEN_LITERAL))
        {
            return new ValueNode(t);
        }
        else if(t.type == TOKEN_IDENTIFIER)
        {
            Node* func_call = FunctionCall();
            if(func_call != NULL) return func_call;

            Advance();
            return new ValueNode(t);
        }
        else if(AdvanceIfMatch(TOKEN_TRUE) ||
                AdvanceIfMatch(TOKEN_FALSE))
        {
            return new ValueNode(t);
        }
        // TODO: Add array literal

        return NULL;
	}
};

struct TypeChecker : NodeVisitor
{
    Stack<Value> stack;

	void visit(SequenceNode* node)
	{
		for (auto&& n : node->lst)
		{
			n->visit(this);
		}
	}

	void visit(BinaryNode* node)
	{
		node->left->visit(this);
		node->right->visit(this);

        Value r = stack.top(); stack.pop();
        Value l = stack.top(); stack.pop();

        if(!CompareValueTypes(l, r))
        {
            LogError("Type's invalid", l.GetLine(), l.GetLoc());
        }

        stack.push(l);
	}

	void visit(ValueNode* node)
    {
        stack.push(node->value);
    }

	void visit(VariableDeclNode* node)
	{
        table.SetType(node->id.text, node->type.text);
	}

	void visit(AssignmentNode* node)
	{
        node->value->visit(this);
        Value var = StringToValue(table.FindType(node->id.text));
        Value type = stack.top(); stack.pop();
        if(!CompareValueTypes(var, type))
        {
            LogError("Cannot assign '" + type.GetTypeAsString() + "' to '" + var.GetTypeAsString() + "'", node->id.line, node->id.location);
        }
	}

	void visit(FunctionNode* node)
	{

	}

	void visit(CallNode* node)
	{

	}

	void visit(IfNode* node)
	{
		node->cond->visit(this);
		node->block->visit(this);
		if (node->elseblock != NULL) node->elseblock->visit(this);
	}

	void visit(ForNode* node)
	{
		if (node->init != NULL) node->init->visit(this);
		if (node->cond != NULL) node->cond->visit(this);
		if (node->incr != NULL) node->incr->visit(this);
		node->block->visit(this);
	}

	void visit(LoopNode* node)
	{
		node->block->visit(this);
	}

	void visit(ReturnNode* node)
    {
        node->expr->visit(this);
    }

	void visit(BreakNode* node)
    {
    }

	void visit(ContinueNode* node)
    {
    }
};

struct NodeFreeVisitor : NodeVisitor
{
	void visit(SequenceNode* node)
	{
		for (auto&& n : node->lst)
		{
			n->visit(this);
			delete n;
		}

		node->lst.clear();
	}

	void visit(BinaryNode* node)
	{
		node->left->visit(this);
		delete node->left;
		node->right->visit(this);
		delete node->right;
	}

	void visit(ValueNode* node) {}
	void visit(VariableDeclNode* node) {}

	void visit(AssignmentNode* node)
	{
        node->value->visit(this);
        delete node->value;
	}

	void visit(FunctionNode* node)
	{
		for (auto&& n : node->args)
		{
			n->visit(this);
			delete n;
		}

		node->args.clear();

        node->block->visit(this);
        delete node->block;
	}

	void visit(CallNode* node)
	{
		for (auto&& n : node->args)
		{
			n->visit(this);
			delete n;
		}

		node->args.clear();
	}

	void visit(IfNode* node)
	{
		node->cond->visit(this); delete node->cond;
		node->block->visit(this); delete node->block;
		if (node->elseblock != NULL)
		{
			node->elseblock->visit(this);
			delete node->elseblock;
		}
	}

	void visit(ForNode* node)
	{
		if (node->init != NULL) { node->init->visit(this); delete node->init; }
		if (node->cond != NULL) { node->cond->visit(this); delete node->cond; }
		if (node->incr != NULL) { node->incr->visit(this); delete node->incr; }
		node->block->visit(this);
		delete node->block;
	}

	void visit(LoopNode* node)
	{
		node->block->visit(this);
		delete node->block;
	}

	void visit(ReturnNode* node)
    {
        if(node->expr != NULL) delete node->expr;
    }

	void visit(BreakNode* node) {}
	void visit(ContinueNode* node) {}
};

enum InstructionType : std::uint8_t
{
	INSTRUCTION_NONE,

	INSTRUCTION_PUSH, INSTRUCTION_POP,
	INSTRUCTION_STORE, INSTRUCTION_LOAD,

	INSTRUCTION_JUMP, INSTRUCTION_JUMP_IF_TRUE, INSTRUCTION_JUMP_IF_FALSE,
    INSTRUCTION_START_SUBROUTINE, INSTRUCTION_JUMP_SUBROUTINE, INSTRUCTION_RETURN_FROM_SUBROUTINE,

	INSTRUCTION_ADD, INSTRUCTION_MINUS, INSTRUCTION_MULTIPLY, INSTRUCTION_DIVIDE,

    INSTRUCTION_COMP_LESS, INSTRUCTION_COMP_GREATER,
    INSTRUCTION_COMP_LESS_EQUAL, INSTRUCTION_COMP_GREATER_EQUAL,
    INSTRUCTION_COMP_EQUAL, INSTRUCTION_COMP_NOT_EQUAL,
};

struct Instruction
{
	InstructionType type;
	Value value;

    Instruction(InstructionType type_, Value value_ = {})
        : type(type_), value(value_)
    {}
};

void PrintInstruction(Instruction ins)
{

    std::cout << '\n';
}

namespace std
{
    std::string to_string(Value other)
    {
        switch (other.GetType())
        {
            case VALUE_INT: { return std::to_string(other.GetInt()); } break;
            case VALUE_REAL: { return std::to_string(other.GetReal()); } break;
            case VALUE_STR: { return other.GetStr(); } break;
            case VALUE_BOOL: { return other.GetBool() ? "true" : "false"; } break;
            case VALUE_ID: { return other.GetId(); } break;
        }
        return "";
    }

    std::string to_string(Token other)
    {
	    return std::to_string(other.type) + ": " + "\"" + other.text + "\"" + '\n';
    }

    std::string to_string(Instruction other)
    {
        switch(other.type)
        {
            case INSTRUCTION_PUSH: { return "PUSH " + std::to_string(other.value); } break;
            case INSTRUCTION_POP: { return "POP"; } break;
            case INSTRUCTION_STORE: { return "STORE " + std::to_string(other.value); } break;
            case INSTRUCTION_LOAD: { return "LOAD " + std::to_string(other.value); } break;
            case INSTRUCTION_JUMP: { return "JUMP " + std::to_string(other.value); } break;
            case INSTRUCTION_JUMP_IF_TRUE: { return "JUMP_IF_TRUE " + std::to_string(other.value); } break;
            case INSTRUCTION_JUMP_IF_FALSE: { return "JUMP_IF_FALSE " + std::to_string(other.value); } break;

            case INSTRUCTION_START_SUBROUTINE: { return "START_SUBROUTINE " + std::to_string(other.value); } break;
            case INSTRUCTION_JUMP_SUBROUTINE: { return "JUMP_SUBROUTINE " + std::to_string(other.value); } break;
            case INSTRUCTION_RETURN_FROM_SUBROUTINE: { return "RETURN_FROM_SUBROUTINE " + std::to_string(other.value); } break;

            case INSTRUCTION_ADD: { return "ADD"; } break;
            case INSTRUCTION_MINUS: { return "MINUS"; } break;
            case INSTRUCTION_MULTIPLY: { return "MULTIPLY"; } break;
            case INSTRUCTION_DIVIDE: { return "DIVIDE"; } break;

            case INSTRUCTION_COMP_LESS: { return "COMP_LESS"; } break;
            case INSTRUCTION_COMP_GREATER: { return "COMP_GREATER"; } break;
            case INSTRUCTION_COMP_LESS_EQUAL: { return "COMP_LESS_EQUAL"; } break;
            case INSTRUCTION_COMP_GREATER_EQUAL: { return "COMP_GREATER_EQUAL"; } break;
            case INSTRUCTION_COMP_EQUAL: { return "COMP_EQUAL"; } break;
            case INSTRUCTION_COMP_NOT_EQUAL: { return "COMP_NOT_EQUAL"; } break;
        }

        return "INVALID INSTRUCTION";
    }
}

struct ScopedEnvironment
{
	ScopedEnvironment* parent;

	std::map<std::string, Value> identifiers;

	void Delete()
	{
		if (parent != NULL)
		{
			parent->Delete();
			delete parent;
		}
	}

	void SetIdentifier(std::string name, Value value)
	{
		identifiers.emplace(name, value);
	}

	std::optional<Value> FindIdentifier(std::string name)
	{
		auto it = identifiers.find(name);

		if (it != identifiers.end())
		{
			return it->second;
		}

		if (parent != NULL) return parent->FindIdentifier(name);

		return {};
	}
};

using Program = std::vector<Instruction>;
struct VirtualMachine
{
    using Loc = std::uint64_t;

	Stack<Value> stack;
    Stack<Loc> returnStack;
    std::map<std::string,Loc> jumpTable;

	ScopedEnvironment global, *current;

	VirtualMachine()
	{
		current = &global;
	}

	~VirtualMachine()
	{
		current->Delete();
		delete current;
	}

	void NewCurrent()
	{
		if (current == NULL) return;

		ScopedEnvironment* temp = current;

		current = new ScopedEnvironment();
		current->parent = temp;
	}

	void DeleteCurrent()
	{
		if (current->parent == NULL) return;

		current = current->parent;
	}

	void Run(Program program)
	{
		for(Loc pc = 0; pc < program.size();)
		{
			Instruction ins = program.at(pc);

			switch (ins.type)
			{
                case INSTRUCTION_PUSH:
                {
                    stack.push(ins.value);
                } break;
                case INSTRUCTION_POP:
                {
                    stack.pop();
                } break;

                case INSTRUCTION_STORE:
                {
                    Value v = stack.top();
                    stack.pop();

                    current->SetIdentifier(ins.value.GetId(), v);
                } break;
                case INSTRUCTION_LOAD:
                {
                    auto id = current->FindIdentifier(ins.value.GetId());
                    if (id)
                    {
                        stack.push(*id);
                    }
                } break;

                case INSTRUCTION_JUMP:
                {
                    pc = ins.value.GetInt();
                    continue;
                } break;
                case INSTRUCTION_JUMP_IF_TRUE:
                {
                    Value v = stack.top(); stack.pop();
                    if(v.GetBool())
                    {
                        pc = ins.value.GetInt();
                        continue;
                    }
                } break;
                case INSTRUCTION_JUMP_IF_FALSE:
                {
                    Value v = stack.top(); stack.pop();
                    if(!v.GetBool())
                    {
                        pc = ins.value.GetInt();
                        continue;
                    }
                } break;

                case INSTRUCTION_START_SUBROUTINE:
                {
                    jumpTable.emplace(ins.value.GetId(), pc + 1);
                } break;
                case INSTRUCTION_JUMP_SUBROUTINE:
                {
                    auto it = jumpTable.find(ins.value.GetId());
                    if(it != jumpTable.end())
                    {
                        pc = it->second + 1;
                    }
                    else
                    {
                        LogError("Jump location doesn't exist '" + ins.value.GetId()  + "'");
                    }
                } break;
                case INSTRUCTION_RETURN_FROM_SUBROUTINE:
                {
                    pc = returnStack.top(); returnStack.pop();
                    continue;
                } break;

                case INSTRUCTION_ADD:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value r = l.GetInt() + r.GetInt();
                        stack.push(r);
                    }
                    else if(l.IsReal())
                    {
                        Value r = l.GetReal() + r.GetReal();
                        stack.push(r);
                    }
                } break;
                case INSTRUCTION_MINUS:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value r = l.GetInt() - r.GetInt();
                        stack.push(r);
                    }
                    else if(l.IsReal())
                    {
                        Value r = l.GetReal() - r.GetReal();
                        stack.push(r);
                    }
                } break;
                case INSTRUCTION_MULTIPLY:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value r = l.GetInt() * r.GetInt();
                        stack.push(r);
                    }
                    else if(l.IsReal())
                    {
                        Value r = l.GetReal() * r.GetReal();
                        stack.push(r);
                    }
                } break;
                case INSTRUCTION_DIVIDE:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value r = l.GetInt() / r.GetInt();
                        stack.push(r);
                    }
                    else if(l.IsReal())
                    {
                        Value r = l.GetReal() / r.GetReal();
                        stack.push(r);
                    }
                } break;

                case INSTRUCTION_COMP_LESS:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value r = l.GetInt() < r.GetInt();
                        stack.push(r);
                    }
                    else if(l.IsReal())
                    {
                        Value r = l.GetReal() < r.GetReal();
                        stack.push(r);
                    }
                } break;
                case INSTRUCTION_COMP_GREATER:
                case INSTRUCTION_COMP_LESS_EQUAL:
                case INSTRUCTION_COMP_GREATER_EQUAL:
                case INSTRUCTION_COMP_EQUAL:
                case INSTRUCTION_COMP_NOT_EQUAL:
                {
                    // TODO: Implement this
                } break;

                default:
                {
                    LogError("Invalid instruction type: " + std::to_string(ins));
                } break;
			}

			pc++;
		}
	}
};

struct BytecodeEmitter : NodeVisitor
{
    Program program;

	void visit(SequenceNode* node)
	{
        for(auto&& n : node->lst)
        {
            n->visit(this);
        }
	}

	void visit(BinaryNode* node)
	{
        node->left->visit(this);
        node->right->visit(this);
        switch(node->op.type)
        {
            case TOKEN_PLUS: { program.emplace_back(INSTRUCTION_ADD); } break;
            case TOKEN_MINUS: { program.emplace_back(INSTRUCTION_MINUS); } break;
            case TOKEN_MULTIPLY: { program.emplace_back(INSTRUCTION_MULTIPLY); } break;
            case TOKEN_DIVIDE: { program.emplace_back(INSTRUCTION_DIVIDE); } break;

            case TOKEN_EQUAL_EQUAL: { program.emplace_back(INSTRUCTION_COMP_EQUAL); } break;
            case TOKEN_NOT_EQUAL: { program.emplace_back(INSTRUCTION_COMP_NOT_EQUAL); } break;
            case TOKEN_LESS: { program.emplace_back(INSTRUCTION_COMP_LESS); } break;
            case TOKEN_GREATER: { program.emplace_back(INSTRUCTION_COMP_GREATER); } break;
            case TOKEN_LESS_EQUAL: { program.emplace_back(INSTRUCTION_COMP_LESS_EQUAL); } break;
            case TOKEN_GREATER_EQUAL: { program.emplace_back(INSTRUCTION_COMP_GREATER_EQUAL); } break;

            default:
            {
                LogError("Invalid binary operator '" + std::to_string(node->op) + "'");
            } break;
        }
	}

	void visit(ValueNode* node)
    {
        if(node->value.IsId())
        {
            program.emplace_back(INSTRUCTION_LOAD, node->value);
        }
        else
        {
            program.emplace_back(INSTRUCTION_PUSH, node->value);
        }
    }

	void visit(VariableDeclNode* node)
	{
        program.emplace_back(INSTRUCTION_PUSH, Value((int64_t)0));
        program.emplace_back(INSTRUCTION_STORE, Value(node->id.text));
	}

	void visit(AssignmentNode* node)
	{
        node->value->visit(this);
        program.emplace_back(INSTRUCTION_STORE, Value(node->id.text));
	}

	void visit(FunctionNode* node)
	{
        program.emplace_back(INSTRUCTION_START_SUBROUTINE, Value(node->id.text));

        int64_t start = program.size();
        program.emplace_back(INSTRUCTION_JUMP, Value((int64_t)0));

        for(Node* arg : node->args)
        {
            arg->visit(this);
        }

        node->block->visit(this);
        program.emplace_back(INSTRUCTION_RETURN_FROM_SUBROUTINE);

        program.at(start).value = (int64_t)program.size();
	}

	void visit(CallNode* node)
	{
        for(auto&& arg : node->args)
        {
            arg->visit(this);
        }
        program.emplace_back(INSTRUCTION_JUMP_SUBROUTINE, Value(node->id.text));
	}

	void visit(IfNode* node)
	{
        node->cond->visit(this);
        if(node->elseblock == NULL)
        {
            int64_t start = program.size();
            program.emplace_back(INSTRUCTION_JUMP_IF_FALSE, Value((int64_t)0));
            node->block->visit(this);
            program.at(start).value = (int64_t)program.size();
        }
	}

	void visit(ForNode* node)
	{
	}

	void visit(LoopNode* node)
	{
        int64_t start = program.size(); 
        node->block->visit(this);
        // TODO: Figure out a better way to do loop break, and continue
        program.emplace_back(INSTRUCTION_JUMP, Value((int64_t)start));
	}

	void visit(ReturnNode* node)
    {
        program.emplace_back(INSTRUCTION_RETURN_FROM_SUBROUTINE);
    }

	void visit(BreakNode* node)
    {
    }

	void visit(ContinueNode* node)
    {
    }
};

void PrintProgram(Program program)
{
    std::cout << " --- BYTECODE PROGRAM, SIZE: " << program.size() << " --- \n";
    for(int i = 0; i < program.size(); i++)
    {
        auto ins = program.at(i);
        std::cout << "[" << i << "]: " << std::to_string(ins) << '\n';
    }
}

void PrintTokenStream(TokenStream stream)
{
	for (auto&& t : stream)
	{
        std::cout << std::to_string(t);
	}
}


std::string LoadEntireFileIntoString(std::string path)
{
	std::filesystem::path fspath = path;
	if (!std::filesystem::exists(fspath))
	{
		LogError("file does not exists within path");
		return "";
	}

	std::ifstream i(path);
	std::stringstream buffer;
	buffer << i.rdbuf();

	return buffer.str();
}

int main()
{
	auto source = LoadEntireFileIntoString("test.code");

	if (!source.empty())
	{
		Lexer lexer(source);
		if (error) return 1;

		PrintTokenStream(lexer.stream);

		Parser parser(lexer.stream);
		if (error) return 1;
		
		TypeChecker tcv;
		parser.tree->visit(&tcv);
		if (error) return 1;

        BytecodeEmitter bce;
        parser.tree->visit(&bce);
		if (error) return 1;

        PrintProgram(bce.program);

        VirtualMachine vm;
        vm.Run(bce.program);
		if (error) return 1;
		
		NodeFreeVisitor nfv;
		parser.tree->visit(&nfv);
	}
	else
	{
		LogError("Source is empty");
	}

	return 0;
}
