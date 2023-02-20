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
<statement> ::= "if" <expression> <statement-block> ("else" <statement-block>)?
			  | "loop" <statement-block>
			  | "for" (<variable-decleration> | <expression>) ',' <expression> ',' <expression> <statement-block>
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

<expression> ::= string | integer | float | identifier
<literal> ::= integer | float | string | identifier

<function-call> ::= <identifier> '(' <argument-list> ')'
*/

bool error = false;
void LogError(std::string msg)
{
	std::cout << "Error: " << msg << '\n';
    error = true;
}

void LogError(std::string msg, int line, int location)
{
	std::cout << "Error: [Line: " << line << ", Location: " << location << "] " << msg << '\n';
    error = true;
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

	TOKEN_PLUS, TOKEN_MINUS, TOKEN_DIVIDE, TOKEN_MULTIPLY,

	TOKEN_EQUAL, TOKEN_NOT,

	TOKEN_EQUAL_EQUAL, TOKEN_NOT_EQUAL,
	TOKEN_LESS, TOKEN_GREATER, TOKEN_LESS_EQUAL, TOKEN_GREATER_EQUAL,

    TOKEN_AND, TOKEN_OR,
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

namespace std {
    std::string to_string(Token other) {
        return std::to_string(other.type) + ": " + "\"" + other.text + "\"" + '\n';
    }
}

std::map<std::string, TokenType> keywords = {
	{ "if", TOKEN_IF },
	{ "else", TOKEN_ELSE },
	{ "func", TOKEN_FUNC },
	{ "for", TOKEN_FOR },
	{ "loop", TOKEN_LOOP },
	{ "return", TOKEN_RETURN },
	{ "break", TOKEN_BREAK },
	{ "continue", TOKEN_CONTINUE },

    { "true", TOKEN_LITERAL },
    { "false", TOKEN_LITERAL },
};

enum ValueType
{
    VALUE_NONE,

    VALUE_INT, VALUE_REAL, VALUE_STR, VALUE_BOOL,
    VALUE_ID
};

ValueType StringToValueType(std::string type)
{
    if(type == "str") return VALUE_STR;
    if(type == "int") return VALUE_INT;
    if(type == "float") return VALUE_REAL;
    if(type == "bool") return VALUE_BOOL;

    return VALUE_NONE;
}

namespace std {
    std::string to_string(ValueType other) {
        switch (other)
        {
            case VALUE_INT: { return "int"; } break;
            case VALUE_REAL: { return "float"; } break;
            case VALUE_STR: { return "str"; } break;
            case VALUE_BOOL: { return "bool"; } break;
            case VALUE_ID: { return "id"; } break;
        }
        return "";
    }
}

class Value
{
private:
	ValueType type = VALUE_NONE;
    void* data = NULL;

    void FreeData()
    {
        if(data == NULL) return;

        switch(type)
        {
            case VALUE_INT:
            {
                delete static_cast<int64_t*>(data);
            } break;
            case VALUE_REAL:
            {
                delete static_cast<double*>(data);
            } break;
            case VALUE_BOOL:
            {
                delete static_cast<bool*>(data);
            } break;
            case VALUE_STR:
            {
                delete static_cast<std::string*>(data);
            } break;
            case VALUE_ID:
            {
                delete static_cast<std::string*>(data);
            } break;
        }
    }

    void ParseStr(std::string text)
    {
        char b = text.front(), e = text.back();
        if(b == '"' && e == '"')
        {
            std::string r = text;
            r.erase(0,1);
            r.erase(r.size(),1);

            data = new std::string(r);
            type = VALUE_STR;
        }
    }

    void ParseInt(std::string text)
    {
        for(int i = 0; i < text.size(); i++)
        {
            char c = text.at(i);
            if(!IsNumber(c))
            {
                return;
            }
        }

        type = VALUE_INT;
        data = new int64_t(std::stoll(text));
    }

    void ParseReal(std::string text)
    {
        bool dot_used = false;
        for(int i = 0; i < text.size(); i++)
        {
            char c = text.at(i);
            if(c == '.')
            {
                if(dot_used)
                {
                    LogError("Real number cannot contain two decimal points", linked.line, linked.location);
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
        data = new double(std::stod(text));
    }

    void ParseBool(std::string text)
    {
        if(text == "true")
        {
            type = VALUE_BOOL;
            data = new bool(true);
        }
        else if(text == "false")
        {
            type = VALUE_BOOL;
            data = new bool(false);
        }
    }
    
    void ParseIdentifier(std::string text)
    {
        char b = text.front(), e = text.back();
        if(b == '"' && e == '"')
        {
            return;
        }

        data = new std::string(text);
        type = VALUE_ID;
    }
public:
    bool IsInt() const { return type == VALUE_INT; }
    bool IsReal() const { return type == VALUE_REAL; }
    bool IsStr() const { return type == VALUE_STR; }
    bool IsBool() const { return type == VALUE_BOOL; }
    bool IsId() const { return type == VALUE_ID; }

    ValueType& GetType() { return type; }

    int64_t* GetInt() const { return static_cast<int64_t*>(data); }
    double* GetReal() const { return static_cast<double*>(data); }
    std::string* GetStr() const { return static_cast<std::string*>(data); }
    bool* GetBool() const { return static_cast<bool*>(data); }
    std::string* GetId() const { return static_cast<std::string*>(data); }

    void Negative()
    {
        if(IsInt())
        {
            *GetInt() = -*GetInt();
        }
        else if(IsReal())
        {
            *GetReal() = -*GetReal();
        }
        else
        {
            LogError("Cannot negativize type '" + GetTypeAsString() + "'");
        }
    }

    Value() = default;

    ~Value() {
        FreeData();
    }

    Value(const Value& other)
    {
        LinkToken(other.linked);
        type = other.type;

        switch(type)
        {
            case VALUE_INT:
            {
                data = new int64_t(*other.GetInt());
            } break;
            case VALUE_REAL:
            {
                data = new double(*other.GetReal());
            } break;
            case VALUE_BOOL:
            {
                data = new bool(*other.GetBool());
            } break;
            case VALUE_STR:
            {
                data = new std::string(*other.GetStr());
            } break;
            case VALUE_ID:
            {
                data = new std::string(*other.GetId());
            } break;
        }
    }

    Value(Token& other)
    {
        LinkToken(other);

        if(other.type == TOKEN_IDENTIFIER)
        {
            ParseIdentifier(other.text);
        }
        else if(other.type == TOKEN_LITERAL)
        {
            ParseStr(other.text);
            ParseReal(other.text);
            ParseInt(other.text);
            ParseBool(other.text);
        }
    }

	Value(int64_t other)
	{
		type = VALUE_INT;
		data = new int64_t(other);
	}

	Value(double other)
	{
		type = VALUE_REAL;
		data = new double(other);
	}

	Value(std::string other)
	{
        ParseStr(other);
        ParseIdentifier(other);
	}

	Value(bool other)
	{
		type = VALUE_BOOL;
		data = new bool(other);
	}

    Value(ValueType type_)
        : type(type_)
    {
        switch(type)
        {
            case VALUE_INT:
            {
                data = new int64_t(0);
            } break;
            case VALUE_REAL:
            {
                data = new double(0.0);
            } break;
            case VALUE_BOOL:
            {
                data = new bool(false);
            } break;
            case VALUE_STR:
            {
                data = new std::string("");
            } break;
            case VALUE_ID:
            {
                data = new std::string("");
            } break;
        }
    }

    Value(ValueType type_, std::string text_)
    {
        switch(type_)
        {
            case VALUE_INT:
            {
                ParseInt(text_);
            } break;
            case VALUE_REAL:
            {
                ParseReal(text_);
            } break;
            case VALUE_BOOL:
            {
                ParseBool(text_);
            } break;
            case VALUE_STR:
            {
                ParseStr(text_);
            } break;
            case VALUE_ID:
            {
                ParseIdentifier(text_);
            } break;
            default:
            {
                ParseIdentifier(text_);
                ParseStr(text_);
                ParseReal(text_);
                ParseInt(text_);
                ParseBool(text_);
            }
        }
    }

	Value& operator=(Value& other)
    {
        FreeData();
        LinkToken(other.linked);
        type = other.type;

        switch(type)
        {
            case VALUE_INT:
            {
                data = new int64_t(*other.GetInt());
            } break;
            case VALUE_REAL:
            {
                data = new double(*other.GetReal());
            } break;
            case VALUE_BOOL:
            {
                data = new bool(*other.GetBool());
            } break;
            case VALUE_STR:
            {
                data = new std::string(*other.GetStr());
            } break;
            case VALUE_ID:
            {
                data = new std::string(*other.GetId());
            } break;
        }

        return *this;
    }


    Value& operator=(Token& other)
    {
        LinkToken(other);

        if(other.type == TOKEN_IDENTIFIER)
        {
            ParseIdentifier(other.text);
        }
        else if(other.type == TOKEN_LITERAL)
        {
            ParseStr(other.text);
            ParseReal(other.text);
            ParseInt(other.text);
            ParseBool(other.text);
        }

        return *this;
    }

	Value& operator=(int64_t other)
	{
        FreeData();
		type = VALUE_INT;
		data = new int64_t(other);
        return *this;
	}

	Value& operator=(double other)
	{
        FreeData();
		type = VALUE_REAL;
		data = new double(other);
        return *this;
	}

	Value& operator=(bool other)
	{
        FreeData();
		type = VALUE_BOOL;
		data = new bool(other);
        return *this;
	}

	Value& operator=(const char* other)
	{
        FreeData();
        ParseStr(std::string(other));
        ParseIdentifier(std::string(other));

        return *this;
	}

	Value& operator=(std::string other)
	{
        FreeData();
        ParseStr(other);
        ParseIdentifier(other);
        return *this;
	}

    Value& operator=(ValueType type_)
    {
        FreeData();
        type = type_;
        switch(type)
        {
            case VALUE_INT:
            {
                data = new int64_t(0);
            } break;
            case VALUE_REAL:
            {
                data = new double(0.0);
            } break;
            case VALUE_BOOL:
            {
                data = new bool(false);
            } break;
            case VALUE_STR:
            {
                data = new std::string("");
            } break;
            case VALUE_ID:
            {
                data = new std::string("");
            } break;
        }

        return *this;
    }
private:
    Token linked;
public:
    void LinkToken(const Token t)
    {
        linked = t;
    }

    int GetLoc() const { return linked.location; }
    int GetLine() const { return linked.line; }

    std::string GetTypeAsString() const
    {
        return std::to_string(type);
    }
};

namespace std {
    std::string to_string(Value other) {
        switch (other.GetType())
        {
            case VALUE_INT: { return std::to_string(*other.GetInt()) + ": " + std::to_string(other.GetType()); } break;
            case VALUE_REAL: { return std::to_string(*other.GetReal()) + ": " + std::to_string(other.GetType()); } break;
            case VALUE_STR: { return *other.GetStr() + ": " + std::to_string(other.GetType()); } break;
            case VALUE_BOOL: { return (std::string)(*other.GetBool() ? "true" : "false") + ": " + std::to_string(other.GetType()); } break;
            case VALUE_ID: { return *other.GetId() + ": " + std::to_string(other.GetType()); } break;
        }
        return "";
    }
}


bool CompareValueTypes(Value l, Value r)
{
    return l.GetType() == r.GetType();
}

TokenType GetKeyword(std::string text)
{
	auto it = keywords.find(text);
	if (it != keywords.end()) return it->second;
	return TOKEN_IDENTIFIER;
}

using TokenStream = std::vector<Token>;
class Lexer
{
public:
	TokenStream stream;
private:
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

    void eval() {
        		int line = 0;
		for (location = 0; location < source.size(); location++)
		{
			char c = source.at(location);

			Token token;
			token.location = location;
			token.size = 1;
			token.line = line;

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
				if (Peek() == '/')
				{
					while (Peek() != '\n')
					{
						Advance();
					}

					line++;
					continue;
				}
                else if(Peek() == '*')
                {
                    Advance();
                    Advance();

                    int nested = 1;
                    while(nested > 0)
                    {
                        if(GetCurrent() == '\n')
                        {
                            line++;
                        }

                        if(GetCurrent() == '/' && Peek() == '*')
                        {
                            nested++;
                        }
                        else if(GetCurrent() == '*' && Peek() == '/')
                        {
                            nested--;
                        }

                        Advance();
                    }

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

            case '&':
            {
				if(Match('&')) { token.type = TOKEN_AND; break; }
            }
            case '|':
            {
				if(Match('|')) { token.type = TOKEN_OR; break; }
            }

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
public:
	Lexer(std::string source_)
		: source(source_)
	{
        eval();
	}
};

struct Node;
struct ScopeNode;
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
	virtual void visit(ScopeNode* node) = 0;
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

#define PRINT_FREED_MEMORY false
struct Node
{
	virtual void visit(NodeVisitor* visitor) = 0;

    ~Node()
	{
#if PRINT_FREED_MEMORY
		std::cout << "Freed memory " << this << '\n';
#endif
	}
};

#define VISIT_ virtual void visit(NodeVisitor* v) { v->visit(this); } 

struct ScopeNode : Node
{
    Node* statement;

	ScopeNode(Node* statement_)
		: statement(statement_)
	{}

	VISIT_
};

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
    Token token;
    bool negative = false;

	ValueNode(Token token_, bool negative_ = false)
        : token(token_), negative(negative_)
    {}

	ValueNode(Token token_, ValueType type_, bool negative_ = false)
        : token(token_), type(type_), negative(negative_)
    {}
private:
    ValueType type = VALUE_NONE;
public:
    void SetType(ValueType type_) {
        type = type_;
    }

    ValueType GetType() {
        return type;
    }

	VISIT_
};

struct VariableDeclNode : Node
{
	Token id, type;
    Node* value;
    bool infer;

	VariableDeclNode(Token id_, Token type_, Node* value_ = NULL)
		: id(id_), type(type_), value(value_), infer(false)
	{}

	VariableDeclNode(Token id_, Node* value_ = NULL)
		: id(id_), type(), value(value_), infer(true)
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
    { TOKEN_AND, 0 },
    { TOKEN_OR, 0 },

    { TOKEN_EQUAL_EQUAL, 1 },
    { TOKEN_NOT_EQUAL , 1 },
    { TOKEN_LESS, 1 },
    { TOKEN_GREATER, 1 },
    { TOKEN_LESS_EQUAL, 1 },
    { TOKEN_GREATER_EQUAL, 1 },

    { TOKEN_PLUS,2 },
    { TOKEN_MINUS,3 },
    { TOKEN_MULTIPLY,4 },
    { TOKEN_DIVIDE,5 },
};

int GetPrecedence(Token t)
{
    auto it = operatorprecendence.find(t.type);
    if(it != operatorprecendence.end()) return it->second;
    return -1;
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
				return new ScopeNode(lst);
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
			Node* cond = Expression();
			Node* block = StatementBlock();
			Node* elseblock = NULL;

			if (AdvanceIfMatch(TOKEN_ELSE))
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
            Node* init = VariableDecleration();
            if(init == NULL) init = Expression();

            if(AdvanceIfMatch(TOKEN_COMMA))
            {
                Node* cond = Expression();
                if(AdvanceIfMatch(TOKEN_COMMA))
                {
                    Node* incr = Expression();

                    Node* block = StatementBlock();

                    return new ForNode(init, cond, incr, block);
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
                else
                {
                    LogError("Argument list decleration must have a closing parenthesis", id.line, id.location);
                }
			}
            else
            {
                LogError("Function statement must have opening parenthesis", id.line, id.location);
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
            else
            {
                LogError("Variable decleration must end with a semicolon", current.line, current.location);
            }
        }

        if(Node* n = VariableAssignment(); n != NULL)
        {
            if(AdvanceIfMatch(TOKEN_SEMICOLON))
            {
                return n;
            }
            else
            {
                LogError("Variable assignment must end with a semicolon", current.line, current.location);
            }
        }

        if(Node* n = Expression(); n != NULL)
        {
            if(AdvanceIfMatch(TOKEN_SEMICOLON))
            {
                return n;
            }
            else
            {
                LogError("Expression statement must end with a semicolon", current.line, current.location);
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
                            return new VariableDeclNode(id, type, value);
                        }
                        else
                        {
                            LogError("Value assigned to '" + id.text + "' is invalid", id.line, id.location);
                        }
                    }
                    else
                    {
                        return new VariableDeclNode(id, type);
                    }
                }
                else if(AdvanceIfMatch(TOKEN_EQUAL))
                {
                    if(Node* value = Expression(); value != NULL)
                    {
                        return new VariableDeclNode(id, value);
                    }
                    else
                    {
                        LogError("Value assigned to '" + id.text + "' is invalid", id.line, id.location);
                    }
                }
                else
                {
                    LogError("Variable decleration must have either specify a type of be assigned a value", id.line, id.location);
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
            if(arg == NULL)
            {
                if(current.type == TOKEN_COMMA) {
                    LogError("Agrgument decleration must precede a comma", current.line, current.location);
                    return {};
                }

                break;
            }
            sequence.push_back(arg);
        }
        while(AdvanceIfMatch(TOKEN_COMMA));

		return sequence;
	}

	std::vector<Node*> ArgumentList()
	{
        std::vector<Node*> sequence;

        Node* arg = NULL;
        do
        {
            arg = Expression();
            if(arg == NULL)
            {
                if(current.type == TOKEN_COMMA) {
                    LogError("Agrgument decleration must precede a comma", current.line, current.location);
                    return {};
                }

                break;
            }

            sequence.push_back(arg);
        }
        while(AdvanceIfMatch(TOKEN_COMMA));

		return sequence;
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
        else if(AdvanceIfMatch(TOKEN_NOT))
        {
            // TODO: Implement not expression
            Node* expr = Expression();
            return expr;
        }
        else if(AdvanceIfMatch(TOKEN_LEFT_PARENTHESIS))
        {
            Node* expr = Expression();
            if(AdvanceIfMatch(TOKEN_RIGHT_PARENTHESIS))
            {
                return expr;
            }
            else
            {
                LogError("Grouped expression must have a closing parenthesis", current.line, current.location);
            }
        }
        else if(t.type == TOKEN_IDENTIFIER)
        {
            Node* func_call = FunctionCall();
            if(func_call != NULL) return func_call;

            Advance();
            return new ValueNode(t, VALUE_ID);
        }
        // TODO: Add array literal

        return NULL;
	}
};

struct FunctionData {
    ValueType returntype;
    std::vector<ValueType> args;

    FunctionData() = default;

    FunctionData(std::string returntype_, std::vector<ValueType> args_)
        : returntype(StringToValueType(returntype_)), args(args_)
    {}
};

struct ScopedSymbolTable {
    ScopedSymbolTable* parent = NULL;

    ScopedSymbolTable(ScopedSymbolTable* parent_ = NULL)
        : parent(parent_)
    {}

	std::map<std::string, std::string> typetable;

	void SetType(std::string name, std::string type)
	{
		typetable[name] = type;
	}

	ValueType FindType(std::string name)
	{
		auto it = typetable.find(name);
		if (it != typetable.end()) return StringToValueType(it->second);

		if (parent != NULL) return parent->FindType(name);

		return VALUE_NONE;
	}

    bool VariableExists(std::string name) {
		auto it = typetable.find(name);
		if (it != typetable.end()) return true;

		if (parent != NULL) return parent->FindType(name);

		return false;
    }

	std::map<std::string, FunctionData> functable;
    
	void SetFunction(std::string name, FunctionData data)
	{
		functable[name] = data;
	}

    std::optional<FunctionData> FindFunction(std::string name)
	{
		auto it = functable.find(name);
		if (it != functable.end()) return it->second;

		if (parent != NULL) return parent->FindFunction(name);

		return {};
	}
};

class TypeChecker : public NodeVisitor
{
private:
    ScopedSymbolTable global, *current;
    Stack<Value> stack;

    ValueType assignmentType = VALUE_NONE;
    bool is_arg = false;
public:
    TypeChecker() {
        current = &global;
    }

    ~TypeChecker()
    {
        while(current->parent != NULL)
        {
            DeleteCurrent();
        }
    }
private:
	void NewCurrent()
	{
		if (current == NULL) return;

		ScopedSymbolTable* temp = current;

		current = new ScopedSymbolTable();
		current->parent = temp;
	}

	void DeleteCurrent()
	{
		if (current->parent == NULL) return;

        ScopedSymbolTable* temp = current;
		current = temp->parent;
        delete temp;
	}
public:
	void visit(ScopeNode* node)
    {
        NewCurrent();
        node->statement->visit(this);
        DeleteCurrent();
    }

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

        // TODO: Test if these arent neccessary.
        //if(l.IsId()) l.GetType() = current->FindType(*l.GetId());
        //if(r.IsId()) r.GetType() = current->FindType(*r.GetId());

        if(!CompareValueTypes(l, r))
        {
            LogError("Type '" + l.GetTypeAsString() + "' is incompatible with type '" + r.GetTypeAsString() + "'", l.GetLine(), l.GetLoc());
        }

        if(!(node->op.type < TOKEN_EQUAL_EQUAL || node->op.type > TOKEN_OR))
        {
            l = true;
        }

        stack.push(l);
	}

	void visit(ValueNode* node)
    {
        if(assignmentType != VALUE_NONE) {
            node->SetType(assignmentType);
            assignmentType = VALUE_NONE;
        }

        Value v(node->token);

        if(node->GetType() == VALUE_ID) {
            if(current->VariableExists(node->token.text)) {
                v.GetType() = current->FindType(node->token.text);
            } else {
                LogError("Variable '" + node->token.text + "' does not exist within the current scope", node->token.line, node->token.location);
            }
        }

        stack.push(v);
    }

	void visit(VariableDeclNode* node)
	{
        if(current->VariableExists(node->id.text)) {
            LogError("Variable already exist's with name '" + node->id.text + "' within the current scope", node->id.line, node->id.location);
            return;
        }

        ValueType vartype = VALUE_NONE;
        if(node->infer) {
            node->value->visit(this);
            Value value = stack.top(); stack.pop();
            vartype = value.GetType();
        }
        else {
            vartype = StringToValueType(node->type.text);
            if(vartype == VALUE_NONE) {
                LogError("Type does not exist '" + node->type.text + "'", node->type.line, node->type.location);
                return;
            }

            if(node->value != NULL)
            {
                node->value->visit(this);
                Value valuetype = stack.top(); stack.pop();
                if(!CompareValueTypes(vartype, valuetype))
                {
                    LogError("Cannot assign '" + std::to_string(valuetype) + "' to '" + std::to_string(vartype) + "'", node->id.line, node->id.location);
                    return;
                }
            }
        }

        current->SetType(node->id.text, std::to_string(vartype));
        if(is_arg) stack.push(vartype);
	}

	void visit(AssignmentNode* node)
	{
        Value var = current->FindType(node->id.text);

        assignmentType = var.GetType();
        node->value->visit(this);
        Value type = stack.top(); stack.pop();
        if(!CompareValueTypes(var, type))
        {
            LogError("Cannot assign '" + type.GetTypeAsString() + "' to '" + var.GetTypeAsString() + "'", node->id.line, node->id.location);
        }
	}

	void visit(FunctionNode* node)
	{
        NewCurrent();
        std::vector<ValueType> args;

        is_arg = true;
        for(auto&& arg : node->args)
        {
            arg->visit(this);
            Value argtype = stack.top(); stack.pop();
            args.push_back(argtype.GetType());
        }
        is_arg = false;

        current->parent->SetFunction(node->id.text, FunctionData(node->returntype.text, args));

        node->block->visit(this);

        DeleteCurrent();
	}

	void visit(CallNode* node)
	{
        auto func = current->FindFunction(node->id.text);
        if(func)
        {
            if(node->args.size() == func->args.size())
            {
                for(int i = 0; i < node->args.size(); i++)
                {
                    node->args[i]->visit(this);

                    Value declared = func->args.at(i);
                    Value passed = stack.top(); stack.pop();

                    if(!CompareValueTypes(declared, passed))
                    {
                        LogError("Argument " + std::to_string(i + 1) + " expect '" + declared.GetTypeAsString() + "' but got '" + passed.GetTypeAsString() + "'", passed.GetLine(), passed.GetLoc());
                    }
                }

                stack.push(func->returntype);
            }
            else
            {
                LogError("Invalid arg's size: Expected '" + std::to_string(func->args.size()) + "'", node->id.line, node->id.location);
            }
        }
        else
        {
            LogError("Function '" + node->id.text + "' does not exist within the current scope", node->id.line, node->id.location);
        }
	}

	void visit(IfNode* node)
	{
		node->cond->visit(this);
        Value cond = stack.top(); stack.pop();
        if(!cond.IsBool()) {
            LogError("Condition must be of boolean type", cond.GetLine(), cond.GetLoc());
            return;
        }
		node->block->visit(this);
		if (node->elseblock != NULL) node->elseblock->visit(this);
	}

	void visit(ForNode* node)
	{
        NewCurrent();
		if (node->init != NULL) node->init->visit(this);
		if (node->cond != NULL) node->cond->visit(this);
		if (node->incr != NULL) node->incr->visit(this);
		node->block->visit(this);
        DeleteCurrent();
	}

	void visit(LoopNode* node)
	{
		node->block->visit(this);
	}

	void visit(ReturnNode* node)
    {
        // TODO: Make sure returned value has same type as returntype.
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
	void visit(ScopeNode* node)
    {
        node->statement->visit(this);
        delete node->statement;
    }

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
	void visit(VariableDeclNode* node)
    {
        if(node->value != NULL)
        {
            node->value->visit(this);
            delete node->value;
        }
    }

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

    INSTRUCTION_COMP_AND, INSTRUCTION_COMP_OR,

    INSTRUCTION_START_LOOP, INSTRUCTION_JUMP_LOOP, INSTRUCTION_BREAK_LOOP,
};

struct Instruction
{
	InstructionType type;
	Value value;

    Instruction(InstructionType type_)
        : type(type_)
    {}

    Instruction(InstructionType type_, Value value_)
        : type(type_), value(value_)
    {}
};

void PrintInstruction(Instruction ins)
{
    std::cout << '\n';
}

namespace std {
    std::string to_string(Instruction other) {
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

            case INSTRUCTION_START_LOOP: { return "START_LOOP"; } break;
            case INSTRUCTION_JUMP_LOOP: { return "JUMP_LOOP"; } break;
            case INSTRUCTION_BREAK_LOOP: { return "BREAK_LOOP"; } break;

            case INSTRUCTION_COMP_AND: { return "COMP_AND"; } break;
            case INSTRUCTION_COMP_OR: { return "COMP_OR"; } break;

        }

        return "INVALID INSTRUCTION";
    }
}

struct Environment
{
	std::map<std::string, Value> identifiers;

    void CreateIdentifier(std::string name)
    {
		identifiers[name] = (int64_t)0;
    }

	void SetIdentifier(std::string name, Value value)
	{
		auto it = identifiers.find(name);

		if (it != identifiers.end())
		{
			it->second = value;
            return;
		}
	}

	std::optional<Value> FindIdentifier(std::string name)
	{
		auto it = identifiers.find(name);

		if (it != identifiers.end())
		{
			return it->second;
		}

		return {};
	}

    void Print()
    {
        std::cout << "IDENTIFIERS - VALUE\n";
        for(auto&& [id,value] : identifiers)
        {
            char last = id.back();
            if(last >= '1' && last <= '9') continue;

            std::string n = id.substr(0, id.size()-1);
            std::cout << n << " : " << std::to_string(value) << '\n';
        }
    }
};

#define VM_BINARY_OP(type, l, op, r)\
    {\
        Value result = l.Get##type() op r.Get##type();\
        stack.push(result);\
    }

class Program {
private:
    std::vector<Instruction> stream;
public:
    void emplace(InstructionType type) {
        stream.push_back(Instruction(type));
    }

    void emplace(InstructionType type, Value value) {
        stream.push_back(Instruction(type, Value(value)));
    }

    Instruction& at(size_t idx) {
        return stream.at(idx);
    }

    size_t size() const {
        return stream.size();
    }
};

class VirtualMachine
{
private:
    using Loc = std::uint64_t;

	Stack<Value> stack;
    Stack<Loc> callStack;
    std::map<std::string,Loc> jumpTable;

	Environment env;

public:
    void PrintFinalSymbolTable()
    {
        std::cout << "--- FINAL IDENTIFIER TABLE --- \n";
        env.Print();
        std::cout << "\n";
    }

    void PrintRestOfStack()
    {
        if(!stack.empty())
        {
            std::cout << "--- REST OF STACK --- \n";
            while(!stack.empty())
            {
                Value v = stack.top(); stack.pop();
                std::cout << std::to_string(v) << "\n";
            }
        }
        std::cout << "\n";
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

                    std::string name = *ins.value.GetId() + std::to_string(callStack.size());

                    auto id = env.FindIdentifier(name);
                    if(id) {
                        env.SetIdentifier(name, v);
                    }
                    else {
                        env.CreateIdentifier(name);
                        env.SetIdentifier(name, v);
                    }
                } break;
                case INSTRUCTION_LOAD:
                {
                    std::string name = *ins.value.GetId() + std::to_string(callStack.size());
                    auto id = env.FindIdentifier(name);
                    if (id)
                    {
                        stack.push(*id);
                    }
                } break;

                case INSTRUCTION_JUMP:
                {
                    pc = *ins.value.GetInt();
                    continue;
                } break;
                case INSTRUCTION_JUMP_IF_TRUE:
                {
                    Value v = stack.top(); stack.pop();
                    if(v.IsBool()) {
                        if(*v.GetBool())
                        {
                            pc = *ins.value.GetInt();
                            continue;
                        }
                    }
                } break;
                case INSTRUCTION_JUMP_IF_FALSE:
                {
                    Value v = stack.top(); stack.pop();
                    if(v.IsBool()) {
                        if(!*v.GetBool())
                        {
                            pc = *ins.value.GetInt();
                            continue;
                        }
                    }
                } break;

                case INSTRUCTION_START_SUBROUTINE:
                {
                    jumpTable.emplace(*ins.value.GetId(), pc + 2);
                } break;
                case INSTRUCTION_JUMP_SUBROUTINE:
                {
                    auto it = jumpTable.find(*ins.value.GetId());
                    if(it != jumpTable.end())
                    {
                        callStack.push(pc + 1);
                        pc = it->second;
                    }
                    else
                    {
                        LogError("Jump location doesn't exist '" + *ins.value.GetId()  + "'");
                    }
                    continue;
                } break;
                case INSTRUCTION_RETURN_FROM_SUBROUTINE:
                {
                    pc = callStack.top(); callStack.pop();
                    continue;
                } break;

                case INSTRUCTION_ADD:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value result = *l.GetInt() + *r.GetInt();
                        stack.push(result);
                    }
                    else if(l.IsReal())
                    {
                        Value result = *l.GetReal() + *r.GetReal();
                        stack.push(result);
                    }
                } break;
                case INSTRUCTION_MINUS:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value result = *l.GetInt() - *r.GetInt();
                        stack.push(result);
                    }
                    else if(l.IsReal())
                    {
                        Value result = *l.GetReal() - *r.GetReal();
                        stack.push(result);
                    }
                } break;
                case INSTRUCTION_MULTIPLY:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value result = *l.GetInt() * *r.GetInt();
                        stack.push(result);
                    }
                    else if(l.IsReal())
                    {
                        Value result = *l.GetReal() * *r.GetReal();
                        stack.push(result);
                    }
                } break;
                case INSTRUCTION_DIVIDE:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value result = *l.GetInt() / *r.GetInt();
                        stack.push(result);
                    }
                    else if(l.IsReal())
                    {
                        Value result = *l.GetReal() / *r.GetReal();
                        stack.push(result);
                    }
                } break;

                case INSTRUCTION_COMP_LESS:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value result = (bool)(*l.GetInt() < *r.GetInt());
                        stack.push(result);
                    }
                    else if(l.IsReal())
                    {
                        Value result = (bool)(*l.GetReal() < *r.GetReal());
                        stack.push(result);
                    }
                } break;
                case INSTRUCTION_COMP_GREATER:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value result = (bool)(*l.GetInt() > *r.GetInt());
                        stack.push(result);
                    }
                    else if(l.IsReal())
                    {
                        Value result = (bool)(*l.GetReal() > *r.GetReal());
                        stack.push(result);
                    }
                } break;
                case INSTRUCTION_COMP_LESS_EQUAL:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value result = (bool)(*l.GetInt() <= *r.GetInt());
                        stack.push(result);
                    }
                    else if(l.IsReal())
                    {
                        Value result = (bool)(*l.GetReal() <= *r.GetReal());
                        stack.push(result);
                    }
                } break;
                case INSTRUCTION_COMP_GREATER_EQUAL:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value result = (bool)(*l.GetInt() >= *r.GetInt());
                        stack.push(result);
                    }
                    else if(l.IsReal())
                    {
                        Value result = (bool)(*l.GetReal() >= *r.GetReal());
                        stack.push(result);
                    }
                } break;
                case INSTRUCTION_COMP_EQUAL:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value result = (bool)(*l.GetInt() == *r.GetInt());
                        stack.push(result);
                    }
                    else if(l.IsReal())
                    {
                        Value result = (bool)(*l.GetReal() == *r.GetReal());
                        stack.push(result);
                    }
                } break;
                case INSTRUCTION_COMP_NOT_EQUAL:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    if(l.IsInt())
                    {
                        Value result = (bool)(*l.GetInt() != *r.GetInt());
                        stack.push(result);
                    }
                    else if(l.IsReal())
                    {
                        Value result = (bool)(*l.GetReal() != *r.GetReal());
                        stack.push(result);
                    }
                } break;

                case INSTRUCTION_COMP_AND:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    Value result = (bool)(*l.GetBool() && *r.GetBool());
                    stack.push(result);
                } break;
                case INSTRUCTION_COMP_OR:
                {
                    Value r = stack.top(); stack.pop();
                    Value l = stack.top(); stack.pop();

                    Value result = (bool)(*l.GetBool() || *r.GetBool());
                    stack.push(result);
                } break;

                case INSTRUCTION_START_LOOP:
                {

                } break;
                case INSTRUCTION_JUMP_LOOP:
                {

                } break;
                case INSTRUCTION_BREAK_LOOP:
                {

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

	void visit(ScopeNode* node)
    {
        node->statement->visit(this);
    }

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
            case TOKEN_PLUS: { program.emplace(INSTRUCTION_ADD); } break;
            case TOKEN_MINUS: { program.emplace(INSTRUCTION_MINUS); } break;
            case TOKEN_MULTIPLY: { program.emplace(INSTRUCTION_MULTIPLY); } break;
            case TOKEN_DIVIDE: { program.emplace(INSTRUCTION_DIVIDE); } break;

            case TOKEN_EQUAL_EQUAL: { program.emplace(INSTRUCTION_COMP_EQUAL); } break;
            case TOKEN_NOT_EQUAL: { program.emplace(INSTRUCTION_COMP_NOT_EQUAL); } break;
            case TOKEN_LESS: { program.emplace(INSTRUCTION_COMP_LESS); } break;
            case TOKEN_GREATER: { program.emplace(INSTRUCTION_COMP_GREATER); } break;
            case TOKEN_LESS_EQUAL: { program.emplace(INSTRUCTION_COMP_LESS_EQUAL); } break;
            case TOKEN_GREATER_EQUAL: { program.emplace(INSTRUCTION_COMP_GREATER_EQUAL); } break;

            case TOKEN_AND: { program.emplace(INSTRUCTION_COMP_AND); } break;
            case TOKEN_OR: { program.emplace(INSTRUCTION_COMP_OR); } break;

            default:
            {
                LogError("Invalid binary operator '" + std::to_string(node->op) + "'");
            } break;
        }
	}

	void visit(ValueNode* node)
    {
        Value value(node->GetType(), node->token.text);

        if(value.IsId())
        {
            program.emplace(INSTRUCTION_LOAD, value);
        }
        else
        {
            if(node->negative) value.Negative();
            program.emplace(INSTRUCTION_PUSH, value);
        }
    }

	void visit(VariableDeclNode* node)
	{
        if(node->value != NULL)
        {
            node->value->visit(this);
        }
        else
        {
            program.emplace(INSTRUCTION_PUSH, Value((int64_t)0));
        }

        program.emplace(INSTRUCTION_STORE, Value(node->id.text));
        previous_id = node->id.text;
	}

	void visit(AssignmentNode* node)
	{
        node->value->visit(this);
        program.emplace(INSTRUCTION_STORE, Value(VALUE_ID, node->id.text));
	}

    std::string previous_id = "";
	void visit(FunctionNode* node)
	{
        program.emplace(INSTRUCTION_START_SUBROUTINE, Value(node->id.text));

        int64_t start = program.size();
        program.emplace(INSTRUCTION_JUMP, Value((int64_t)0));

        for(int i = node->args.size() - 1; i >= 0; i--)
        {
            node->args[i]->visit(this);
            program.emplace(INSTRUCTION_STORE, Value(previous_id));
        }

        node->block->visit(this);
        program.emplace(INSTRUCTION_RETURN_FROM_SUBROUTINE);

        program.at(start).value = (int64_t)program.size();
	}

	void visit(CallNode* node)
	{
        for(auto&& arg : node->args)
        {
            arg->visit(this);
        }
        program.emplace(INSTRUCTION_JUMP_SUBROUTINE, Value(node->id.text));

        // TODO: Fix scope end problem's
	}

	void visit(IfNode* node)
	{
        node->cond->visit(this);
        if(node->elseblock == NULL)
        {
            int64_t start = program.size();
            program.emplace(INSTRUCTION_JUMP_IF_FALSE, Value((int64_t)0));
            node->block->visit(this);
            program.at(start).value = (int64_t)program.size();
        }
        else
        {
            int64_t start = program.size();
            program.emplace(INSTRUCTION_JUMP_IF_FALSE, Value((int64_t)0));
            node->block->visit(this);
            int64_t end = program.size();
            program.emplace(INSTRUCTION_JUMP, Value((int64_t)0));
            program.at(start).value = (int64_t)program.size();
            node->elseblock->visit(this);
            program.at(end).value = (int64_t)program.size();
        }
        // TODO: Implement else block
	}

	void visit(ForNode* node)
	{
	}

	void visit(LoopNode* node)
	{
        int64_t start = program.size(); 
        node->block->visit(this);
        // TODO: Figure out a better way to do loop break, and continue
        program.emplace(INSTRUCTION_JUMP, Value((int64_t)start));
	}

	void visit(ReturnNode* node)
    {
        if(node->expr != NULL) node->expr->visit(this);
        program.emplace(INSTRUCTION_RETURN_FROM_SUBROUTINE);
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
        vm.PrintRestOfStack();
        vm.PrintFinalSymbolTable();
		
		NodeFreeVisitor nfv;
		parser.tree->visit(&nfv);
        // TODO: Automatic cast int to float if assigning to float e.g.
        // number : float = 25;
        //                   ^ this should be casted to a float

        // TODO: Add more error checking.
	}
	else
	{
		LogError("Source is empty");
	}

	return 0;
}
