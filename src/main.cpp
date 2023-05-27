#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <fstream>
#include <sstream>
#include <filesystem>
#include <optional>
#include <variant>
#include <iomanip>
#include <algorithm>
#include <sstream>
#include <cstring>
#include <cassert>
#include <functional>

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

using u8 = std::uint8_t;
using i8 = std::int8_t;
using u16 = std::uint16_t;
using i16 = std::int16_t;
using u32 = std::uint32_t;
using i32 = std::int32_t;
using u64 = std::uint64_t;
using i64 = std::int64_t;

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
	u64 size() const { return data.size(); }

	void push(T t) {
        data.push_back(t);
    }

	void pop() {
        data.pop_back();
    }

	T& top() { return data.back(); }
	T& at(u64 depth) { return data.at(size() - depth - 1); }
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

	TOKEN_EQUAL, TOKEN_NOT,

	TOKEN_PLUS, TOKEN_MINUS, TOKEN_DIVIDE, TOKEN_MULTIPLY,

    TOKEN_PLUS_EQUAL, TOKEN_MINUS_EQUAL, TOKEN_MULTIPLY_EQUAL, TOKEN_DIVIDE_EQUAL,

	TOKEN_EQUAL_EQUAL, TOKEN_NOT_EQUAL,
	TOKEN_LESS, TOKEN_GREATER, TOKEN_LESS_EQUAL, TOKEN_GREATER_EQUAL,


    TOKEN_AND, TOKEN_OR,

    TOKEN_DEFER, TOKEN_IMPORT,
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
    Token(TokenType type_, std::string text_) : type(type_), text(text_) {}
};

namespace std {
    std::string to_string(Token other) {
        return std::to_string(other.type) + ": " + "\'" + other.text + "\'" + '\n';
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

    { "defer", TOKEN_DEFER },
    { "import", TOKEN_IMPORT },
};

enum IntegerSizes : u8 {
    INTEGER_8BIT  = 0x00,
    INTEGER_16BIT = 0x01,
    INTEGER_32BIT = 0x02,
    INTEGER_64BIT = 0x03,
};

class Value;

// TODO: Turn this into a full class with more complex type's like functions with arg's and return types.
enum ValueType : u8 { 
    VALUE_NONE = 0x00,

    VALUE_INT,
    VALUE_REAL,
    VALUE_BOOL,
    VALUE_STR,
    VALUE_ID,

    VALUE_ANY
};

ValueType StringToValueType(std::string type)
{
    // NOTE: Cant convert string to array.
    if(type == "string") return VALUE_STR;
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
            case VALUE_STR: { return "string"; } break;
            case VALUE_BOOL: { return "bool"; } break;
            case VALUE_ID: { return "id"; } break;
            case VALUE_ANY: { return "any"; } break;
            case VALUE_NONE: { return "none";  } break;
        }
        return "NULL TYPE " + std::to_string((int)other);
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
                delete static_cast<i64*>(data);
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
            case VALUE_ANY: 
            {
                // do nothing
            } break;
            default:
            {
                LogError("No delete function for type: " + std::to_string(type));
            } break;
        }
    }

    void ParseStr(std::string text)
    {
        char b = text.front(), e = text.back();
        if(b == '"' && e == '"')
        {
            std::string r = text;
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
        data = new i64(std::stoll(text));
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

    i64* GetInt() const { return static_cast<i64*>(data); }
    double* GetReal() const { return static_cast<double*>(data); }
    std::string* GetStr() const { return static_cast<std::string*>(data); }
    bool* GetBool() const { return static_cast<bool*>(data); }
    std::string* GetId() const { return static_cast<std::string*>(data); }

    std::string GetStrUnqouted() const {
        std::string result = *static_cast<std::string*>(data);
        return result.substr(1, result.size() - 2);
    }

    u64 GetSize() const {
        u64 r = 0;
        switch (type) {
            case VALUE_INT: r = sizeof(u64); break;
            case VALUE_REAL: r = sizeof(double); break;
            case VALUE_BOOL: r = sizeof(bool); break;
            case VALUE_STR: r = sizeof(char) * GetStr()->size(); break;
            case VALUE_ID: r = sizeof(char) * GetId()->size(); break;
            default:
            {
                LogError("Cannot get size of type: \'" + std::to_string(type) + "\'");
            } break;
        }
        return r;
    }

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

    void Not()
    {
        if(IsBool())
        {
            *GetBool() = !*GetBool();
        }
        else
        {
            LogError("Cannot use '!' on type '" + GetTypeAsString() + "'");
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
                data = new i64(*other.GetInt());
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

	Value(i64 other)
	{
		type = VALUE_INT;
		data = new i64(other);
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
                data = new i64(0);
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

	Value& operator=(const Value& other)
    {
        FreeData();
        LinkToken(other.linked);
        type = other.type;

        switch(type)
        {
            case VALUE_INT:
            {
                data = new i64(*other.GetInt());
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
        FreeData();
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

	Value& operator=(i64 other)
	{
        FreeData();
		type = VALUE_INT;
		data = new i64(other);
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
                data = new i64(0);
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
    std::string to_string(Value& other) {
        switch (other.GetType())
        {
            case VALUE_INT: { return std::to_string(*other.GetInt()); } break;
            case VALUE_REAL: { return std::to_string(*other.GetReal()); } break;
            case VALUE_STR: { return *other.GetStr(); } break;
            case VALUE_BOOL: { return (std::string)(*other.GetBool() ? "true" : "false"); } break;
            case VALUE_ID: { return *other.GetId() ; } break;
        }
        return "";
    }
}

TokenType GetKeyword(std::string text)
{
	auto it = keywords.find(text);
	if (it != keywords.end()) return it->second;
	return TOKEN_IDENTIFIER;
}

std::string QuoteStr(std::string str) {
    return '"' + str + '"';
}

class Program;
Program ToProgram(i64 value);
Program ToProgram(double value);
Program ToProgram(bool value);
Program ToProgram(std::string value);
Program ToProgram(Value v);

class Program {
private:
    using Stream = std::vector<u8>;
    Stream stream;
public:
    auto data() { return stream.data(); }
    auto begin() { return stream.begin(); }
    auto end() { return stream.end(); }

    void push(u8 byte) {
        stream.push_back(byte);
    }

    void push(Program other) {
        stream.insert(end(), other.begin(), other.end());
    }

    void push(Value val) {
        Program p = ToProgram(val);
        stream.insert(end(), p.begin(), p.end());
    }

    u8& at(u64 idx) {
        return stream.at(idx);
    }

    u64 size() const {
        return stream.size();
    }

    void replace(u64 idx, Program other) {
        std::copy(other.begin(), other.end(), begin() + idx);
    }

    void replace(u64 idx, Value val) {
        Program other = ToProgram(val);
        replace(idx, other);
    }

    Program subprogram(u64 start, u64 size) {
        Program result;
        result.stream = Stream(stream.begin() + start, stream.begin() + start + size);
        return result;
    }
};

Program ToProgram(Value v) {
    Program result;

    result.push(v.GetType());

    switch(v.GetType()) {
        case VALUE_INT: { result.push(ToProgram(*v.GetInt())); } break;
        case VALUE_REAL: { result.push(ToProgram(*v.GetReal())); } break;
        case VALUE_STR: { result.push(ToProgram(*v.GetStr())); } break;
        case VALUE_BOOL: { result.push(ToProgram(*v.GetBool())); } break;
        case VALUE_ID: { result.push(ToProgram(*v.GetStr())); } break;
    }

    return result;
}

Program ToProgram(i64 value) {
    u8 type = INTEGER_64BIT; 
    u8 sign = 1;

    u8 properties = (sign << 2) | type;
    
    Program result;
    result.push(properties);

    result.push((u8)((value >>  0) & 0xff));
    result.push((u8)((value >>  8) & 0xff));
    result.push((u8)((value >> 16) & 0xff));
    result.push((u8)((value >> 24) & 0xff));
    result.push((u8)((value >> 32) & 0xff));
    result.push((u8)((value >> 40) & 0xff));
    result.push((u8)((value >> 48) & 0xff));
    result.push((u8)((value >> 56) & 0xff));

    return result;
}

Program ToProgram(double fvalue) {
    Program result;

    u64 value = reinterpret_cast<u64&>(fvalue);
    result.push((u8)((value >>  0) & 0xff));
    result.push((u8)((value >>  8) & 0xff));
    result.push((u8)((value >> 16) & 0xff));
    result.push((u8)((value >> 24) & 0xff));
    result.push((u8)((value >> 32) & 0xff));
    result.push((u8)((value >> 40) & 0xff));
    result.push((u8)((value >> 48) & 0xff));
    result.push((u8)((value >> 56) & 0xff));
    return result;
}

Program ToProgram(bool value) {
    Program result;
    result.push(value);
    return result;
}

Program ToProgram(std::string value) {
    Program result = ToProgram(Value((i64)value.size()));
    for(int i = 0; i < value.size(); i++) {
        char c = value.at(i);
        result.push(c);
    }

    return result;
}

using TokenStream = std::vector<Token>;
class Lexer
{
public:
	TokenStream stream;
private:
	std::string source;

	int location = 0;

	void advance()
	{
		if (location + 1 > source.size())
		{
			return;
		}

		location++;
	}

	char peek()
	{
		if (location + 1 > source.size())
		{
			return ' ';
		}

		return source.at(location + 1);
	}

	bool match(char c)
	{
		if (peek() == c)
		{
			advance();
			return true;
		}

		return false;
	}

	char GetCurrent()
	{
		return source.at(location);
	}

    void eval() {
        int line = 1;
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

            case '+': { token.type = match('=') ? TOKEN_PLUS_EQUAL : TOKEN_PLUS; } break;
			case '-': { token.type = match('=') ? TOKEN_MINUS_EQUAL : TOKEN_MINUS; } break;
			case '*': { token.type = match('=') ? TOKEN_MULTIPLY_EQUAL : TOKEN_MULTIPLY; } break;
			case '/':
			{
				if (peek() == '/')
				{
					while (peek() != '\n')
					{
						advance();
					}

					line++;
					continue;
				}
                else if(peek() == '*')
                {
                    advance();
                    advance();

                    int nested = 1;
                    while(nested > 0)
                    {
                        if(GetCurrent() == '\n')
                        {
                            line++;
                        }

                        if(GetCurrent() == '/' && peek() == '*')
                        {
                            nested++;
                        }
                        else if(GetCurrent() == '*' && peek() == '/')
                        {
                            nested--;
                        }

                        advance();
                    }

                    continue;
                }
				else
				{
					token.type = match('=') ? TOKEN_DIVIDE_EQUAL : TOKEN_DIVIDE;
				}
			} break;

			case '=':
			{
				token.type = match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL;
			} break;
			case '!':
			{
				token.type = match('=') ? TOKEN_NOT_EQUAL : TOKEN_NOT_EQUAL;
			} break;
			case '<':
			{
				token.type = match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS;
			} break;
			case '>':
			{
				token.type = match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER;
			} break;

            case '&':
            {
				if(match('&')) { token.type = TOKEN_AND; break; }
            }
            case '|':
            {
				if(match('|')) { token.type = TOKEN_OR; break; }
            }

            case '"':
            {
                do
                {
                    token.size++;
                    advance();
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
					while (IsNumber(peek()) || peek() == '.')
					{
						token.size++;
						advance();
					}

					token.text = source.substr(token.location, token.size);
					token.type = TOKEN_LITERAL;
				}
				else if (IsLetter(c))
				{
					while (IsLetter(peek()) || IsNumber(peek()) || peek() == '_')
					{
						token.size++;
						advance();
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
struct IdentifierNode;
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
struct DeferNode;
struct ImportNode;

struct NodeVisitor
{
	virtual void visit(ScopeNode* node) = 0;
	virtual void visit(IdentifierNode* node) = 0;
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
	virtual void visit(DeferNode* node) = 0;
    virtual void visit(ImportNode* node) = 0;
};

#define PRINT_FREED_MEMORY false
struct Node
{
    u64 line, location;

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

struct IdentifierNode : Node
{
    std::string id;

	IdentifierNode(std::string id_)
		: id(id_)
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

    ValueType type = VALUE_NONE;

	VISIT_
};

struct VariableDeclNode : Node
{
    Node *id, *type, *value;
    bool infer;

    VariableDeclNode() = default;

    static VariableDeclNode* CreateInferred(Node* id_, Node* value_) {
        VariableDeclNode* result = new VariableDeclNode();

        result->id = id_;
        result->type = NULL;
        result->value = value_;
        result->infer = true;

        return result;
    }

	VariableDeclNode(Node* id_, Node* type_, Node* value_ = NULL)
		: id(id_), type(type_), value(value_), infer(false)
	{}
    
	VISIT_
};

struct AssignmentNode : Node
{
    Node *id, *value;

	AssignmentNode(Node* id_, Node* value_)
		: id(id_), value(value_)
	{}

	VISIT_
};

struct FunctionNode : Node
{
	Node *id, *returntype, *block;
    std::vector<Node*> args;

	FunctionNode(Node* id_, Node* returntype_, std::vector<Node*> args_, Node* block_)
		: id(id_), returntype(returntype_), args(args_), block(block_)
	{}

	VISIT_
};

struct CallNode : Node
{
	Node* id;
    std::vector<Node*> args;

	CallNode(Node* id_, std::vector<Node*> args_)
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
    Node * expr;

    ReturnNode(Node* expr_)
        : expr(expr_)
    {}

    VISIT_
};

struct BreakNode : Node {
    VISIT_
};

struct ContinueNode : Node {
    VISIT_
};

struct DeferNode : Node {
    Node* stat;

    DeferNode(Node* stat_) : stat(stat_) {}
    VISIT_
};

struct ImportNode : Node {
    Token file;
    Node* path;

    ImportNode(Token file_, Node* path_) : file(file_), path(path_) {}
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

	void advance(int n = 1)
	{
		if (location < stream.size() - n)
		{
            location += n;
            current = stream.at(location);
		}
	}

    Token peek()
    {
        if(location < stream.size() - 1)
            return stream.at(location + 1);

        return Token(TOKEN_NONE);
    }

    bool AdvanceIfMatch(TokenType match)
    {
        if(current.type == match)
        {
            advance();
            return true;
        }

        return false;
    }

	Parser(TokenStream input)
		: stream(input)
	{
        current = stream.at(0);
		tree = new ScopeNode(StatementList());
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
        Token t = current;
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
            if(Node* block = StatementBlock(); block != NULL)
            {
                return new LoopNode(block);
            }
            else
            {
                LogError("Loop must contain a valid statement block", current.line, current.location);
            }
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
                    Node* incr = VariableAssignment();

                    if(Node* block = StatementBlock(); block != NULL)
                    {
                        return new ForNode(init, cond, incr, block);
                    }
                    else
                    {
                        LogError("For loop must contain a valid statement block", current.line, current.location);
                    }
                }
            }
        }
		else if (AdvanceIfMatch(TOKEN_FUNC))
		{
			Token id = current;
			advance();

			if (AdvanceIfMatch(TOKEN_LEFT_PARENTHESIS))
			{
				auto args_decl = ArgumentListDecleration();

				if (AdvanceIfMatch(TOKEN_RIGHT_PARENTHESIS))
				{
					Token returntype;
					if (AdvanceIfMatch(TOKEN_COLON))
					{
						returntype = current;
                        advance();
					}

					Node* block = StatementBlock();

					return new FunctionNode(new IdentifierNode(id.text), new IdentifierNode(returntype.text), args_decl, block);
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
                return new BreakNode();
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
                return new ContinueNode();
            }
            else
            {
                LogError("Break must end with a semicolon", current.line, current.location);
            }
        }
        else if(AdvanceIfMatch(TOKEN_DEFER))
        {
            if(Node* stat = Statement(); stat != NULL) {
                return new DeferNode(stat);
            }
            else
            {
                LogError("Defer statement must have a valid statement", current.line, current.location);
            }
        }
        else if (AdvanceIfMatch(TOKEN_IMPORT)) {
            Token file = current;
            if (Node* v = Literal(); v != NULL) {
                if (AdvanceIfMatch(TOKEN_SEMICOLON)) {
                    return new ImportNode(file, v);
                }
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
            if(peek().type == TOKEN_COLON)
            {
                advance(2);

                Node* type = Type();

                if(AdvanceIfMatch(TOKEN_EQUAL))
                {
                    Node* value = Expression();
                    if(type != NULL)
                    {
                        return new VariableDeclNode(new IdentifierNode(id.text), type, value);
                    }
                    else
                    {
                        return VariableDeclNode::CreateInferred(new IdentifierNode(id.text), value);
                    }
                }
                else
                {
                    return new VariableDeclNode(new IdentifierNode(id.text), type);
                }
            }
        }

        return NULL;
    }

    Node* VariableAssignment()
    {
        if(Token id = current; id.type == TOKEN_IDENTIFIER)
        {
            // TODO: Implement array indexing e.g. names[5] = "James";
            if(peek().type == TOKEN_EQUAL)
            {
                advance(2);
                return new AssignmentNode(new IdentifierNode(id.text), Expression());
            }
            else if(peek().type == TOKEN_PLUS_EQUAL || 
                    peek().type == TOKEN_MINUS_EQUAL ||
                    peek().type == TOKEN_MULTIPLY_EQUAL ||
                    peek().type == TOKEN_DIVIDE_EQUAL)
            {
                Token op = peek();
                advance(2);

                switch(op.type)
                {
                    case TOKEN_PLUS_EQUAL: op.type = TOKEN_PLUS; break;
                    case TOKEN_MINUS_EQUAL: op.type = TOKEN_MINUS; break;
                    case TOKEN_MULTIPLY_EQUAL: op.type = TOKEN_MULTIPLY; break;
                    case TOKEN_DIVIDE_EQUAL: op.type = TOKEN_DIVIDE; break;
                }

                Node* binary = new BinaryNode(new ValueNode(id), op, Expression());
                return new AssignmentNode(new IdentifierNode(id.text), binary);
            }
        }

        return NULL;
    }

    Node* Identifier() {
        // TODO: Implement this.
        if(Token id = current; AdvanceIfMatch(TOKEN_IDENTIFIER))
        {
            if(AdvanceIfMatch(TOKEN_LEFT_BRACKET))
            {
                // NOTE: Is array
                Node* size = Expression();
                if(AdvanceIfMatch(TOKEN_RIGHT_BRACKET))
                {

                }
            }
        }

        return NULL;
    }

    Node* Type() {
        if(Token type = current; AdvanceIfMatch(TOKEN_IDENTIFIER))
        {
            return new IdentifierNode(type.text);
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
                    LogError("Argument decleration must precede a comma", current.line, current.location);
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
            if(peek().type == TOKEN_LEFT_PARENTHESIS)
            {
                Token id = current;
                advance(2);

                auto arg_list = ArgumentList();

                if(current.type == TOKEN_RIGHT_PARENTHESIS)
                {
                    advance();
                    return new CallNode(new IdentifierNode(id.text), arg_list);
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
            advance();
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
            t = peek();
            if(t.type == TOKEN_LITERAL)
            {
                advance(2);
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

            if(AdvanceIfMatch(TOKEN_LEFT_BRACKET)) {
                Token idx = current;
                if(idx.type == TOKEN_LITERAL) {
                    if(AdvanceIfMatch(TOKEN_RIGHT_BRACKET)) {
                        return new ValueNode(t/*, idx*/);
                    }
                }
            }

            advance();
            return new ValueNode(t, VALUE_ID);
        }

        return NULL;
	}
};

struct FunctionData {
    ValueType returntype;
    std::vector<ValueType> args;

    FunctionData() = default;

    FunctionData(ValueType returntype_, std::vector<ValueType> args_)
        : returntype(returntype_), args(args_)
    {}
};

struct ScopedSymbolTable {
    ScopedSymbolTable* parent = NULL;

    ScopedSymbolTable(ScopedSymbolTable* parent_ = NULL)
        : parent(parent_)
    {}

	std::map<std::string, ValueType> typetable;

	void SetType(std::string name, ValueType type)
	{
		typetable[name] = type;
	}

	ValueType FindType(std::string name)
	{
		auto it = typetable.find(name);
		if (it != typetable.end()) return it->second;

		if (parent != NULL) return parent->FindType(name);
		return VALUE_NONE;
	}

    bool VariableExists(std::string name) {
        auto it = typetable.find(name);
        if (it != typetable.end()) return true;

		if (parent != NULL) return parent->VariableExists(name);

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
public:
    ScopedSymbolTable global;
private:
    ScopedSymbolTable *current;
    Stack<Value> stack;
    Stack<Node*> deferstack;

    ValueType assignmentType = VALUE_NONE;

    bool is_arg = false;

    ValueType returntype = VALUE_NONE;
    bool got_return = false;
    bool in_loop = false;
    bool in_func = false;

    std::string sourcepath;

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
    TypeChecker(std::string sourcepath_)
        : sourcepath(sourcepath_)
    {
        current = &global;
    }

    ~TypeChecker()
    {
        while(current->parent != NULL)
        {
            DeleteCurrent();
        }
    }

	void visit(ScopeNode* node)
    {
        NewCurrent();
        node->statement->visit(this);

        while(!deferstack.empty()) {
            deferstack.top()->visit(this);
            deferstack.pop();
        }

        DeleteCurrent();
    }

	void visit(SequenceNode* node)
	{
		for (auto&& n : node->lst)
		{
			n->visit(this);
		}
	}

    void visit(IdentifierNode* node) {
        stack.push(Value(VALUE_ID, node->id));
    }

	void visit(BinaryNode* node)
	{
		node->left->visit(this);
		node->right->visit(this);

        Value r = stack.top(); stack.pop();
        Value l = stack.top(); stack.pop();

        if(l.IsStr() && r.IsStr()) {
            switch(node->op.type)
            {
                case TOKEN_PLUS:
                case TOKEN_EQUAL_EQUAL:
                case TOKEN_NOT_EQUAL:
                {

                } break;
                default:
                {
                    LogError("Invalid operator operation on a string", l.GetLine(), l.GetLoc());
                } break;
            }
        }
        else if (l.GetType() != r.GetType()) {
            LogError("Cannot perform a binary operation on \'" + std::to_string(l.GetType()) + "\' and \'" + std::to_string(r.GetType()) + "\'", node->line, node->location);
        }

        if(!(node->op.type < TOKEN_EQUAL_EQUAL || node->op.type > TOKEN_OR))
        {
            l = true;
        }

        stack.push(l);
	}

	void visit(ValueNode* node)
    {
        Value v(node->token);

        if(v.GetType() == VALUE_ID) {
            if(current->VariableExists(node->token.text)) {
                v.GetType() = current->FindType(node->token.text);
            } else {
                LogError("Variable '" + node->token.text + "' does not exist within the current scope", node->token.line, node->token.location);
            }
        }

        if(assignmentType != VALUE_NONE) {
            assignmentType = v.GetType();
        }

        stack.push(v);
    }

	void visit(VariableDeclNode* node)
	{
        node->id->visit(this);
        Value id = stack.top(); stack.pop();
        if(current->VariableExists(*id.GetId())) {
            LogError("Variable already exist's with name '" + *id.GetId() + "' within the current scope", node->line, node->location);
            return;
        }

        ValueType vartype = VALUE_NONE;
        if(node->infer) {
            node->value->visit(this);
            Value value = stack.top(); stack.pop();
            vartype = value.GetType();
            node->type = new IdentifierNode(value.GetTypeAsString());
        }
        else {
            if(node->value != NULL)
            {
                node->value->visit(this);
                node->type->visit(this);
                vartype = stack.top().GetType(); stack.pop();
                ValueType valuetype = stack.top().GetType(); stack.pop();
                if(vartype != valuetype) {
                    LogError("Cannot assign '" + std::to_string(vartype) + "' to '" + std::to_string(valuetype) + "'", node->line, node->location);
                }
            }
            else {
                node->type->visit(this);
                vartype = stack.top().GetType();
            }
        }

        current->SetType(*id.GetId(), vartype);
        if(is_arg) stack.push(vartype);
	}

	void visit(AssignmentNode* node)
	{
        node->id->visit(this);
        Value id = stack.top(); stack.pop();
        if(!current->VariableExists(*id.GetId())) {
            LogError("Cannot assign to '" + *id.GetId() + "' since it doesn't exist within the current scope", node->line, node->location);
            return;
        }

        Value var = current->FindType(*id.GetId());
        node->value->visit(this);
        Value val = stack.top(); stack.pop();
        if(var.GetType() != val.GetType()) {
            LogError("Cannot assign '" + val.GetTypeAsString() + "' to '" + var.GetTypeAsString() + "'", node->line, node->location);
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

        node->id->visit(this);
        Value id = stack.top(); stack.pop();
        
        node->returntype->visit(this);
        Value returnval = stack.top(); stack.pop();

        current->parent->SetFunction(*id.GetId(), FunctionData(StringToValueType(*returnval.GetId()), args));

        got_return = false;

        in_func = true;
        node->block->visit(this);
        in_func = false;

        if(returnval.GetType() != VALUE_NONE && !got_return) {
            LogError("Function '" + *id.GetId() + "' expect's an '" + *returnval.GetId() + "' to be returned, but never get's one.");
            // TODO: Check if all code path's return a value.
        }

        DeleteCurrent();
	}

	void visit(CallNode* node)
	{
        node->id->visit(this);
        Value id = stack.top(); stack.pop();

        auto func = current->FindFunction(*id.GetId());
        if(func)
        {
            if(node->args.size() == func->args.size())
            {
                for(int i = 0; i < node->args.size(); i++)
                {
                    node->args[i]->visit(this);
                    Value given = stack.top(); stack.pop();
                    Value declared = func->args.at(i);

                    if(declared.GetType() == VALUE_ANY) {
                    }
                    else {
                        if(declared.GetType() != given.GetType()) {
                            LogError("Argument " + std::to_string(i + 1) + " expect '" + declared.GetTypeAsString() + "' but got '" + given.GetTypeAsString() + "'", node->line, node->location);
                        }
                    }
                }

                stack.push(func->returntype);
            }
            else
            {
                LogError("Invalid arg's size: Expected '" + std::to_string(func->args.size()) + "'", node->line, node->location);
            }
        }
        else
        {
            LogError("Function '" + *id.GetId() + "' does not exist within the current scope", node->line, node->location);
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

        in_loop = true;
		node->block->visit(this);
        in_loop = false;
        DeleteCurrent();
	}

	void visit(LoopNode* node)
	{
        in_loop = true;
		node->block->visit(this);
        in_loop = false;
	}

	void visit(ReturnNode* node)
    {
        if(!in_func)
        {
            LogError("Return statement must be in a function", node->line, node->location);
        }

        ValueType rtype = VALUE_NONE;
        if(node->expr != NULL) {
             node->expr->visit(this);
             rtype = stack.top().GetType(); stack.pop();
        }

        if(returntype != rtype)
        {
            LogError("Returned type '" + std::to_string(rtype) + "' is different as expected '" + std::to_string(returntype) + "'", node->line, node->location);
        }

        got_return = true;
    }

	void visit(BreakNode* node)
    {
        if(!in_loop)
        {
            LogError("Break statement must be in a loop", node->line, node->location);
        }
    }

	void visit(ContinueNode* node)
    {
        if(!in_loop)
        {
            LogError("Continue statement must be in a loop", node->line, node->location);
        }
    }

	void visit(DeferNode* node)
    {
        deferstack.push(node->stat);
    }

    void visit(ImportNode* node) {
        node->path->visit(this);
        Value filePath = stack.top(); stack.pop();

        if (!filePath.IsStr()) {
            LogError("Import statement must be provided a string", node->file.line, node->file.location);
            return;
        }

        std::filesystem::path path(*filePath.GetStr());
        if (!std::filesystem::exists(path)) {
            LogError("Import statement path points to a file that does not exist", node->file.line, node->file.location);
            return;
        }

        if (std::filesystem::is_directory(path)) {
            LogError("Import statement path cannot be a directory, it must be a file", node->file.line, node->file.location);
            return;
        }

        if (path == sourcepath) {
            LogError("Cannot import file within itself", node->file.line, node->file.location);
            return;
        }
    }
};

struct NodeFreeVisitor : NodeVisitor {
	void visit(ScopeNode* node)
    {
        node->statement->visit(this);
        delete node->statement;
    }

    void visit(IdentifierNode* node) {}

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
        delete node->id;

        if(node->type != NULL)
        {
            node->type->visit(this);
            delete node->type;
        }

        if(node->value != NULL)
        {
            node->value->visit(this);
            delete node->value;
        }
    }

	void visit(AssignmentNode* node)
	{
        delete node->id;
        node->value->visit(this);
        delete node->value;
	}

	void visit(FunctionNode* node)
	{
        delete node->id;

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
        delete node->id;

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

	void visit(DeferNode* node)
    {
        node->stat->visit(this);
        delete node->stat;
    }

    void visit(ImportNode* node) 
    {
        delete node->path;
    }
};

enum OpcodeType : u8
{
	OPCODE_NONE = 0x00,

	OPCODE_PUSH, OPCODE_POP,
	OPCODE_STORE, OPCODE_LOAD,

	OPCODE_JUMP, OPCODE_JUMP_IF_TRUE, OPCODE_JUMP_IF_FALSE,
    OPCODE_START_SUBROUTINE, OPCODE_JUMP_SUBROUTINE, OPCODE_RETURN_FROM_SUBROUTINE,

	OPCODE_ADD, OPCODE_MINUS, OPCODE_MULTIPLY, OPCODE_DIVIDE,

    OPCODE_COMP_LESS, OPCODE_COMP_GREATER,
    OPCODE_COMP_LESS_EQUAL, OPCODE_COMP_GREATER_EQUAL,
    OPCODE_COMP_EQUAL, OPCODE_COMP_NOT_EQUAL,

    OPCODE_COMP_AND, OPCODE_COMP_OR,

    OPCODE_START_DEFER, OPCODE_EXEC_DEFER,

    OPCODE_MAX = 0xff
};

std::map<OpcodeType,u64> OpcodeArgTable = {
    { OPCODE_PUSH, 1 },
    { OPCODE_POP, 0 },
    { OPCODE_STORE, 1 },
    { OPCODE_LOAD, 1 },
    { OPCODE_JUMP, 1 },
    { OPCODE_JUMP_IF_TRUE, 1 },
    { OPCODE_JUMP_IF_FALSE, 1 },
    { OPCODE_START_SUBROUTINE, 2 },
    { OPCODE_JUMP_SUBROUTINE, 1 },
    { OPCODE_RETURN_FROM_SUBROUTINE, 0 },
    { OPCODE_ADD, 0 },
    { OPCODE_MINUS, 0 },
    { OPCODE_MULTIPLY, 0 },
    { OPCODE_DIVIDE, 0 },
    { OPCODE_COMP_LESS, 0 },
    { OPCODE_COMP_GREATER, 0 },
    { OPCODE_COMP_LESS_EQUAL, 0 },
    { OPCODE_COMP_GREATER_EQUAL, 0 },
    { OPCODE_COMP_EQUAL, 0 },
    { OPCODE_COMP_NOT_EQUAL, 0 },
    { OPCODE_COMP_AND, 0 },
    { OPCODE_COMP_OR, 0 },

    { OPCODE_START_DEFER, 1 },
    { OPCODE_EXEC_DEFER, 1 },
};

u64 GetOpcodeArgC(OpcodeType op) {
    auto it = OpcodeArgTable.find(op);
    if(it != OpcodeArgTable.end()) return it->second;
    return -1;
}

struct Environment
{
    struct Variable {
        std::string name;
        Value value;
        u64 depth;

        Variable(std::string name_, Value value_, u64 depth_)
            : name(name_), value(value_), depth(depth_)
        {}
    };

    Stack<Variable> varstack;
    u64 depth = 0;

    void IncrementDepth() {
        depth++;
    }

    void DecrementDepth() {
        depth--;

        if(depth > 0) {
            Variable current = varstack.top();
            while(current.depth > depth)
            {
                varstack.pop();
                current = varstack.top();
            }
        }
    }

    void CreateIdentifier(std::string name) {
        varstack.push(Variable(name, (i64)0, depth));
    }

	void SetIdentifier(std::string name, Value value) {
        for(int i = 0; i < varstack.size(); i++)
        {
            Variable& var = varstack.at(i);
            if(var.depth > depth) continue;

            if(var.name == name) {
                var.value = value;
                break;
            }
        }
	}

	std::optional<Value> FindIdentifier(std::string name) {
        for(int i = 0; i < varstack.size(); i++)
        {
            Variable& var = varstack.at(i);
            if(var.depth > depth) continue;

            if(var.name == name) {
                return var.value;
            }
        }

		return {};
	}
};

class VirtualMachine
{
private:
    using IntrinsicFunction = std::function<void(VirtualMachine&)>;

    Stack<u64> callStack;
    Stack<Program> deferstack;

    std::map<std::string,u64> jumpTable;

	Environment env;

    std::map<std::string,IntrinsicFunction> intrinsics;
public:
	Stack<Value> stack;

    void RegisterIntrinsic(std::string id, IntrinsicFunction func) {
        intrinsics.emplace(id, func);
    }

    void CallIntrinsic(std::string id) {
        auto it = intrinsics.find(id);
        if(it != intrinsics.end()) {
            std::invoke(it->second, *this);
        }
        else {
            LogError("Cannot call function '" + id + "' as it doesn't exist");
        }
    }

    void RunProgram(Program& p) {
        for (u64 pc = 0; pc < p.size();)
        {
            exec(p, pc);
        }
    }

    void PrintGlobalSymbolTable()
    {
        if(env.varstack.empty()) return;

        std::cout << "--- FINAL IDENTIFIER TABLE --- \n";
        std::cout << "IDENTIFIERS - VALUE\n";
        for(int i = 0; i < env.varstack.size(); i++)
        {
            Environment::Variable& var = env.varstack.at(i);
            if(var.depth > 0) continue;
            std::cout << var.name << ": " << std::to_string(var.value) << '\n';
        }
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

    u8 GetByte(Program& p, u64& pc) { return p.at(pc++); }

    Value decodeConstant(Program& p, u64& pc) {
        u8 type = GetByte(p, pc);
        switch(type) {
            case VALUE_INT:
            {
                u8 properties = GetByte(p, pc);

                u8 size = properties & 0b00000011;
                u8 sign = (properties >> 2) & 0b00000001;

                switch(size)
                {
                    case INTEGER_8BIT:
                    {
                        u8 finalvalue = GetByte(p, pc);
                    } break;
                    case INTEGER_16BIT:
                    {
                        u8 bytes8[2] = {
                            GetByte(p, pc),
                            GetByte(p, pc)
                        };

                        u16 finalvalue = (u16)((bytes8[1] << 8) | bytes8[0]);
                    } break;
                    case INTEGER_32BIT:
                    {
                        u8 bytes8[4] = {
                            GetByte(p, pc),
                            GetByte(p, pc),
                            GetByte(p, pc),
                            GetByte(p, pc)
                        };

                        u16 bytes16[4] = {
                            (u16)((bytes8[1] << 8) | bytes8[0]),
                            (u16)((bytes8[3] << 8) | bytes8[2])
                        };

                        u32 finalvalue = (u32)((bytes16[1] << 16) | bytes16[0]);
                    } break;
                    case INTEGER_64BIT:
                    {
                        u8 bytes8[8] = {
                            GetByte(p, pc),
                            GetByte(p, pc),
                            GetByte(p, pc),
                            GetByte(p, pc),
                            GetByte(p, pc),
                            GetByte(p, pc),
                            GetByte(p, pc),
                            GetByte(p, pc)
                        };

                        u16 bytes16[4] = {
                            (u16)((bytes8[1] << 8) | bytes8[0]),
                            (u16)((bytes8[3] << 8) | bytes8[2]),
                            (u16)((bytes8[5] << 8) | bytes8[4]),
                            (u16)((bytes8[7] << 8) | bytes8[6])
                        };

                        u32 bytes32[2] = {
                            (u32)((bytes16[1] << 16) | bytes16[0]),
                            (u32)((bytes16[3] << 16) | bytes16[2])
                        };

                        u64 finalValue = (((u64)bytes32[1] << 32) | bytes32[0]);

                        return Value((i64)finalValue);
                    } break;
                }
            } break;
            case VALUE_REAL:
            {
                u8 bytes8[8] = {
                    GetByte(p, pc),
                    GetByte(p, pc),
                    GetByte(p, pc),
                    GetByte(p, pc),
                    GetByte(p, pc),
                    GetByte(p, pc),
                    GetByte(p, pc),
                    GetByte(p, pc)
                };

                u16 bytes16[4] = {
                    (u16)((bytes8[1] << 8) | bytes8[0]),
                    (u16)((bytes8[3] << 8) | bytes8[2]),
                    (u16)((bytes8[5] << 8) | bytes8[4]),
                    (u16)((bytes8[7] << 8) | bytes8[6])
                };

                u32 bytes32[2] = {
                    (u32)((bytes16[1] << 16) | bytes16[0]),
                    (u32)((bytes16[3] << 16) | bytes16[2])
                };

                u64 finalValue = (((u64)bytes32[1] << 32) | bytes32[0]);

                return Value(reinterpret_cast<double&>(finalValue));
            } break;
            case VALUE_BOOL:
            {
                return Value((bool)GetByte(p, pc));
            } break;
            case VALUE_STR:
            {
                Value size = decodeConstant(p, pc);
                std::string str;
                for(int i = 0; i < *size.GetInt(); i++) {
                    str.append(1, GetByte(p, pc));
                }

                return Value((ValueType)type, str);
            } break;
            case VALUE_ID:
            {
                Value size = decodeConstant(p, pc);
                std::string id;
                for(int i = 0; i < *size.GetInt(); i++) {
                    id.append(1, GetByte(p, pc));
                }

                return Value((ValueType)type, id);
            } break;

            default:
            {
                LogError("Invalid constant type: " + std::to_string(type));
            } break;
        }

        return{};
    }

	void exec(Program& p, u64& pc)
	{
        u8 opcode = GetByte(p, pc);
        switch (opcode)
        {
            case OPCODE_PUSH:
            {
                stack.push(decodeConstant(p, pc));
            } break;
            case OPCODE_POP:
            {
                stack.pop();
            } break;

            case OPCODE_STORE:
            {
                Value v = stack.top();
                stack.pop();

                Value arg = decodeConstant(p, pc);
                std::string name = *arg.GetId();

                auto id = env.FindIdentifier(name);
                if(id) {
                    env.SetIdentifier(name, v);
                }
                else {
                    env.CreateIdentifier(name);
                    env.SetIdentifier(name, v);
                }
            } break;
            case OPCODE_LOAD:
            {
                Value arg = decodeConstant(p, pc);
                std::string name = *arg.GetId();
                auto id = env.FindIdentifier(name);
                if (id)
                {
                    stack.push(*id);
                }
            } break;

            case OPCODE_JUMP:
            {
                Value arg = decodeConstant(p, pc);
                pc = *arg.GetInt();
            } break;
            case OPCODE_JUMP_IF_TRUE:
            {
                Value arg = decodeConstant(p, pc);
                Value v = stack.top(); stack.pop();
                if(v.IsBool()) {
                    if(*v.GetBool())
                    {
                        pc = *arg.GetInt();
                    }
                }
            } break;
            case OPCODE_JUMP_IF_FALSE:
            {
                Value arg = decodeConstant(p, pc);
                Value v = stack.top(); stack.pop();
                if(v.IsBool()) {
                    if(!*v.GetBool())
                    {
                        pc = *arg.GetInt();
                    }
                }
            } break;

            case OPCODE_START_SUBROUTINE:
            {
                Value id = decodeConstant(p, pc);
                Value skip = decodeConstant(p, pc);
                jumpTable.emplace(*id.GetId(), pc);
                pc = *skip.GetInt();
            } break;
            case OPCODE_JUMP_SUBROUTINE:
            {
                Value arg = decodeConstant(p, pc);
                
                if(auto it = jumpTable.find(*arg.GetId()); it != jumpTable.end())
                {
                    env.IncrementDepth();

                    callStack.push(pc);
                    pc = it->second;
                }
                else
                {
                    CallIntrinsic(*arg.GetId());
                }
            } break;
            case OPCODE_RETURN_FROM_SUBROUTINE:
            {
                pc = callStack.top(); callStack.pop();
                env.DecrementDepth();
            } break;

            case OPCODE_ADD:
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
                else if(l.IsStr() && r.IsStr()) {
                    Value result = QuoteStr(l.GetStrUnqouted() + r.GetStrUnqouted());
                    stack.push(result);
                }
            } break;
            case OPCODE_MINUS:
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
            case OPCODE_MULTIPLY:
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
            case OPCODE_DIVIDE:
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

            case OPCODE_COMP_LESS:
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
            case OPCODE_COMP_GREATER:
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
            case OPCODE_COMP_LESS_EQUAL:
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
            case OPCODE_COMP_GREATER_EQUAL:
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
            case OPCODE_COMP_EQUAL:
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
                else if(l.IsStr()) {
                    Value result = (bool)(*l.GetStr() == *r.GetStr());
                    stack.push(result);
                }
            } break;
            case OPCODE_COMP_NOT_EQUAL:
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
                else if(l.IsStr()) {
                    Value result = (bool)(*l.GetStr() != *r.GetStr());
                    stack.push(result);
                }
            } break;

            case OPCODE_COMP_AND:
            {
                Value r = stack.top(); stack.pop();
                Value l = stack.top(); stack.pop();

                Value result = (bool)(*l.GetBool() && *r.GetBool());
                stack.push(result);
            } break;
            case OPCODE_COMP_OR:
            {
                Value r = stack.top(); stack.pop();
                Value l = stack.top(); stack.pop();

                Value result = (bool)(*l.GetBool() || *r.GetBool());
                stack.push(result);
            } break;

            case OPCODE_START_DEFER:
            {
                u64 current_pc = pc;
                Value size = decodeConstant(p, pc);
                Program subprogram = p.subprogram(pc, *size.GetInt() - (pc - current_pc));
                deferstack.push(subprogram);
                pc = current_pc + *size.GetInt();
            } break;
            case OPCODE_EXEC_DEFER:
            {
                Value size = decodeConstant(p, pc);

                if(!deferstack.empty()) {
                    while((*size.GetInt())--) {
                        Program s = deferstack.top(); deferstack.pop();
                        RunProgram(s);
                    }
                }
            } break;

            default:
            {
                LogError("Invalid opcode type: " + std::to_string(opcode));
            } break;
        }
	}
    
    void PrintProgram(Program& p) {
        u64 pc = 0;

        u64 programsize = sizeof(p) + (sizeof(p.at(0)) * p.size());
        std::cout << " --- Bytecode Program, Size(bytes): " << programsize << " --- \n";
        for(u64 i = 0; pc < p.size(); i++) {
            std::cout << pc << ": " << PrintOpCode(p, pc) << '\n';
        }
    }

    std::string PrintOpCode(Program& p, u64& pc)
    {
        u8 opcode = GetByte(p, pc);

        std::string result;

        switch(opcode)
        {
            case OPCODE_PUSH: { result = "PUSH"; } break;
            case OPCODE_POP: { result = "POP"; } break;
            case OPCODE_STORE: { result = "STORE"; } break;
            case OPCODE_LOAD: { result = "LOAD"; } break;
            case OPCODE_JUMP: { result = "JUMP"; } break;
            case OPCODE_JUMP_IF_TRUE: { result = "JUMP_IF_TRUE"; } break;
            case OPCODE_JUMP_IF_FALSE: { result = "JUMP_IF_FALSE"; } break;

            case OPCODE_START_SUBROUTINE: { result = "START_SUBROUTINE"; } break;
            case OPCODE_JUMP_SUBROUTINE: { result = "JUMP_SUBROUTINE"; } break;
            case OPCODE_RETURN_FROM_SUBROUTINE: { result = "RETURN_FROM_SUBROUTINE"; } break;

            case OPCODE_ADD: { result = "ADD"; } break;
            case OPCODE_MINUS: { result = "MINUS"; } break;
            case OPCODE_MULTIPLY: { result = "MULTIPLY"; } break;
            case OPCODE_DIVIDE: { result = "DIVIDE"; } break;

            case OPCODE_COMP_LESS: { result = "COMP_LESS"; } break;
            case OPCODE_COMP_GREATER: { result = "COMP_GREATER"; } break;
            case OPCODE_COMP_LESS_EQUAL: { result = "COMP_LESS_EQUAL"; } break;
            case OPCODE_COMP_GREATER_EQUAL: { result = "COMP_GREATER_EQUAL"; } break;
            case OPCODE_COMP_EQUAL: { result = "COMP_EQUAL"; } break;
            case OPCODE_COMP_NOT_EQUAL: { result = "COMP_NOT_EQUAL"; } break;
            case OPCODE_COMP_AND: { result = "COMP_AND"; } break;
            case OPCODE_COMP_OR: { result = "COMP_OR"; } break;

            case OPCODE_START_DEFER: { result = "START_DEFER"; } break;
            case OPCODE_EXEC_DEFER: { result = "EXEC_DEFER"; } break;

            default: { result = "INVALID OPCODE"; } break;
        }

        u64 argc = GetOpcodeArgC((OpcodeType)opcode);
        if(argc == -1)
        {
            LogError("Opcode does not have an existing argc, '" + std::to_string(opcode) + "'");
            return "";
        }

        for(int i = 0; i < argc; i++) {
            if(i == 0) {
                result += ": ";
            }
            else {
                result += ", ";
            }

            Value arg = decodeConstant(p, pc);
            result += std::to_string(arg);
        }

        return result;
    }
};

struct BytecodeEmitter : NodeVisitor
{
    Program program;

    u64 defercount = 0;
    u64 og_defercount = 0;
	void visit(ScopeNode* node)
    {
        og_defercount = defercount;
        node->statement->visit(this);

        if(defercount > og_defercount) {
            program.push(OPCODE_EXEC_DEFER);
            program.push(Value((i64)(defercount - og_defercount)));
        }

        defercount = og_defercount;
    }

	void visit(SequenceNode* node)
	{
        for(auto&& n : node->lst)
        {
            n->visit(this);
        }
	}

    void visit(IdentifierNode* node)
    {
        program.push(Value(VALUE_ID, node->id));
        previous_id = node->id;
    }

	void visit(BinaryNode* node)
	{
        node->left->visit(this);
        node->right->visit(this);
        switch(node->op.type)
        {
            case TOKEN_PLUS: { program.push(OPCODE_ADD); } break;
            case TOKEN_MINUS: { program.push(OPCODE_MINUS); } break;
            case TOKEN_MULTIPLY: { program.push(OPCODE_MULTIPLY); } break;
            case TOKEN_DIVIDE: { program.push(OPCODE_DIVIDE); } break;

            case TOKEN_EQUAL_EQUAL: { program.push(OPCODE_COMP_EQUAL); } break;
            case TOKEN_NOT_EQUAL: { program.push(OPCODE_COMP_NOT_EQUAL); } break;
            case TOKEN_LESS: { program.push(OPCODE_COMP_LESS); } break;
            case TOKEN_GREATER: { program.push(OPCODE_COMP_GREATER); } break;
            case TOKEN_LESS_EQUAL: { program.push(OPCODE_COMP_LESS_EQUAL); } break;
            case TOKEN_GREATER_EQUAL: { program.push(OPCODE_COMP_GREATER_EQUAL); } break;

            case TOKEN_AND: { program.push(OPCODE_COMP_AND); } break;
            case TOKEN_OR: { program.push(OPCODE_COMP_OR); } break;

            default:
            {
                LogError("Invalid binary operator '" + std::to_string(node->op) + "'");
            } break;
        }
	}

	void visit(ValueNode* node)
    {
        Value value(node->type, node->token.text);

        if(value.IsId())
        {
            program.push(OPCODE_LOAD);
            program.push(value);
        }
        else
        {
            if(node->negative) value.Negative();
            program.push(OPCODE_PUSH);
            program.push(value);
        }
    }

	void visit(VariableDeclNode* node)
	{
        if (node->value != NULL)
        {
            node->value->visit(this);
        }
        else
        {
            program.push(OPCODE_PUSH);
            program.push(Value((i64)0));
        }

        node->type->visit(this);
	}

	void visit(AssignmentNode* node)
	{
        node->value->visit(this);
        program.push(OPCODE_STORE);
        node->id->visit(this);
	}

    std::string previous_id = "";
	void visit(FunctionNode* node)
	{
        program.push(OPCODE_START_SUBROUTINE);
        node->id->visit(this);
        i64 start = program.size();
        program.push(Value((i64)0));

        for(int i = node->args.size() - 1; i >= 0; i--)
        {
            node->args[i]->visit(this);
            program.push(OPCODE_STORE);
            program.push(Value(previous_id));
        }

        node->block->visit(this);
        program.push(OPCODE_RETURN_FROM_SUBROUTINE);

        program.replace(start, Value((i64)program.size()));
	}

	void visit(CallNode* node)
	{
        for(auto&& arg : node->args)
        {
            arg->visit(this);
        }

        program.push(OPCODE_JUMP_SUBROUTINE);
        node->id->visit(this);
	}

	void visit(IfNode* node)
	{
        node->cond->visit(this);
        if(node->elseblock == NULL)
        {
            program.push(OPCODE_JUMP_IF_FALSE);
            i64 start = program.size();
            program.push(Value((i64)0));

            node->block->visit(this);
            program.replace(start, Value((i64)program.size()));
        }
        else
        {

            program.push(OPCODE_JUMP_IF_FALSE);
            i64 start = program.size();
            program.push(Value((i64)0));

            node->block->visit(this);
            
            program.push(OPCODE_JUMP);
            i64 end = program.size();
            program.push(Value((i64)0));

            program.replace(start, Value((i64)program.size()));
            node->elseblock->visit(this);
            program.replace(end, Value((i64)program.size()));
        }
	}

    u64 loopstart, breakloc;
	void visit(ForNode* node)
	{
		if (node->init != NULL) node->init->visit(this);
        loopstart = program.size();
		if (node->cond != NULL) node->cond->visit(this);

        program.push(OPCODE_JUMP_IF_TRUE);
        i64 jumpoverloc = program.size();
        program.push(Value((i64)0));


        breakloc = program.size();
        program.push(OPCODE_JUMP);
        program.push(Value((i64)0));

        program.replace(jumpoverloc, Value((i64)program.size()));

		if (node->incr != NULL) node->incr->visit(this);
		node->block->visit(this);

        program.push(OPCODE_JUMP);
        program.push(Value((i64)loopstart));

        program.replace(breakloc+1, Value((i64)program.size()));
	}

	void visit(LoopNode* node)
	{
        program.push(OPCODE_JUMP);
        i64 jumpoverloc = program.size();
        program.push(Value((i64)0));

        breakloc = program.size();
        program.push(OPCODE_JUMP);
        program.push(Value((i64)0));

        program.replace(jumpoverloc, Value((i64)program.size()));

        loopstart = program.size();
        node->block->visit(this);

        program.push(OPCODE_JUMP);
        program.push(Value((i64)loopstart));

        program.replace(breakloc+1, Value((i64)program.size()));
	}

	void visit(ReturnNode* node)
    {
        if(node->expr != NULL) node->expr->visit(this);

        if(defercount > og_defercount) {
            program.push(OPCODE_EXEC_DEFER);
            program.push(Value((i64)(defercount - og_defercount)));
            defercount = og_defercount;
        }

        program.push(OPCODE_RETURN_FROM_SUBROUTINE);
    }

	void visit(BreakNode* node)
    {
        if(defercount > og_defercount) {
            program.push(OPCODE_EXEC_DEFER);
            program.push(Value((i64)(defercount - og_defercount)));
            defercount = og_defercount;
        }

        program.push(OPCODE_JUMP);
        program.push(Value((i64)breakloc));
    }

	void visit(ContinueNode* node)
    {
        if(defercount > og_defercount) {
            program.push(OPCODE_EXEC_DEFER);
            program.push(Value((i64)(defercount - og_defercount)));
            defercount = og_defercount;
        }

        program.push(OPCODE_JUMP);
        program.push(Value((i64)loopstart));
    }

	void visit(DeferNode* node)
    {
        program.push(OPCODE_START_DEFER);
        u64 deferLoc = program.size();
        program.push(Value((i64)0));
        node->stat->visit(this);
        program.replace(deferLoc, Value((i64)(program.size() - deferLoc)));
        defercount++;
    }

    void visit(ImportNode* node) {
    }
};

void PrintTokenStream(TokenStream stream) {
	for (auto&& t : stream) {
        std::cout << std::to_string(t);
	}
}

std::string LoadEntireFileIntoString(std::string path) {
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

void WriteProgramToFile(std::string path, Program program) {
    std::ofstream out(path, std::ios::binary);

    u64 progsize = program.size();
    u8* data = program.data();
    out.write(reinterpret_cast<char*>(&progsize), sizeof progsize);
    out.write(reinterpret_cast<char*>(&data), sizeof(u8) * program.size());
}

#define REGISTER_INTRINSIC(function_name, function_data, lambda)\
    [&]() {\
        tcv.global.SetFunction(#function_name, function_data);\
        vm.RegisterIntrinsic(#function_name, lambda);\
    }()

void IntrinsicPrintln(VirtualMachine& vm)
{
    Value v = vm.stack.top(); vm.stack.pop();
    switch(v.GetType()) {
        case VALUE_STR:
        {
            std::cout << v.GetStrUnqouted() << '\n';
        } break;
        case VALUE_INT:
        {
            std::cout << std::to_string(*v.GetInt()) << '\n';
        } break;
        case VALUE_REAL:
        {
            std::cout << std::to_string(*v.GetReal()) << '\n';
        } break;
        case VALUE_BOOL:
        {
            std::cout << (*v.GetBool() ? "true" : "false") << '\n';
        } break;
        default:
        {
            LogError("Invalid println type \'" + std::to_string(v.GetType()) + "\'");
        } break;
    }
}

void IntrinsicGetln(VirtualMachine& vm)
{
    Value v = vm.stack.top(); vm.stack.pop();

    std::string result = "";
    std::cout << v.GetStrUnqouted();
    std::cin >> result;

    vm.stack.push(result);
}

void IntrinsicToString(VirtualMachine& vm)
{
    Value v = vm.stack.top(); vm.stack.pop();
    std::string result = "";
    switch(v.GetType()) {
        case VALUE_STR:
        {
            result = *v.GetStr();
        } break;
        case VALUE_INT:
        {
            result = std::to_string(*v.GetInt());
        } break;
        case VALUE_REAL:
        {
            result = std::to_string(*v.GetReal());
        } break;
        case VALUE_BOOL:
        {
            result = *v.GetBool() ? "true" : "false";
        } break;
        default:
        {
            LogError("Invalid to_string type \'" + std::to_string(v.GetType()) + "\'");
        } break;
    }

    vm.stack.push('"' + result + '"');
}

int CompileSourceFile(std::string path) {
    auto source = LoadEntireFileIntoString(path);

    if (!source.empty()) {
        Lexer lexer(source);
        if (error) return 1;

        Parser parser(lexer.stream);
        if (error) return 1;

        VirtualMachine vm;
        TypeChecker tcv(path);

        REGISTER_INTRINSIC(
                println,
                FunctionData(VALUE_NONE, { VALUE_ANY }),
                IntrinsicPrintln);

        REGISTER_INTRINSIC(
                getln,
                FunctionData(VALUE_STR, { VALUE_STR }),
                IntrinsicGetln);

        REGISTER_INTRINSIC(
                max,
                FunctionData(VALUE_INT, { VALUE_INT, VALUE_INT }),
                [&](VirtualMachine& vm) {
                    Value r = vm.stack.top(); vm.stack.pop();
                    Value l = vm.stack.top(); vm.stack.pop();

                    vm.stack.push((*l.GetInt() > *r.GetInt()) ? l : r);
                });

        REGISTER_INTRINSIC(
                min,
                FunctionData(VALUE_INT, { VALUE_INT, VALUE_INT }),
                [&](VirtualMachine& vm) {
                    Value r = vm.stack.top(); vm.stack.pop();
                    Value l = vm.stack.top(); vm.stack.pop();

                    vm.stack.push((*l.GetInt() < *r.GetInt()) ? l : r);
                });

        REGISTER_INTRINSIC(
                to_string,
                FunctionData(VALUE_STR, { VALUE_ANY }),
                IntrinsicToString);

        parser.tree->visit(&tcv);
        if (error) return 1;

        BytecodeEmitter bce;
        parser.tree->visit(&bce);
        if (error) return 1;

        vm.PrintProgram(bce.program);
        vm.RunProgram(bce.program);
        if (error) return 1;

        WriteProgramToFile("test.kasm", bce.program);

        NodeFreeVisitor nfv;
        parser.tree->visit(&nfv);

        // TODO: Implement out of order function calling.
        // TODO: Implement default arg value.
        // TODO: Make type's an explicit value;
        // TODO: Implement explicit casting for e.g. cast(25, float)
        // TODO: Implement variadic args.
    }
    else {
        LogError("Source is empty");
    }

    return 0;
}

int main(int argc, char** argv) {
    for (int i = 1; i < argc; i++) {
        std::string in = argv[i];

        if (in == "-o") {

        }
    }

    return CompileSourceFile("test.kilt");
}
