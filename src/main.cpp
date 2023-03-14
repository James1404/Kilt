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
	size_t size() const { return data.size(); }

	void push(T t) { data.push_back(t); }
	void pop() { data.pop_back(); }
	T& top() { return data.back(); }

	T& at(size_t depth) { return data.at(size() - depth - 1); }
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

    TOKEN_PTR,
    TOKEN_ALLOC, TOKEN_FREE,
    TOKEN_REF, TOKEN_DEREF, 

    TOKEN_LIST, // list is "..."

    TOKEN_DEFER,
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

    { "ptr", TOKEN_PTR },
    { "alloc", TOKEN_ALLOC },
    { "free", TOKEN_FREE },
    { "ref", TOKEN_REF },
    { "deref", TOKEN_DEREF },

    { "...", TOKEN_LIST },

    { "defer", TOKEN_DEFER },
};

enum IntegerSizes : u8 {
    INTEGER_8BIT = 0x00,
    INTEGER_16BIT = 0x01,
    INTEGER_32BIT = 0x02,
    INTEGER_64BIT = 0x03,
};

class Value;

// TODO: Turn this into a full class with more complex type's like functions with arg's and return types.
enum ValueType : u8 {
    VALUE_NONE = 0x00,

    VALUE_ARRAY,
    VALUE_INT,
    VALUE_REAL,
    VALUE_BOOL,
    VALUE_STR,
    VALUE_ID,
    VALUE_PTR
};

class PtrValue {
private:
    u8* ptr;
    u64 size;
    ValueType type;
public:
    PtrValue() = delete;
    PtrValue(u8* ptr_, u64 size_, ValueType type_)
        : ptr(ptr_), size(size_), type(type_)
    {}

    u8* GetPtr() {
        return ptr;
    }

    u64 GetSize() {
        switch(type)
        {
            case VALUE_ARRAY: { return size; } break;
            case VALUE_INT: { return sizeof(u64); } break;
            case VALUE_REAL: { return sizeof(double); } break;
            case VALUE_STR: { return size; } break;
            case VALUE_BOOL: { return sizeof(bool); } break;
            case VALUE_ID: { return size; } break;
            case VALUE_PTR: { return size; } break;
        }
        return 0;
    }
};

class ArrayValue {
private:
    // NOTE: If array value is 0 then it is dynamic.
    // NOTE: size > 0 then its fixed sized.
    size_t allocated = 0, used = 0;
    Value* data = NULL;

    ValueType type;

    void allocate(size_t size) {
        /*
        if (data == NULL) {
            data = (Value*)malloc(sizeof(Value) * size);
        }
        else {
            Value* temp = data;
            data = (Value*)malloc(sizeof(Value) * size);
            memcpy(temp, data, allocated);
            delete[] temp;
        }
        allocated = size;
        */
    }
public:
    ArrayValue(ValueType type_, size_t size = -1)
        : type(type_)
    {
        if (size > 0) {
            allocate(size);
        }
        else {
            allocate(10);
        }
    }

    ArrayValue(const ArrayValue& other)
        : allocated(other.allocated), used(other.used)
    {
        allocate(allocated);
        memcpy(&data, &other.data, used); // TODO: Fix this its stupid probably
    }
    /*
    void push(Value* v) {
        assert(v->GetType() == type);

        if (used >= allocated) {
            allocate(allocated * 2);
        }

        data[used++] = *v;
    }

    void erase(u64 idx) {
        assert(idx <= used);
        memmove(&data[idx], &data[idx + 1], used - idx); // TODO: Confirm this works
        used--;
    }

    ArrayValue slice(u64 start, u64 end) {
        assert(start < end);
        assert(end <= used);

        u64 size = end - start;

        ArrayValue result(type, size);
        memcpy(result.data, &data[start], size);

        return result;
    }

    Value* at(u64 idx) {
        assert(idx <= used);
        return &data[idx];
    }

    size_t size() const {
        return used;
    }*/
};

namespace std {
    std::string to_string(ArrayValue other) {
        return "";
    }
}

class FunctionValue {
private:
    ValueType returntype;
    std::vector<ValueType> argtypes;
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
            case VALUE_ARRAY: { return "array"; } break;
            case VALUE_INT: { return "int"; } break;
            case VALUE_REAL: { return "float"; } break;
            case VALUE_STR: { return "string"; } break;
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
            case VALUE_ARRAY:
            {
                delete static_cast<ArrayValue*>(data);
            } break;
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
    bool IsArray() const { return type == VALUE_ARRAY; }
    bool IsInt() const { return type == VALUE_INT; }
    bool IsReal() const { return type == VALUE_REAL; }
    bool IsStr() const { return type == VALUE_STR; }
    bool IsBool() const { return type == VALUE_BOOL; }
    bool IsId() const { return type == VALUE_ID; }
    bool IsPtr() const { return type == VALUE_PTR; }

    ValueType& GetType() { return type; }

    ArrayValue* GetArray() const { return static_cast<ArrayValue*>(data); }
    i64* GetInt() const { return static_cast<i64*>(data); }
    double* GetReal() const { return static_cast<double*>(data); }
    std::string* GetStr() const { return static_cast<std::string*>(data); }
    bool* GetBool() const { return static_cast<bool*>(data); }
    std::string* GetId() const { return static_cast<std::string*>(data); }
    PtrValue* GetPtr() const { return static_cast<PtrValue*>(data); }

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
            case VALUE_ARRAY:
            {
                data = new ArrayValue(*other.GetArray());
            } break;
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
            /*case VALUE_ARRAY:
            {
                data = new ArrayValue();
            } break;*/
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

	Value& operator=(Value& other)
    {
        FreeData();
        LinkToken(other.linked);
        type = other.type;

        switch(type)
        {
            case VALUE_ARRAY:
            {
                data = new ArrayValue(*other.GetArray());
            } break;
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
            /*
            case VALUE_ARRAY:
            {
                data = new ArrayValue();
            } break;
            */
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
    std::string to_string(Value other) {
        switch (other.GetType())
        {
            case VALUE_ARRAY: { return std::to_string(*other.GetArray()); } break;
            case VALUE_INT: { return std::to_string(*other.GetInt()); } break;
            case VALUE_REAL: { return std::to_string(*other.GetReal()); } break;
            case VALUE_STR: { return '"' + *other.GetStr() + '"'; } break;
            case VALUE_BOOL: { return (std::string)(*other.GetBool() ? "true" : "false"); } break;
            case VALUE_ID: { return *other.GetId() ; } break;
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

class Program;
Program ToProgram(i64 value);
Program ToProgram(double value);
Program ToProgram(bool value);
Program ToProgram(std::string value);
Program ToProgram(Value v);

class Program {
private:
    std::vector<u8> stream;
public:
    auto data() {
        return stream.data();
    }

    auto begin() {
        return stream.begin();
    }

    auto end() {
        return stream.end();
    }

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

    size_t size() const {
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
        return{};
    }
};

Program ToProgram(Value v) {
    Program result;

    result.push(v.GetType());

    switch(v.GetType()) {
        //case VALUE_ARRAY: { return; } break;
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
	virtual void visit(DeferNode* node) = 0;
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

    ValueType type = VALUE_NONE;

	VISIT_
};

struct VariableDeclNode : Node
{
	Token id, type;
    Node* value;
    bool infer;

    bool is_array = false;

	VariableDeclNode(Token id_, Token type_, Node* value_ = NULL, bool is_array_ = false)
		: id(id_), type(type_), value(value_), infer(false), is_array(is_array_)
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
    Token id;
    Node* expr;

    ReturnNode(Token id_, Node* expr_)
        : id(id_), expr(expr_)
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

struct DeferNode : Node {
    Node* stat;

    DeferNode(Node* stat_) : stat(stat_) {}
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
                return new ReturnNode(t, expr);
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

                if(current.type == TOKEN_IDENTIFIER)
                {
                    Token type = current;
                    advance();

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
                else if(AdvanceIfMatch(TOKEN_LEFT_BRACKET))
                {
                    Token type = current;
                    if(AdvanceIfMatch(TOKEN_IDENTIFIER))
                    {
                        if(AdvanceIfMatch(TOKEN_COMMA))
                        {
                            Node* size = Expression();
                            if(size == NULL) {
                                if(AdvanceIfMatch(TOKEN_LIST))
                                {
                                    size = new ValueNode(Token(TOKEN_LITERAL, "-1"));
                                }
                                else
                                {
                                    LogError("Array size must be a valid expression", id.line, id.location);
                                }
                            }

                            if(AdvanceIfMatch(TOKEN_RIGHT_BRACKET))
                            {
                                return new VariableDeclNode(id, type, size, true);
                            }
                            else
                            {
                                LogError("Array decleration must have a closing bracket", id.line, id.location);
                            }
                        }
                        else
                        {
                            LogError("Array decleration requires a comma after the type identifier", id.line, id.location);
                        }
                    }
                    else
                    {
                        LogError("Array decleration requires a type identifier", id.line, id.location);
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
            // TODO: Implement array indexing e.g. names[5] = "James";
            if(peek().type == TOKEN_EQUAL)
            {
                advance(2);
                return new AssignmentNode(id, Expression());
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
                return new AssignmentNode(id, binary);
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
            if(peek().type == TOKEN_LEFT_PARENTHESIS)
            {
                Token id = current;
                advance(2);

                auto arg_list = ArgumentList();

                if(current.type == TOKEN_RIGHT_PARENTHESIS)
                {
                    advance();
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
        else if(AdvanceIfMatch(TOKEN_LEFT_BRACKET))
        {
            if(AdvanceIfMatch(TOKEN_RIGHT_BRACKET))
            {
                return new ValueNode(t, VALUE_ARRAY);
            }
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

    ValueType returntype = VALUE_NONE;
    bool got_return = false;
    bool in_loop = false;
    bool in_func = false;
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

        if(l.GetType() == VALUE_ARRAY)
        {
            switch(node->op.type)
            {
                case TOKEN_DIVIDE:
                case TOKEN_MULTIPLY:
                case TOKEN_NOT:
                case TOKEN_LESS:
                case TOKEN_GREATER:
                case TOKEN_LESS_EQUAL:
                case TOKEN_GREATER_EQUAL:
                {
                    LogError("Invalid operator operation on an array", l.GetLine(), l.GetLoc());
                } break;
            }
        }

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
                if(node->is_array) {
                    node->value->visit(this);
                    Value arraysizetype = stack.top(); stack.pop();
                    if(!arraysizetype.IsInt()) {
                        LogError("Array size must be an integer, not an '" + arraysizetype.GetTypeAsString() + "'");
                        return;
                    }

                    vartype = VALUE_ARRAY;
                }
                else {
                    node->value->visit(this);
                    Value valuetype = stack.top(); stack.pop();
                    if(!CompareValueTypes(vartype, valuetype))
                    {
                        LogError("Cannot assign '" + std::to_string(valuetype) + "' to '" + std::to_string(vartype) + "'", node->id.line, node->id.location);
                        return;
                    }
                }
            }
        }

        current->SetType(node->id.text, vartype);
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

        returntype = StringToValueType(node->returntype.text);
        current->parent->SetFunction(node->id.text, FunctionData(returntype, args));

        got_return = false;

        in_func = true;
        node->block->visit(this);
        in_func = false;

        if(returntype != VALUE_NONE && !got_return) {
            LogError("Function '" + node->id.text + "' expect's an '" + node->returntype.text + "' to be returned, but never get's one.");
            // TODO: Check if all code path's return a value.
        }

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
            LogError("Return statement must be in a function", node->id.line, node->id.location);
        }

        ValueType rtype = VALUE_NONE;
        if(node->expr != NULL) {
             node->expr->visit(this);
             rtype = stack.top().GetType(); stack.pop();
        }

        if(returntype != rtype)
        {
            LogError("Returned type '" + std::to_string(rtype) + "' is different as expected '" + std::to_string(returntype) + "'", node->id.line, node->id.location);
        }

        got_return = true;
    }

	void visit(BreakNode* node)
    {
        if(!in_loop)
        {
            LogError("Break statement must be in a loop", node->id.line, node->id.location);
        }
    }

	void visit(ContinueNode* node)
    {
        if(!in_loop)
        {
            LogError("Continue statement must be in a loop", node->id.line, node->id.location);
        }
    }

	void visit(DeferNode* node)
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

	void visit(DeferNode* node)
    {
        node->stat->visit(this);
        delete node->stat;
    }
};

enum OpCodeType : u8
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

    OPCODE_STORE_ARRAY, OPCODE_STORE_ARRAY_INDEX, OPCODE_LOAD_ARRAY_INDEX,

    OPCODE_STORE_PTR, OPCODE_LOAD_PTR,
    OPCODE_ALLOC, OPCODE_FREE,

    OPCODE_START_DEFER, OPCODE_EXEC_DEFER,

    OPCODE_MAX = 0xff
};

std::map<OpCodeType,u64> OpCodeArgTable = {
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
    { OPCODE_STORE_ARRAY, 1 },
    { OPCODE_STORE_ARRAY_INDEX, 1 },
    { OPCODE_LOAD_ARRAY_INDEX, 1 },

    { OPCODE_STORE_PTR, 1 },
    { OPCODE_LOAD_PTR, 1 },
    { OPCODE_ALLOC, 2 },
    { OPCODE_FREE, 2 },

    { OPCODE_START_DEFER, 1 },
    { OPCODE_EXEC_DEFER, 0 },

};

u64 GetOpcodeArgC(OpCodeType op) {
    auto it = OpCodeArgTable.find(op);
    if(it != OpCodeArgTable.end()) return it->second;
    return -1;
}

// Instruction set
//
// PUSH constant
// POP
// STORE id
// LOAD id
//
// OPCODE_START_SUBROUTINE id skip

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

#define VM_BINARY_OP(type, l, op, r)\
    {\
        Value result = l.Get##type() op r.Get##type();\
        stack.push(result);\
    }

class MemoryArena {
private:
   u8* mem = NULL;
   size_t used = 0, allocated = 0;

   struct FreeMem {
       u8* loc;
       u64 size;
   };
   std::vector<FreeMem> freestack;

   static constexpr size_t defaultSize = 10000;

   void resize(u64 size) {
       allocated += size;
       u8* newMem = new u8[allocated];
       if(mem != NULL) {
           memcpy(mem, newMem, used);
           delete[] mem;
       }
       mem = newMem;
   }

public:
   MemoryArena() {
       allocated = defaultSize;
       resize(0);
   }

   ~MemoryArena() {
       delete[] mem;
   }

   u8* Alloc(u64 size) {
       for(int i = 0; i < freestack.size(); i++) {
           FreeMem freed = freestack.at(i);
           if(freed.size >= size) {
               u64 left = freed.size - size;
               freestack.erase(freestack.begin() + i);

               if(size > 0) {
                   FreeMem f = {
                       freed.loc + size,
                       left
                   };
                   freestack.push_back(f);
               }

               return freed.loc;
           }
       }

       used += size;

       if(used > allocated) {
           resize(allocated * 1.5);
       }

       return (mem + used); 
   }

   void Free(PtrValue value) {
       FreeMem f = {
           value.GetPtr(),
           value.GetSize()
       };
       freestack.push_back(f);
   }
};

class VirtualMachine
{
private:
    u64 pc = 0;

	Stack<Value> stack;
    Stack<u64> callStack;
    Stack<Program> deferstack;

    std::map<std::string,u64> jumpTable;

	Environment env;
    MemoryArena memory;

    Program program;
public:
    VirtualMachine(Program p_)
        : program(p_)
    {
        PrintProgram();

        for(pc = 0;pc < program.size();)
        {
            exec();
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

    u8 GetByte() { return program.at(pc++); }

    Value decodeConstant() {
        u8 type = GetByte();
        switch(type) {
            case VALUE_ARRAY:
            {

            } break;
            case VALUE_INT:
            {
                u8 properties = GetByte();

                u8 size = properties & 0b00000011;
                u8 sign = (properties >> 2) & 0b00000001;

                switch(size)
                {
                    case INTEGER_8BIT:
                    {
                    } break;
                    case INTEGER_16BIT:
                    {
                    } break;
                    case INTEGER_32BIT:
                    {
                    } break;
                    case INTEGER_64BIT:
                    {
                        u8 bytes8[8] = {
                            GetByte(),
                            GetByte(),
                            GetByte(),
                            GetByte(),
                            GetByte(),
                            GetByte(),
                            GetByte(),
                            GetByte()
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
                    GetByte(),
                    GetByte(),
                    GetByte(),
                    GetByte(),
                    GetByte(),
                    GetByte(),
                    GetByte(),
                    GetByte()
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
                return Value((bool)GetByte());
            } break;
            case VALUE_STR:
            {
                Value size = decodeConstant();
                std::string str;
                for(int i = 0; i < *size.GetInt(); i++) {
                    str.append(1, GetByte());
                }

                return Value((ValueType)type, str);
            } break;
            case VALUE_ID:
            {
                Value size = decodeConstant();
                std::string id;
                for(int i = 0; i < *size.GetInt(); i++) {
                    id.append(1, GetByte());
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

	void exec()
	{
        u8 opcode = GetByte();
        switch (opcode)
        {
            case OPCODE_PUSH:
            {
                stack.push(decodeConstant());
            } break;
            case OPCODE_POP:
            {
                stack.pop();
            } break;

            case OPCODE_STORE:
            {
                Value v = stack.top();
                stack.pop();

                Value arg = decodeConstant();
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
                Value arg = decodeConstant();
                std::string name = *arg.GetId();
                auto id = env.FindIdentifier(name);
                if (id)
                {
                    stack.push(*id);
                }
            } break;

            case OPCODE_JUMP:
            {
                Value arg = decodeConstant();
                pc = *arg.GetInt();
            } break;
            case OPCODE_JUMP_IF_TRUE:
            {
                Value arg = decodeConstant();
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
                Value arg = decodeConstant();
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
                Value id = decodeConstant();
                Value skip = decodeConstant();
                jumpTable.emplace(*id.GetId(), pc);
                pc = *skip.GetInt();
            } break;
            case OPCODE_JUMP_SUBROUTINE:
            {
                Value arg = decodeConstant();
                auto it = jumpTable.find(*arg.GetId());
                if(it != jumpTable.end())
                {
                    env.IncrementDepth();

                    callStack.push(pc);
                    pc = it->second;
                }
                else
                {
                    LogError("Jump location doesn't exist '" + *arg.GetId()  + "'");
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

            case OPCODE_STORE_ARRAY:
            {
                Value arg = decodeConstant();
                Value size = stack.top(); stack.pop();

                std::string name = *arg.GetId();

                Value arr;

                auto id = env.FindIdentifier(name);
                if(id) {
                    env.SetIdentifier(name, arr);
                }
                else {
                    env.CreateIdentifier(name);
                    env.SetIdentifier(name, arr);
                }
            } break;
            case OPCODE_STORE_ARRAY_INDEX:
            {
                Value arg = decodeConstant();
                Value idx = stack.top(); stack.pop();

                std::string name = *arg.GetId();
            } break;
            case OPCODE_LOAD_ARRAY_INDEX:
            {
                Value arg = decodeConstant();
                Value idx = stack.top(); stack.pop();

                std::string name = *arg.GetId();
            } break;

            case OPCODE_STORE_PTR:
            {
                Value loc = decodeConstant();
            } break;
            case OPCODE_LOAD_PTR:
            {
                Value loc = decodeConstant();
            } break;
            case OPCODE_ALLOC:
            {
                Value size = decodeConstant();

                Value ptr = memory.Alloc(*size.GetInt());
                stack.push(ptr);
            } break;
            case OPCODE_FREE:
            {
                Value v = decodeConstant();
                memory.Free(*v.GetPtr());
            } break;

            case OPCODE_START_DEFER:
            {
                Value size = decodeConstant();
                auto subprogram = program.subprogram(pc, *size.GetInt());
                deferstack.push(subprogram);
                pc += *size.GetInt();
            } break;
            case OPCODE_EXEC_DEFER:
            {
                if(!deferstack.empty()) {
                    stack.pop();

                }
            } break;

            default:
            {
                LogError("Invalid opcode type: " + std::to_string(opcode));
            } break;
        }
	}
    
    void PrintProgram() {
        pc = 0;

        u64 programsize = sizeof(program) + (sizeof(program.at(0)) * program.size());
        std::cout << " --- Bytecode Program, Size(bytes): " << programsize << " --- \n";
        for(int i = 0; pc < program.size(); i++) {
            std::cout << "# " << PrintOpCode() << '\n';
        }
    }

    std::string PrintOpCode()
    {
        u8 opcode = GetByte();

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

            case OPCODE_STORE_ARRAY: { result = "STORE_ARRAY"; } break;
            case OPCODE_STORE_ARRAY_INDEX: { result = "STORE_ARRAY_INDEX"; } break;
            case OPCODE_LOAD_ARRAY_INDEX: { result = "LOAD_ARRAY_INDEX"; } break;

            case OPCODE_STORE_PTR: { result = "STORE_PTR"; } break;
            case OPCODE_LOAD_PTR: { result = "LOAD_PTR"; } break;
            case OPCODE_ALLOC: { result = "ALLOC"; } break;
            case OPCODE_FREE: { result = "FREE"; } break;
            case OPCODE_START_DEFER: { result = "START_DEFER"; } break;
            case OPCODE_EXEC_DEFER: { result = "EXEC_DEFER"; } break;

            default: { result = "INVALID OPCODE"; } break;
        }

        u64 argc = GetOpcodeArgC((OpCodeType)opcode);
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

            Value arg = decodeConstant();
            result += std::to_string(arg);
        }

        return result;
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
        if(node->is_array)
        {
            node->value->visit(this);
            program.push(OPCODE_STORE_ARRAY);
            program.push(Value(node->id.text));
        }
        else
        {
            if(node->value != NULL)
            {
                node->value->visit(this);
            }
            else
            {
                program.push(OPCODE_PUSH);
                program.push(Value((i64)0));
            }

            program.push(OPCODE_STORE);
            program.push(Value(node->id.text));
            previous_id = node->id.text;
        }
	}

	void visit(AssignmentNode* node)
	{
        node->value->visit(this);
        program.push(OPCODE_STORE);
        program.push(Value(VALUE_ID, node->id.text));
	}

    std::string previous_id = "";
	void visit(FunctionNode* node)
	{
        program.push(OPCODE_START_SUBROUTINE);
        program.push(Value(node->id.text));
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
        program.push(Value(node->id.text));
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
        program.push(OPCODE_RETURN_FROM_SUBROUTINE);
    }

	void visit(BreakNode* node)
    {
        program.push(OPCODE_JUMP);
        program.push(Value((i64)breakloc));
    }

	void visit(ContinueNode* node)
    {
        program.push(OPCODE_JUMP);
        program.push(Value((i64)loopstart));
    }

	void visit(DeferNode* node)
    {
        program.push(OPCODE_START_DEFER);
        u64 deferLoc = program.size();
        program.push(Value((i64)0));
        node->stat->visit(this);
        program.push(OPCODE_JUMP);
        u64 jumpLoc = 0;
        program.push(Value((i64)0));
        program.replace(deferLoc, Value((i64)(program.size() - deferLoc)));
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

    size_t progsize = program.size();
    u8* data = program.data();
    out.write(reinterpret_cast<char*>(&progsize), sizeof progsize);
    out.write(reinterpret_cast<char*>(&data), sizeof(data) * program.size());
}

int main() {
	auto source = LoadEntireFileIntoString("test.code");

	if (!source.empty()) {
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

        WriteProgramToFile("test.kasm", bce.program);

        VirtualMachine vm(bce.program);
		if (error) return 1;
        vm.PrintRestOfStack();
        vm.PrintGlobalSymbolTable();
		
		NodeFreeVisitor nfv;
		parser.tree->visit(&nfv);
        // TODO: Automatic cast int to float if assigning to float e.g.
        // number : float = 25;
        //                   ^ this should be casted to a float
        // NOTE: Actually this might not have to be.
        // TODO: Think about explicit casting for e.g. (cast float)25

        // TODO: Make type's an explicit value;

        // TODO: Implement out of order function calling.
        // TODO: Implement default arg value.
        // TODO: Implement any type.
        // TODO: Implement variadic args.
        // TODO: Implement ptr's

        // TODO: Implement intrinsic function's like
        // println(args : any ...)
        // len(list)
        // at(list, idx)
	}
	else {
		LogError("Source is empty");
	}

	return 0;
}
