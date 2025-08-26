#pragma once
#include "token/token.hpp"
#include <memory>
#include <string>
#include <iostream>
#include <vector>
#include <optional>
#include <utility>

#define CPPREST_FORCE_REBUILD

enum class Mutability
{
    IMMUTABLE,
    MUTABLE,
    CONSTANT
};

// GENERAL AST NODE
struct Node
{
    Token token;
    virtual std::string toString()
    {
        return "Node: " + token.TokenLiteral;
    };
};

// GENERAL EXPRESSION NODE
struct Expression : Node
{
    Token expression;
    std::string toString() override
    {
        return "Expression: " + expression.TokenLiteral;
    }
    Expression(Token expr) : expression(expr) {};
};

// GENERAL STATEMENT NODE
struct Statement : Node
{
    Token statement;
    std::string toString() override
    {
        return "Statement: " + statement.TokenLiteral;
    }
    Statement(Token stmt) : statement(stmt) {};
};

// Identifier statement node
struct Identifier : Expression
{
    Token identifier;
    std::string toString() override
    {
        return "Identifier Expression: " + identifier.TokenLiteral;
    }
    Identifier(Token ident) : Expression(ident), identifier(ident) {};
};

struct FieldAccessExpression : Expression
{
    std::unique_ptr<Expression> base; // e.g. self
    Token field;

    std::string toString() override
    {
        return "Field Access Expression: " + base->toString() + "." + field.TokenLiteral;
    }

    FieldAccessExpression(std::unique_ptr<Expression> baseExpr, Token fieldToken)
        : Expression(fieldToken), base(std::move(baseExpr)), field(fieldToken) {};
};

struct NewComponentExpression : Expression
{
    Token new_token;      // token for 'new'
    Token component_name; // e.g. 'Player'
    std::vector<std::unique_ptr<Expression>> arguments;

    std::string toString() override
    {
        std::string args_str;
        for (auto &arg : arguments)
        {
            args_str += arg->toString() + ", ";
        }
        return "NewComponentExpression: new " + component_name.TokenLiteral + "(" + args_str + ")";
    }

    NewComponentExpression(Token newTok, Token compName, std::vector<std::unique_ptr<Expression>> args)
        : Expression(newTok), new_token(newTok), component_name(compName), arguments(std::move(args)) {}
};

// Null literal
struct NullLiteral : Expression
{
    Token null_token;
    std::string toString() override
    {
        return "Null Literal: " + null_token.TokenLiteral;
    }

    NullLiteral(Token null_tok) : Expression(null_tok), null_token(null_tok) {};
};

// Signed 16 bit Integer literal
struct ShortLiteral : Expression
{
    Token short_token;
    std::string toString() override
    {
        return "Short Literal: " + short_token.TokenLiteral;
    }
    ShortLiteral(Token short_t) : Expression(short_t), short_token(short_t) {};
};

// Unsigned 16 bit Ineteger literal
struct UnsignedShortLiteral : Expression
{
    Token ushort_token;
    std::string toString() override
    {
        return "UnsignedShort Literal: " + ushort_token.TokenLiteral;
    }
    UnsignedShortLiteral(Token ushort_t) : Expression(ushort_t), ushort_token(ushort_t) {};
};

// Signed 32 bit Integer literal
struct IntegerLiteral : Expression
{
    Token int_token;
    std::string toString() override
    {
        return "Integer Literal: " + int_token.TokenLiteral;
    }
    IntegerLiteral(Token int_t) : Expression(int_t), int_token(int_t) {};
};

// Unsigned 32 bit integer literal
struct UnsignedIntegerLiteral : Expression
{
    Token uint_token;
    std::string toString() override
    {
        return "UnsignedInteger Literal: " + uint_token.TokenLiteral;
    }
    UnsignedIntegerLiteral(Token uint_t) : Expression(uint_t), uint_token(uint_t) {};
};

// Signed 64 bit integer literal
struct LongLiteral : Expression
{
    Token long_token;
    std::string toString() override
    {
        return "Long Literal: " + long_token.TokenLiteral;
    }
    LongLiteral(Token long_t) : Expression(long_t), long_token(long_t) {};
};

// Unsigned 64 bit integer literal
struct UnsignedLongLiteral : Expression
{
    Token ulong_token;
    std::string toString() override
    {
        return "UnsignedLong Literal: " + ulong_token.TokenLiteral;
    }
    UnsignedLongLiteral(Token ulong_t) : Expression(ulong_t), ulong_token(ulong_t) {};
};

// Signed 128 bit integer literal
struct ExtraLiteral : Expression
{
    Token extra_token;
    std::string toString() override
    {
        return "Extra Literal: " + extra_token.TokenLiteral;
    }
    ExtraLiteral(Token extra_t) : Expression(extra_t), extra_token(extra_t) {};
};

// Unsigned 128 bit integer literal
struct UnsignedExtraLiteral : Expression
{
    Token uextra_token;
    std::string toString() override
    {
        return "UnsignedExtra Literal: " + uextra_token.TokenLiteral;
    }
    UnsignedExtraLiteral(Token uextra_t) : Expression(uextra_t), uextra_token(uextra_t) {};
};

// Boolean literal
struct BooleanLiteral : Expression
{
    Token boolean_token;
    std::string toString() override
    {
        return "Boolean Literal: " + boolean_token.TokenLiteral;
    }
    BooleanLiteral(Token bool_t) : Expression(bool_t), boolean_token(bool_t) {};
};

// Float literal
struct FloatLiteral : Expression
{
    Token float_token;
    std::string toString() override
    {
        return "Float Literal: " + float_token.TokenLiteral;
    }
    FloatLiteral(Token float_t) : Expression(float_t), float_token(float_t) {};
};

// Double literal
struct DoubleLiteral : Expression
{
    Token double_token;
    std::string toString() override
    {
        return "Double Literal: " + double_token.TokenLiteral;
    }
    DoubleLiteral(Token double_t) : Expression(double_t), double_token(double_t) {};
};

// 8 bit Char literal
struct CharLiteral : Expression
{
    Token char_token;
    std::string toString() override
    {
        return "Char Literal: " + char_token.TokenLiteral;
    }
    CharLiteral(Token char_t) : Expression(char_t), char_token(char_t) {};
};

// 16 bit Char literal
struct Char16Literal : Expression
{
    Token char16_token;
    std::string toString() override
    {
        return "Char16 Literal: " + char16_token.TokenLiteral;
    }
    Char16Literal(Token char16t) : Expression(char16t), char16_token(char16t) {};
};

// 32 bit Char literal
struct Char32Literal : Expression
{
    Token char32_token;
    std::string toString() override
    {
        return "Char32 Literal: " + char32_token.TokenLiteral;
    }
    Char32Literal(Token char32t) : Expression(char32t), char32_token(char32t) {};
};

// String literal
struct StringLiteral : Expression
{
    Token string_token;
    std::string toString() override
    {
        return "String Literal: " + string_token.TokenLiteral;
    }
    StringLiteral(Token string_t) : Expression(string_t), string_token(string_t) {};
};

// Call expression
struct CallExpression : Expression
{
    std::unique_ptr<Expression> function_identifier;
    std::vector<std::unique_ptr<Expression>> parameters;
    std::string toString() override
    {
        std::string args_str;
        for (size_t i = 0; i < parameters.size(); ++i)
        {
            args_str += parameters[i]->toString();
            if (i < parameters.size() - 1)
                args_str += ", ";
        }
        return "Call Expression: " + function_identifier->toString() + "(" + args_str + ")";
    }

    CallExpression(Token tok, std::unique_ptr<Expression> fn_ident, std::vector<std::unique_ptr<Expression>> params) : Expression(tok), function_identifier(std::move(fn_ident)), parameters(std::move(params)) {};
};

// Function expression struct node
struct FunctionExpression : Expression
{
    Token func_key;
    std::vector<Token> generic_parameters;
    std::vector<std::unique_ptr<Statement>> call;
    std::unique_ptr<Expression> return_type;
    bool isNullable = false;
    std::unique_ptr<Expression> block;

    std::string toString() override
    {
        std::string cl = "(";
        for (size_t i = 0; i < call.size(); ++i)
        {
            cl += call[i] ? call[i]->toString() : "<null>";
            if (i < call.size() - 1)
            {
                cl += ", ";
            }
        }
        cl += ")";

        std::string nullStr = "";
        if (isNullable)
        {
            nullStr += "?";
        }

        std::string genStr = "";
        if (!generic_parameters.empty())
        {
            genStr += "<";
            for (size_t i = 0; i < generic_parameters.size(); ++i)
            {
                genStr += generic_parameters[i].TokenLiteral;
                if (i < generic_parameters.size() - 1)
                    genStr += ",";
            }
            genStr += ">";
        }

        std::string ret_str = return_type ? return_type->toString() : "<no type>";
        std::string block_str = block ? block->toString() : "<no block>";

        return "FunctionExpression: " + func_key.TokenLiteral + " " +
               "Function generics: " + genStr +
               "Function parameters: " + cl +
               " Return type: " + ret_str + nullStr +
               " Function block: " + block_str;
    }

    FunctionExpression(Token fn, std::vector<Token> generics, std::vector<std::unique_ptr<Statement>> c, std::unique_ptr<Expression> return_t, bool isNull, std::unique_ptr<Expression> bl) : Expression(fn), func_key(fn), generic_parameters(generics), call(std::move(c)), return_type(std::move(return_t)), isNullable(isNull), block(std::move(bl)) {};
};

// Return type expression
struct ReturnTypeExpression : Expression
{
    Token typeToken;
    std::string toString() override
    {
        return "Type expression: " + typeToken.TokenLiteral;
    }
    ReturnTypeExpression(Token type) : Expression(type), typeToken(type) {};
};

// Tuple expression
struct TupleExpression : Expression
{
    Token lparen;
    std::vector<std::unique_ptr<Expression>> elements;
    std::string toString() override
    {
        std::string out = "Tuple Expression: (";
        for (size_t i = 0; i < elements.size(); ++i)
        {
            out += elements[i]->toString();
            if (i != elements.size() - 1)
                out += ", ";
        }
        out += ")";
        return out;
    }
    TupleExpression(Token lpar, std::vector<std::unique_ptr<Expression>> components) : Expression(lpar), elements(std::move(components)) {};
};

// Error expression
struct ErrorExpression : Expression
{
    Token error_token;
    std::unique_ptr<Expression> err_message;
    std::string toString() override
    {
        return "Error expression: " + err_message->toString();
    };

    ErrorExpression(Token err_token, std::unique_ptr<Expression> message) : Expression(err_token), error_token(err_token), err_message(std::move(message)) {};
};

// Prefix expression node for syntax like !true;
struct PrefixExpression : Expression
{
    Token operat;
    std::unique_ptr<Expression> operand;
    std::string toString() override
    {
        return "Prefix Expression: (" + operat.TokenLiteral + operand->toString() + ")";
    }
    PrefixExpression(Token opr, std::unique_ptr<Expression> oprand)
        : Expression(opr), operat(opr), operand(std::move(oprand)) {}
};

// Postfix expression node for syntax like i++
struct PostfixExpression : Expression
{
    std::unique_ptr<Expression> operand;
    Token operator_token;

    std::string toString() override
    {
        return "Postfix Expression: (" + operand->toString() + operator_token.TokenLiteral + ")";
    }
    PostfixExpression(std::unique_ptr<Expression> op, Token operat) : Expression(operat), operand(std::move(op)), operator_token(operat) {};
};

// Infix Expression node for syntax like x+y;
struct InfixExpression : Expression
{
    std::unique_ptr<Expression> left_operand;
    Token operat;
    std::unique_ptr<Expression> right_operand;
    std::string toString() override
    {
        return "Infix Expression: (" + left_operand->toString() + " " + operat.TokenLiteral + " " + right_operand->toString() + ")";
    }
    InfixExpression(std::unique_ptr<Expression> left, Token op, std::unique_ptr<Expression> right) : Expression(op), left_operand(std::move(left)), operat(op), right_operand(std::move(right)) {};
};

//-----STATEMENTS----

struct ExpressionStatement : Statement
{
    Token expr;
    std::unique_ptr<Expression> expression;
    std::string toString() override
    {
        if (expression)
        {
            return expression->toString() + ";";
        }
        return ";";
    }
    ExpressionStatement(Token exp, std::unique_ptr<Expression> expr) : Statement(exp), expr(exp), expression(std::move(expr)) {};
};

// Break statement node
struct BreakStatement : Statement
{
    Token break_tok;
    std::string toString()
    {
        return "Break Statement: " + break_tok.TokenLiteral;
    }
    BreakStatement(Token break_t) : Statement(break_t), break_tok(break_t) {};
};

// Continue statement struct
struct ContinueStatement : Statement
{
    Token cont_tok;
    std::string toString() override
    {
        return "Continue Statement: " + cont_tok.TokenLiteral;
    }
    ContinueStatement(Token cont_t) : Statement(cont_t), cont_tok(cont_t) {};
};

// Use statement struct
struct UseStatement : Statement
{
    Token use_token;
    Token kind_token; // "data" or "behavior"
    std::unique_ptr<Expression> blockNameOrCall;

    std::string toString() override
    {
        std::string result = "Use statement: ";
        result += kind_token.TokenLiteral + " ";
        result += blockNameOrCall->toString();
        return result + ";";
    }

    UseStatement(Token useTok, Token kindTok, std::unique_ptr<Expression> name)
        : Statement(useTok), use_token(useTok), kind_token(kindTok), blockNameOrCall(std::move(name)) {}
};

// Data statement struct
struct DataStatement : Statement
{
    Mutability mutability;
    Token data_token;
    std::unique_ptr<Expression> dataBlockName;
    std::vector<std::unique_ptr<Statement>> fields;

    std::string toString() override
    {
        std::string mutStr;
        if (mutability == Mutability::MUTABLE)
        {
            mutStr += "mut ";
        }
        else if (mutability == Mutability::CONSTANT)
        {
            mutStr += "const ";
        }
        std::string result = mutStr + " Data statement: " + dataBlockName->toString() + " {\n";
        for (const auto &field : fields)
        {
            result += "  " + field->toString() + "\n";
        }
        result += "}";
        return result;
    }

    DataStatement(Mutability mut, Token data, std::unique_ptr<Expression> block_name, std::vector<std::unique_ptr<Statement>> data_fields) : Statement(data), mutability(mut), data_token(data), dataBlockName(std::move(block_name)), fields(std::move(data_fields)) {};
};

// Behavior statement struct
struct BehaviorStatement : Statement
{
    Token behavior_token;
    std::unique_ptr<Expression> behaviorBlockName;
    std::vector<std::unique_ptr<Statement>> functions;
    std::string toString() override
    {
        std::string result = "Behavior statement: " + behaviorBlockName->toString() + " {\n";
        for (const auto &func : functions)
        {
            result += "  " + func->toString() + "\n";
        }
        result += "}";
        return result;
    }

    BehaviorStatement(Token behavior, std::unique_ptr<Expression> behavior_name, std::vector<std::unique_ptr<Statement>> funcs) : Statement(behavior), behavior_token(behavior), behaviorBlockName(std::move(behavior_name)), functions(std::move(funcs)) {};
};

// Init statement
struct InitStatement : Statement
{
    Token init_token;
    std::vector<std::unique_ptr<Statement>> constructor_args;
    std::unique_ptr<Statement> block;

    std::string toString() override
    {
        std::string args;
        for (auto &arguments : constructor_args)
        {
            args += arguments->toString();
        }
        return "Init Statement: " + init_token.TokenLiteral + " (" + args + ") " + block->toString();
    }

    InitStatement(Token init, std::vector<std::unique_ptr<Statement>> args, std::unique_ptr<Statement> block_content) : Statement(init), constructor_args(std::move(args)), block(std::move(block_content)) {};
};

// Component statement struct
struct ComponentStatement : Statement
{
    Token component_token;
    std::unique_ptr<Expression> component_name;

    std::vector<std::unique_ptr<Statement>> privateData;
    std::vector<std::unique_ptr<Statement>> privateMethods;

    std::vector<std::unique_ptr<Statement>> usedDataBlocks;
    std::vector<std::unique_ptr<Statement>> usedBehaviorBlocks;

    std::optional<std::unique_ptr<Statement>> initConstructor;

    std::string toString() override
    {
        std::string result = "Component Statement: " + component_name->toString() + " {\n";
        // Private data
        for (const auto &field : privateData)
        {
            result += " " + field->toString() + "\n";
        }

        // Private methods
        for (const auto &method : privateMethods)
        {
            result += "  " + method->toString() + "\n";
        }

        // Used data blocks
        for (const auto &use : usedDataBlocks)
        {
            result += "  " + use->toString() + "\n";
        }

        // Used behavior blocks
        for (const auto &use : usedBehaviorBlocks)
        {
            result += "  " + use->toString() + "\n";
        }

        // Init constructor
        if (initConstructor.has_value())
        {
            result += " " + (*initConstructor)->toString();
        }

        result += "}";
        return result;
    }

    ComponentStatement(
        Token component,
        std::unique_ptr<Expression> name,
        std::vector<std::unique_ptr<Statement>> private_data,
        std::vector<std::unique_ptr<Statement>> private_methods,
        std::vector<std::unique_ptr<Statement>> used_data_blocks,
        std::vector<std::unique_ptr<Statement>> used_behavior_blocks,
        std::optional<std::unique_ptr<Statement>> init)
        : Statement(component),
          component_token(component),
          component_name(std::move(name)),
          privateData(std::move(private_data)),
          privateMethods(std::move(private_methods)),
          usedDataBlocks(std::move(used_data_blocks)),
          usedBehaviorBlocks(std::move(used_behavior_blocks)),
          initConstructor(std::move(init)) {}
};

// Error statement
struct ErrorStatement : Statement
{
    Token errorStmt_token;
    std::unique_ptr<Expression> errorExpr;
    std::string toString() override
    {
        return "Error statement: " + errorExpr->toString();
    };
    ErrorStatement(Token err, std::unique_ptr<Expression> errExpr) : Statement(err), errorExpr(std::move(errExpr)) {};
};

// Let statement node
struct LetStatement : Statement
{
    Mutability mutability;
    Token data_type_token;
    bool isNullable = false;
    Token ident_token;
    std::optional<Token> assign_token;
    std::unique_ptr<Expression> value;
    std::string toString() override
    {
        std::string mut_str = "";
        std::string nullStr = "";
        if (isNullable)
        {
            nullStr += "?";
        }
        if (mutability == Mutability::MUTABLE)
            mut_str = "mutable ";
        else if (mutability == Mutability::CONSTANT)
            mut_str = "constant ";

        std::string result = "Let Statement: (" + mut_str + "Data Type: " + data_type_token.TokenLiteral + nullStr +
                             " Variable name: " + ident_token.TokenLiteral;

        if (value)
        {
            result += " Value: " + value->toString();
        }
        else
        {
            result += " Value: <uninitialized>";
        }

        result += ")";
        return result;
    }

    LetStatement(Mutability muta, Token data_t, bool isNull, Token ident_t, std::optional<Token> assign_t, std::unique_ptr<Expression> val) : mutability(muta),
                                                                                                                                              data_type_token(data_t),
                                                                                                                                              isNullable(isNull),
                                                                                                                                              ident_token(ident_t),
                                                                                                                                              assign_token(assign_t),
                                                                                                                                              Statement(data_t),
                                                                                                                                              value(std::move(val)) {};
};

struct AssignmentStatement : Statement
{
    std::unique_ptr<Expression> identifier;
    std::unique_ptr<Expression> value;
    std::string toString() override
    {
        return "Assignment statement: (Variable: " + identifier->toString() + " Value: " + value->toString() + ")";
    };
    AssignmentStatement(std::unique_ptr<Expression> ident, std::unique_ptr<Expression> val) : Statement(ident->token), identifier(std::move(ident)), value(std::move(val)) {};
};

struct FieldAssignment : Statement
{
    Token assignment_token;
    std::unique_ptr<Expression> value;
    std::string toString() override
    {
        return "Field Assignment: (Variable: " + assignment_token.TokenLiteral + " Value: " + value->toString() + ")";
    }

    FieldAssignment(Token ident, std::unique_ptr<Expression> val) : Statement(ident), assignment_token(ident), value(std::move(val)) {};
};

// Signal statement node
struct SignalStatement : Statement
{
    Token signal_token;
    std::unique_ptr<Expression> identifier;
    std::unique_ptr<Statement> tstart;
    std::unique_ptr<Expression> func_arg;

    std::string toString() override
    {
        std::string result = "Signal Statement: " + signal_token.TokenLiteral + " " + identifier->toString() + "=" + tstart->toString() + "(" + func_arg->toString() + ")";
        return result;
    }

    SignalStatement(Token signal, std::unique_ptr<Expression> ident, std::unique_ptr<Statement> thread_st, std::unique_ptr<Expression> arg) : Statement(signal), identifier(std::move(ident)), tstart(std::move(thread_st)), func_arg(move(arg)) {};
};

// Start statement
struct StartStatement : Statement
{
    Token start_tok;
    std::string toString() override
    {
        return "Start Statement: " + start_tok.TokenLiteral;
    }
    StartStatement(Token start) : Statement(start), start_tok(std::move(start)) {};
};

// Wait statement
struct WaitStatement : Statement
{
    Token wait_token;
    std::unique_ptr<Expression> arg;
    std::string toString() override
    {
        return "Wait Statement: " + wait_token.TokenLiteral + "(" + arg->toString() + ")";
    };
    WaitStatement(Token wait, std::unique_ptr<Expression> a) : Statement(wait), arg(std::move(a)) {};
};

// Return statement node
struct ReturnStatement : Statement
{
    Token return_stmt;
    std::unique_ptr<Expression> return_value;
    std::unique_ptr<Statement> error_val;
    std::string toString() override
    {
        return "Return Statement: ( Token: " + return_stmt.TokenLiteral + " Value: " +
               (return_value ? return_value->toString() : "void") + "), Error: " + (error_val ? error_val->toString() : "no error return");
    }
    ReturnStatement(Token ret, std::unique_ptr<Expression> ret_val, std::unique_ptr<Statement> err) : Statement(ret), return_stmt(ret), return_value(std::move(ret_val)), error_val(std::move(err)) {};
};

// Elif statement node
struct elifStatement : Statement
{
    Token elif_token;
    std::unique_ptr<Expression> elif_condition;
    std::unique_ptr<Statement> elif_result;

    std::string toString() override
    {
        return "elifStatement: " + elif_token.TokenLiteral + "(" + elif_condition->toString() + ") {" + elif_result->toString() + "}";
    }

    elifStatement(Token token, std::unique_ptr<Expression> condition, std::unique_ptr<Statement> result) : Statement(token),
                                                                                                           elif_token(token),
                                                                                                           elif_condition(std::move(condition)),
                                                                                                           elif_result(std::move(result)) {};
};

// If statement node
struct ifStatement : Statement
{
    Token if_stmt;
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Statement> if_result;

    std::vector<std::unique_ptr<Statement>> elifClauses;

    std::optional<Token> else_stmt;
    std::optional<std::unique_ptr<Statement>> else_result;

    std::string toString() override
    {
        std::string result = "IfStatement:\n";

        if (condition)
            result += "  if (" + condition->toString() + ") {\n";
        else
            result += "  if (<null condition>) {\n";

        if (if_result)
            result += "    " + if_result->toString() + "\n";
        else
            result += "    <null if_result>\n";

        result += "  }\n";

        if (!elifClauses.empty())
        {
            for (const auto &elifs : elifClauses)
            {
                result += elifs->toString();
            }
        }

        // ELSE block
        if (else_stmt.has_value())
        {
            result += "  else {\n";

            if (else_result.has_value() && *else_result)
                result += "    " + (*else_result)->toString() + "\n";
            else
                result += "    <null else_result>\n";

            result += "  }\n";
        }

        return result;
    }

    ifStatement(Token if_st, std::unique_ptr<Expression> condition_e, std::unique_ptr<Statement> if_r,
                std::vector<std::unique_ptr<Statement>> elifStmts,
                std::optional<Token> else_st, std::optional<std::unique_ptr<Statement>> else_r) : Statement(if_st),
                                                                                                  if_stmt(if_st), condition(std::move(condition_e)), if_result(std::move(if_r)),
                                                                                                  elifClauses(std::move(elifStmts)),
                                                                                                  else_stmt(else_st), else_result(std::move(else_r)) {};
};

// Case clause struct used in switch-case statement
struct CaseClause : Statement
{
    Token case_token;
    std::unique_ptr<Expression> condition;
    std::vector<std::unique_ptr<Statement>> body;

    std::string toString() override
    {
        std::string body_content;
        for (const auto &b : body)
        {
            body_content += "\n    " + b->toString();
        }
        return "case " + condition->toString() + ":" + body_content;
    }

    CaseClause(Token token, std::unique_ptr<Expression> cond, std::vector<std::unique_ptr<Statement>> stmt)
        : Statement(token), case_token(token), condition(std::move(cond)), body(std::move(stmt)) {}
};

struct SwitchStatement : Statement
{
    Token switch_token;
    std::unique_ptr<Expression> switch_expr;
    // Inside the braces
    // Cases
    std::vector<std::unique_ptr<Statement>> case_clauses;
    // Default case
    Token default_token;
    std::vector<std::unique_ptr<Statement>> default_statements;

    std::string toString() override
    {
        std::string result = "switch (" + switch_expr->toString() + ") {\n";

        // Print all case clauses
        for (const auto &clause : case_clauses)
        {
            result += clause->toString() + "\n";
        }

        // Print default statements if they exist
        if (!default_statements.empty())
        {
            result += "\n  default:\n";
            for (const auto &stmt : default_statements)
            {
                result += "    " + stmt->toString() + "\n";
            }
        }

        result += "}";
        return result;
    }

    SwitchStatement(Token token, std::unique_ptr<Expression> expr, std::vector<std::unique_ptr<Statement>> cases, Token default_tok, std::vector<std::unique_ptr<Statement>> default_stmts)
        : Statement(token), switch_token(token), switch_expr(std::move(expr)), case_clauses(std::move(cases)), default_token(default_tok), default_statements(std::move(default_stmts)) {}
};

struct EnumMember : Node
{
    std::string enumMember;
    std::unique_ptr<Expression> value;

    std::string toString() override
    {
        std::string val;
        if (value)
        {
            val += "= " + value->toString();
        }
        return "Enum Member: " + enumMember + val;
    }

    EnumMember(std::string member, std::unique_ptr<Expression> val) : enumMember(member), value(std::move(val)) {};
};

// Enum class struct
struct EnumClassStatement : Statement
{
    Token enum_token;
    Token class_token;
    std::unique_ptr<Expression> enum_identifier;
    std::optional<Token> int_type;
    std::vector<std::unique_ptr<EnumMember>> enum_content;
    std::string toString()
    {
        std::string enum_block;
        std::string int_typestr = "";

        if (int_type.has_value())
        {
            int_typestr = " : " + int_type.value().TokenLiteral;
        }

        for (const auto &enum_cont : enum_content)
        {
            enum_block += enum_cont->toString() + ",\n";
        }

        return "Enum class statement: " + enum_identifier->toString() + int_typestr + " { " + enum_block + " }";
    }

    EnumClassStatement(Token enum_tok, Token class_tok, std::unique_ptr<Expression> enum_ident, std::optional<Token> intType, std::vector<std::unique_ptr<EnumMember>> enum_block) : Statement(enum_tok), enum_token(enum_tok),
                                                                                                                                                                                     class_token(class_tok),
                                                                                                                                                                                     enum_identifier(std::move(enum_ident)),
                                                                                                                                                                                     int_type(intType),
                                                                                                                                                                                     enum_content(std::move(enum_block)) {};
};

struct ForStatement : Statement
{
    Token for_key;
    std::unique_ptr<Statement> initializer; // int i;
    std::unique_ptr<Expression> condition;  // i < 10
    std::unique_ptr<Expression> step;       // i = i + 1
    std::unique_ptr<Statement> body;        // the loop body

    ForStatement(Token for_k,
                 std::unique_ptr<Statement> init,
                 std::unique_ptr<Expression> cond,
                 std::unique_ptr<Expression> step,
                 std::unique_ptr<Statement> body)
        : Statement(for_k),
          for_key(for_k),
          initializer(std::move(init)),
          condition(std::move(cond)),
          step(std::move(step)),
          body(std::move(body)) {};

    std::string toString() override
    {
        std::string out = "ForStatement(\n";
        out += "  Init: " + (initializer ? initializer->toString() : "null") + "\n";
        out += "  Cond: " + (condition ? condition->toString() : "null") + "\n";
        out += "  Step: " + (step ? step->toString() : "null") + "\n";
        out += "  Body: " + (body ? body->toString() : "null") + "\n";
        out += ")";
        return out;
    }
};

struct EachStatement : Statement
{
    Token each_key;
    std::unique_ptr<Statement> iteratorVar;
    std::unique_ptr<Expression> iterable;
    std::unique_ptr<Statement> body;

    EachStatement(Token for_k,
                  std::unique_ptr<Statement> iterVar,
                  std::unique_ptr<Expression> iterable,
                  std::unique_ptr<Statement> body)
        : Statement(for_k),
          each_key(for_k),
          iteratorVar(std::move(iterVar)),
          iterable(std::move(iterable)),
          body(std::move(body)) {}

    std::string toString() override
    {
        std::string out = "EachStatement(\n";
        out += "  Var: " + (iteratorVar ? iteratorVar->toString() : "null") + "\n";
        out += "  Iterable: " + (iterable ? iterable->toString() : "null") + "\n";
        out += "  Body: " + (body ? body->toString() : "null") + "\n";
        out += ")";
        return out;
    }
};

struct WhileStatement : Statement
{
    Token while_key;
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Statement> loop;

    std::string toString() override
    {
        return "While : " + condition->toString() + loop->toString();
    }

    WhileStatement(Token while_k, std::unique_ptr<Expression> condition, std::unique_ptr<Statement> l) : Statement(while_k), while_key(while_k), condition(std::move(condition)), loop(std::move(l)) {};
};

// Function Statement
struct FunctionStatement : Statement
{
    Token token;
    std::unique_ptr<Expression> funcExpr;
    std::string toString() override
    {
        return "Function Statement: " + funcExpr->toString();
    }
    FunctionStatement(Token funcStmtTok, std::unique_ptr<Expression> expr) : Statement(funcStmtTok), funcExpr(std::move(expr)) {};
};

// Function declaration statement
struct FunctionDeclaration : Statement
{

    Token work_keyword_token;
    std::vector<Token> genericParams;
    std::unique_ptr<Expression> function_name;
    std::vector<std::unique_ptr<Statement>> parameters;
    std::unique_ptr<Expression> return_type;
    bool isNullable = false;

    std::string toString() override
    {
        std::string arguments;
        for (auto &param : parameters)
        {
            arguments += param->toString();
        }
        std::string nullStr = "";
        if (isNullable)
        {
            nullStr += "?";
        }

        std::string genStr = "";
        if (!genericParams.empty())
        {
            genStr += "<";
            for (size_t i = 0; i < genericParams.size(); ++i)
            {
                genStr += genericParams[i].TokenLiteral;
                if (i < genericParams.size() - 1)
                    genStr += ",";
            }
            genStr += ">";
        }

        return "Function Declaration Statement: " + work_keyword_token.TokenLiteral + " " +
               genStr +
               (function_name ? function_name->toString() : "[null_name]") + " " +
               arguments + " " +
               (return_type ? return_type->toString() : "[no_return]") + nullStr;
    }

    FunctionDeclaration(Token work, std::vector<Token> generics, std::unique_ptr<Expression> identifier, std::vector<std::unique_ptr<Statement>> params, std::unique_ptr<Expression> ret_type, bool isNull) : Statement(work),
                                                                                                                                                                                                              work_keyword_token(work),
                                                                                                                                                                                                              genericParams(generics),
                                                                                                                                                                                                              function_name(std::move(identifier)),
                                                                                                                                                                                                              parameters(std::move(params)),
                                                                                                                                                                                                              return_type(std::move(ret_type)),
                                                                                                                                                                                                              isNullable(isNull) {};
};

// Function Declaration expression
struct FunctionDeclarationExpression : Expression
{
    Token work_token;
    std::unique_ptr<Statement> funcDeclrStmt;
    std::string toString() override
    {
        return "Function Declaration Expression: " + funcDeclrStmt->toString();
    }
    FunctionDeclarationExpression(Token work, std::unique_ptr<Statement> declaration) : Expression(work), funcDeclrStmt(std::move(declaration)) {};
};

// Block statement
struct BlockStatement : Statement
{
    Token brace;
    std::vector<std::unique_ptr<Statement>> statements;
    std::string toString() override
    {
        std::string out = "Block Statement: { ";
        for (const auto &s : statements)
        {
            if (s)
            {
                out += s->toString();
            }
        }
        out += " }";
        return out;
    }
    BlockStatement(Token brac, std::vector<std::unique_ptr<Statement>> cont) : Statement(brac), brace(brac), statements(std::move(cont)) {}
};

// BLOCKS
//  Block expression
struct BlockExpression : Expression
{
    Token brace;
    std::vector<std::unique_ptr<Statement>> statements;
    std::optional<std::unique_ptr<Expression>> finalexpr;

    std::string toString() override
    {
        std::string out = "BlockExpression: { \n";
        for (auto &stmt : statements)
        {
            out += stmt->toString() + "} \n";
        }
        if (finalexpr.has_value() && finalexpr.value())
        {
            out += "Final Expression: " + finalexpr.value()->toString();
        }
        return out;
    };

    BlockExpression(Token lbrace) : Expression(lbrace) {};
};

enum class Precedence
{
    PREC_NONE = 0,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // ||
    PREC_AND,        // &&
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // "* /"
    PREC_UNARY,      // "! -"
    PREC_POSTFIX,
    PREC_CALL, // . ()
    PREC_PRIMARY
};