#include <utility>
#include <algorithm>
#include <functional>
#include <string>
#include <string_view>
#include <map>
#include <sstream>
#include <unordered_set>
#include <algorithm>
#include <variant>
#include <iostream>
#include <memory>
#include <vector>
#include <type_traits>
#include <typeindex>
#include <typeinfo>

#include <profileparser/types.hpp>
#include <profileparser/utils.hpp>
#include <profileparser.hpp>
#include "parser.hpp"
#include "peglib.h"

using namespace std::string_view_literals;

namespace Profile {

constexpr char profile_grammar[] = R"GRAMMAR(
Document    <- S? Object S? EndOfFile
~EndOfLine  <- '\r\n' / '\n' / '\r'
~EndOfFile  <- !'.'
Comment    <- ('#' / '//') <(!EndOfLine .)*> &EndOfLine
~Whitespace <- [ \t]+
~S          <- (Whitespace / Comment / EndOfLine)+
SCHAR       <- (!'${' !EndOfLine !["].)*
DocString   <- ((Whitespace / EndOfLine)* Comment EndOfLine)+

# Basic types
Identifier  <- [a-zA-Z_][a-zA-Z_0-9]*
Reference   <- Identifier
String      <- '"' SCHAR '"'^missing_par
Boolean     <- 'false' / 'true'
Unsigned    <- [0-9]+
Sign        <- [-+]
Integer     <- Sign? Unsigned
FloatDot    <- (Integer '.' Unsigned?) / (Sign? Unsigned? '.' Unsigned)
FloatExp    <- (FloatDot / Integer) [eE] (FloatDot / Integer)
Float       <- FloatExp / FloatDot

# TemplateString
TemplateReference <- '${' Whitespace* Reference^missing_id Whitespace* '}'^missing_braces SCHAR
TemplateString    <- '"' SCHAR (TemplateReference)+  '"'^missing_par

StringExpr        <- TemplateString / String

# Expressions (References, Numbers, Operations)
MulOp       <- [*/]
SumOp       <- [-+]
Operand     <- Reference / Float / Integer
Factor      <- Sign? S? Operand / Sign? S? '(' S? Sum S? ')'
Product     <- Factor  (S? MulOp S? Factor )*
Sum         <- Product (S? SumOp S? Product)*
Expr        <- Sum

# Container types
List        <- '[' S? (Value (S? ',' S? Value)* S? ','?)? S? ']'^missing_set_braces
KeyVal      <- (Identifier / String) S? ':' S? Value
Dict        <- '{' S? (KeyVal (S? ',' S? KeyVal)* S? ','?)? S? '}'^missing_set_braces
Value       <- Object / Dict / List / StringExpr / Boolean / Expr

# Object type
ClassName   <- Identifier ('.' Identifier)*
AttrValue   <- Value
Attribute   <- Identifier S? ':' S? AttrValue
ObjectHead  <- ClassName (S Identifier)?
ObjectItem  <- Object / Attribute
Object      <- ObjectHead S? '{' DocString? S? ObjectItem? (S ObjectItem)* S? '}'

missing_id         <- '' { error_message "unexpected %t, missing reference" }
missing_braces     <- '' { error_message "unexpected %t, missing closing braces" }
missing_set_braces <- '' { error_message "unexpected %t, missing closing braces for set" }
missing_par        <- Whitespace* EndOfLine  { error_message "missing closing parentheses" }
)GRAMMAR";

inline SourceInfo source_info(const peg::SemanticValues& vs) {
    return SourceInfo{
        .offset=static_cast<size_t>(std::distance(vs.ss, vs.sv().data())),
        .length=vs.sv().length(),
    };
}

// Helper class for expression evaluation

struct Expression
{
    enum class Op : char
    {
        Plus = '+',
        Minus = '-',
        Mul = '*',
        Div = '/',
    };
    using Constant = std::variant<Integer, Float>;
    using Value = std::variant<Constant, ReferenceNodePtr, NumericExpressionNodePtr>;
    Value value;

    void apply(const Op op, Expression& rhs) { apply(op, rhs.value); }

    void apply(const Op op, Value& rhs)
    {
        // Evaluate constant expression
        if (std::holds_alternative<Constant>(value) && std::holds_alternative<Constant>(rhs)) {
            value = evaluate(std::get<Constant>(value), std::get<Constant>(rhs), op);
            return;
        }
        // Merge references from rhs to expression
        to_expression(value);
        to_expression(rhs);
        for (const auto& ref : std::get<NumericExpressionNodePtr>(rhs)->references) {
            std::get<NumericExpressionNodePtr>(value)->references.emplace_back(ref);
        }
    }

    static void to_expression(Value& value)
    {
        if (std::holds_alternative<Constant>(value)) {
            value = std::make_shared<NumericExpressionNode>();
        }
        else if (std::holds_alternative<ReferenceNodePtr>(value)) {
            ReferenceNodePtr ref = std::get<ReferenceNodePtr>(value);
            value = std::make_shared<NumericExpressionNode>(NumericExpressionNode{
                .source_info=ref->source_info,
                .value="",
                .references=std::vector<ReferenceNodePtr>{ref},
            });
        }
    }

    static Constant evaluate(const Constant& lhs, const Constant& rhs, const Op op)
    {
        return std::visit([&](const auto v1, const auto v2) -> Constant {
            switch (op) {
            case Op::Plus:
                return { v1 + v2 };
            case Op::Minus:
                return { v1 - v2 };
            case Op::Mul:
                return { v1 * v2 };
            default: /* Op::Div */
                return { v1 / Float(v2) };
            }
        }, lhs, rhs);
    }

    static inline Expression fromReference(ReferenceNodePtr name)
    {
        return { std::move(name) };
    }

    template<typename T> static inline Expression fromConstant(const T v)
    {
        return { Constant { v } };
    }
};

// Instantiation of parser, setting up parse actions
peg::parser create_parser()
{
    peg::parser parser;
    if (!parser.load_grammar(profile_grammar)) {
        throw std::runtime_error("Failed to load profile grammar");
    }

    parser["Identifier"] = [](const peg::SemanticValues& vs) -> std::string {
        return vs.token_to_string();
    };

    parser["SCHAR"] = [](const peg::SemanticValues& vs) -> std::string {
        return vs.token_to_string();
    };

    parser["Reference"] = [](peg::SemanticValues& vs) -> ReferenceNodePtr {
        return std::make_shared<ReferenceNode>(ReferenceNode{
            .source_info=source_info(vs),
            .reference=std::move(std::any_cast<std::string&>(vs[0])),
        });
    };

    parser["Boolean"] = [](const peg::SemanticValues& vs) -> bool {
        // 'false' / 'true'
        return vs.choice() != 0;
    };

    parser["String"] = [](peg::SemanticValues& vs) -> String {
        return std::move(std::any_cast<String&>(vs[0]));
    };

    parser["Comment"] = [](peg::SemanticValues& vs) -> std::string {
        return vs.token_to_string();
    };

    parser["DocString"] = [](const peg::SemanticValues& vs) -> DocString {
        // ((Whitespace / EndOfLine)* Comment EndOfLine)+
        const auto getLines = [&]() {
            std::vector<std::string> lines;
            lines.reserve(vs.size());
            for (size_t i = 0; i < vs.size(); ++i) {
                lines.push_back(std::any_cast<std::string>(vs[i]));
            }
            return lines;
        };
        return DocString {
            .source_info=source_info(vs),
            .lines=getLines(),
        };
    };

    parser["TemplateReference"] = [](const peg::SemanticValues& vs) -> std::pair<ReferenceNodePtr, std::string> {
        return { std::any_cast<ReferenceNodePtr>(vs[0]), std::any_cast<std::string>(vs[1]) };
    };

    parser["TemplateString"] = [](peg::SemanticValues& vs) -> StringExpressionNodePtr {
        // SCHAR (TemplateReference)+
        auto expr = std::make_shared<StringExpressionNode>(StringExpressionNode{
            .source_info=source_info(vs),
            .fragments={},
        });
        auto& fragments = expr->fragments;
        fragments.push_back(std::move(std::any_cast<std::string&>(vs[0])));
        for(std::size_t i = 1; i < vs.size(); ++i) {
            auto& [ref, str] = std::any_cast<std::pair<ReferenceNodePtr, std::string>&>(vs[i]);
            fragments.push_back(std::move(ref));
            fragments.push_back(std::move(str));
        }
        return expr;
    };

    parser["StringExpr"] = [](peg::SemanticValues& vs) -> Value {
        // TemplateString / String
        if(vs.choice() == 0) {
            return std::move(std::any_cast<StringExpressionNodePtr&>(vs[0]));
        } else {
            return std::move(std::any_cast<String&>(vs[0]));
        }
    };

    parser["Integer"] = [](const peg::SemanticValues& vs) -> Integer {
        return vs.token_to_number<Integer>();
    };

    parser["Float"] = [](const peg::SemanticValues& vs) -> Float {
        return vs.token_to_number<Float>();
    };

    parser["List"] = [](peg::SemanticValues& vs) -> ListNodePtr {
        // Zero or more Value items
        ListNodePtr list = std::make_shared<ListNode>(ListNode{
            .source_info=source_info(vs),
            .id={},
            .items={},
        });
        auto& items = list->items;
        items.reserve(vs.size());
        for (auto& v : vs) {
            auto& item = std::any_cast<Value&>(v);
            items.emplace_back(std::move(item));
        }
        return list;
    };

    parser["KeyVal"] = [](peg::SemanticValues& vs) -> KeyVal {
        // (Identifier / String) S? ':' S? Value
        auto& id = std::any_cast<std::string&>(vs[0]);
        auto& value = std::any_cast<Value&>(vs[1]);
        return { std::move(id), std::move(value) };
    };

    parser["Dict"] = [](peg::SemanticValues& vs) -> DictNodePtr {
        // Zero or more KeyVal items
        DictNodePtr dct = std::make_shared<DictNode>(DictNode{
            .source_info=source_info(vs),
            .id={},
            .items={},
        });
        auto& items = dct->items;
        items.reserve(vs.size());
        for (auto& kv : vs) {
            auto& item = std::any_cast<KeyVal&>(kv);
            items.emplace_back(std::move(item));
        }
        return dct;
    };

    parser["Value"] = [](peg::SemanticValues& vs) -> Value {
        // Object / Dict / List / StringExpr / Boolean / Expr
        switch (vs.choice()) {
        case 0:
            return std::move(std::any_cast<ObjectNodePtr&>(vs[0]));
        case 1:
            return std::move(std::any_cast<DictNodePtr&>(vs[0]));
        case 2:
            return std::move(std::any_cast<ListNodePtr&>(vs[0]));
        case 3:
            return std::move(std::any_cast<Value&>(vs[0]));
        case 4:
            return std::move(std::any_cast<Boolean>(vs[0]));
        default:
            return std::move(std::any_cast<Value&>(vs[0]));
        }
    };

    constexpr auto parse_op = [](const peg::SemanticValues& vs) {
        return static_cast<Expression::Op>(vs.sv().front());
    };
    parser["Sign"] = parse_op;
    parser["MulOp"] = parse_op;
    parser["SumOp"] = parse_op;

    parser["Operand"] = [](peg::SemanticValues& vs, std::any& ctx) -> Expression {
        // Reference / Float / Integer
        switch (vs.choice()) {
        case 0: {
            auto& reference = std::any_cast<ReferenceNodePtr&>(vs[0]);
            // Substitute reference with constant if defined by context
            if (ctx.has_value()) {
                const auto& subs = *std::any_cast<ValueSubstitutions*>(ctx);
                if (auto it = subs.find(reference->reference); it != subs.end()) {
                    const Value& value = it->second;
                    if (auto v = std::get_if<Float>(&value)) {
                        return Expression::fromConstant(*v);
                    }
                    if (auto v = std::get_if<Integer>(&value)) {
                        return Expression::fromConstant(*v);
                    }
                    throw std::runtime_error("Cannot use '" + reference->reference + "' within expression");
                }
            }
            // Else return operand as reference
            return Expression::fromReference(std::move(reference));
        }
        case 1:
            return Expression::fromConstant(std::any_cast<Float>(vs[0]));
        default:
            return Expression::fromConstant(std::any_cast<Integer>(vs[0]));
        }
    };

    parser["Factor"] = [](peg::SemanticValues& vs) -> Expression {
        // Sign? S? Operand / Sign? S? '(' S? Expr S? ')'
        const auto with_sign = (vs.size() == 2);
        const auto sign = with_sign ? std::any_cast<Expression::Op>(vs[0]) : Expression::Op::Plus;
        Expression& result = std::any_cast<Expression&>(vs[with_sign ? 1 : 0]);
        // Multiply expression result by -1 if sign is negative
        if (sign == Expression::Op::Minus) {
            auto minus_one = Expression::fromConstant(-1l);
            result.apply(Expression::Op::Mul, minus_one);
        }
        return std::move(result);
    };

    constexpr auto reduce_expr = [](peg::SemanticValues& vs) -> Expression {
        // Operand (Op Operand)*
        auto& result = std::any_cast<Expression&>(vs[0]);
        for (auto i = 1u; i < vs.size(); i += 2) {
            result.apply(std::any_cast<Expression::Op>(vs[i]), std::any_cast<Expression&>(vs[i+1]));
        }
        return std::move(result);
    };
    parser["Product"] = reduce_expr;
    parser["Sum"] = reduce_expr;

    parser["Expr"] = [](peg::SemanticValues& vs) -> Value {
        struct Visitor {
            peg::SemanticValues& vs;

            Value operator()(Expression::Constant& c) {
                return std::visit([](const auto v) -> Value { return v; }, c);
            }
            Value operator()(ReferenceNodePtr& ref) {
                return std::move(ref);
            }
            Value operator()(NumericExpressionNodePtr& expr) {
                expr->source_info = source_info(vs);
                expr->value = vs.token();  // Remember full expression
                return std::move(expr);
            }
        };
        return std::visit(Visitor { vs }, std::any_cast<Expression&>(vs[0]).value);
    };

    parser["AttrValue"] = [](peg::SemanticValues& vs) -> AttributeNodePtr {
        // Value
        return std::make_shared<AttributeNode>(AttributeNode{
            .source_info = {},  // deferred
            .value_source_info = source_info(vs),
            .name = {},  // deferred
            .value = std::move(std::any_cast<Value&>(vs[0])),
        });
    };

    parser["Attribute"] = [](peg::SemanticValues& vs) -> AttributeNodePtr {
        // Identifier ':' S? AttrValue
        auto& attribute = std::any_cast<AttributeNodePtr&>(vs[1]);
        attribute->source_info = source_info(vs);
        attribute->name = std::move(std::any_cast<std::string&>(vs[0]));
        return std::move(attribute);
    };

    parser["ObjectItem"] = [](peg::SemanticValues& vs) -> ObjectNode::Item {
        // Object / Attribute
        switch (vs.choice()) {
        case 0: return std::move(std::any_cast<ObjectNodePtr&>(vs[0]));
        default: return std::move(std::any_cast<AttributeNodePtr&>(vs[0]));
        }
    };

    parser["ClassName"] = [](const peg::SemanticValues& vs) -> std::string {
        return vs.token_to_string();
    };

    parser["ObjectHead"] = [](peg::SemanticValues& vs) -> ObjectNodePtr {
        // ClassName (S Identifier)?
        return std::make_shared<ObjectNode>(ObjectNode{
            .source_info={},
            .docstring={},
            .classname=std::move(std::any_cast<std::string&>(vs[0])),
            .id=(vs.size() > 1) ? std::move(std::any_cast<std::string&>(vs[1])) : "",
            .children={},
            .attributes={},
        });
    };

    parser["Object"] = [](peg::SemanticValues& vs) -> ObjectNodePtr {
        // ObjectHead S? '{' DocString? S? ObjectItem? (S ObjectItem)* S? '}'
        auto& obj = std::any_cast<ObjectNodePtr&>(vs[0]);
        obj->source_info = source_info(vs);
        size_t item_idx = 1;
        if(vs.size() > 1 && vs[1].type() == std::type_index(typeid(DocString))) {
            obj->docstring = std::any_cast<DocString>(vs[1]);
            item_idx++;
        }
        for (size_t i = item_idx; i < vs.size(); ++i) {
            auto& item = std::any_cast<ObjectNode::Item&>(vs[i]);
            if (std::holds_alternative<ObjectNodePtr>(item)) {
                obj->children.emplace_back(std::move(std::get<ObjectNodePtr>(item)));
            } else {
                obj->attributes.emplace_back(std::move(std::get<AttributeNodePtr>(item)));
            }
        }
        return std::move(obj);
    };

    parser["Document"] = [](peg::SemanticValues& vs) -> ObjectNodePtr {
        return std::move(std::any_cast<ObjectNodePtr&>(vs[0]));
    };

    // parser.enable_packrat_parsing();  // Seems to be broken in JS, heap corruption?

    return parser;
}

const peg::parser& default_parser_instance()
{
    static const peg::parser parser = create_parser();
    return parser;
}

// External interface

ParsedProfile parse(std::string source)
{
    auto parser = default_parser_instance(); // copy to set logger

    std::vector<ErrorLine> errors;
    parser.set_logger([&](size_t line, size_t pos, const std::string& msg, const std::string& rule) {
        errors.emplace_back(ErrorLine{
            .where=LineInfo{.line=line, .column=pos},
            .message=msg + " (" + rule + ')',
        });
    });

    ObjectNodePtr profile;
    auto result = parser.parse(source, profile);
    if (!result) {
        throw format_error("Failed parsing profile", std::move(errors));
    }

    auto ids = collect_ids(profile, source);
    return ParsedProfile {
        .source=std::move(source),
        .root=std::move(profile),
        .ids=std::move(ids),
    };
}

Value parseExpression(const std::string_view& expr, const ValueSubstitutions& values)
{
    Value value;
    const auto& parser = default_parser_instance();
    std::any ctx { &values };
    const auto p = parser["Expr"].parse_and_get_value(expr.data(), expr.length(), ctx, value);
    return value;
}

} // namespace Profile
