#pragma once

#include "peglib.h"
#include <utility>
#include <algorithm>
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

namespace Profile {

using namespace std::string_view_literals;

constexpr char profile_grammar[] = R"GRAMMAR(
Document    <- S? Object S? EndOfFile
~EndOfLine  <- '\r\n' / '\n' / '\r'
~EndOfFile  <- !'.'
~Comment    <- ('#' / '//') (!EndOfLine .)* &EndOfLine
~Whitespace <- [ \t]+
~S          <- (Whitespace / Comment / EndOfLine)+

# Basic types
Identifier  <- [a-zA-Z_][a-zA-Z_0-9]*
Reference   <- Identifier
String      <- ["] (!["].)* ["]
Boolean     <- 'false' / 'true'
Unsigned    <- [0-9]+
Sign        <- [-+]
Integer     <- Sign? Unsigned
FloatDot    <- (Integer '.' Unsigned?) / (Sign? Unsigned? '.' Unsigned)
FloatExp    <- (FloatDot / Integer) [eE] (FloatDot / Integer)
Float       <- FloatExp / FloatDot

# Expressions (References, Numbers, Operations)
MulOp       <- [*/]
SumOp       <- [-+]
Operand     <- Reference / Float / Integer
Factor      <- Sign? S? Operand / Sign? S? '(' S? Sum S? ')'
Product     <- Factor  (S? MulOp S? Factor )*
Sum         <- Product (S? SumOp S? Product)*
Expr        <- Sum

# Container types
List        <- '[' S? (Value (S? ',' S? Value)* S? ','?)? S? ']'
KeyVal      <- (Identifier / String) S? ':' S? Value
Dict        <- '{' S? (KeyVal (S? ',' S? KeyVal)* S? ','?)? S? '}'
Value       <- Object / Dict / List / String / Boolean / Expr

# Object type
ClassName   <- Identifier ('.' Identifier)*
AttrValue   <- Value
Attribute   <- Identifier S? ':' S? AttrValue
ObjectHead  <- ClassName (S Identifier)?
ObjectItem  <- Object / Attribute
Object      <- ObjectHead S? '{' S? ObjectItem? (S ObjectItem)* S? '}'
)GRAMMAR";

// Cpp type definitions for parsed elements

using Boolean = bool;

using Integer = long;

using Float = double;

using String = std::string;

struct Reference : public std::string
{ };

struct RawExpression
{
    std::string str;
    std::unordered_set<std::string> references;
};

struct List;
using ListPtr = std::shared_ptr<List>;

struct Dict;
using DictPtr = std::shared_ptr<Dict>;

struct Object;
using ObjectPtr = std::shared_ptr<Object>;
using ObjectMap = std::unordered_map<std::string, Object*>;

using Value = std::variant<
    ObjectPtr, DictPtr, ListPtr, String, Boolean,
    Integer, Float, Reference, RawExpression
    >;

using ValuePtr = std::shared_ptr<Value>;

template<typename T> static inline ValuePtr valuePtrFrom(T&& v) {
    return std::make_shared<Value>(std::forward<T>(v));
}

using KeyVal = std::pair<std::string, Value>;

struct Dict : public std::vector<KeyVal>
{ };

struct List : public std::vector<Value>
{ };

struct Attribute
{
    std::string name;
    ValuePtr data;
    std::pair<size_t, size_t> src_loc;
};
using AttributeMap = std::unordered_map<std::string, Attribute*>;

struct Object
{
    using Item = std::variant<ObjectPtr, Attribute>;

    std::string classname;
    std::string id;
    std::vector<Attribute> attributes;
    std::vector<ObjectPtr> children;

    Attribute* find_attribute(const std::string& name)
    {
        auto it = std::find_if(attributes.begin(), attributes.end(), [&](const auto& a) { return a.name == name; });
        if (it == attributes.end()) {
            return nullptr;
        }
        return &(*it);
    }
};

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
    using Value = std::variant<Constant, Reference, RawExpression>;
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
        for (const auto& ref : std::get<RawExpression>(rhs).references) {
            std::get<RawExpression>(value).references.emplace(ref);
        }
    }

    static void to_expression(Value& value)
    {
        if (std::holds_alternative<Constant>(value)) {
            value = RawExpression {};
        }
        else if (std::holds_alternative<Reference>(value)) {
            std::string& ref = std::get<Reference>(value);
            value = RawExpression { {}, { std::move(ref) } };
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

    static inline Expression fromReference(Reference name)
    {
        return { std::move(name) };
    }

    template<typename T> static inline Expression fromConstant(const T v)
    {
        return { Constant { v } };
    }
};

using ValueSubstitutions = std::unordered_map<std::string, ValuePtr>;

// Instantiation of parser, setting up parse actions
inline peg::parser create_parser()
{
    peg::parser parser;
    if (!parser.load_grammar(profile_grammar)) {
        throw std::runtime_error("Failed to load profile grammar");
    }

    parser["Identifier"] = [](const peg::SemanticValues& vs) -> std::string {
        return vs.token_to_string();
    };

    parser["Reference"] = [](peg::SemanticValues& vs) -> Reference {
        return Reference { std::move(std::any_cast<std::string&>(vs[0])) };
    };

    parser["Boolean"] = [](const peg::SemanticValues& vs) -> bool {
        // 'false' / 'true'
        return vs.choice() != 0;
    };

    parser["String"] = [](const peg::SemanticValues& vs) -> std::string {
        const std::string_view sv = vs.sv();
        return { sv.data() + 1, sv.size() - 2 };
    };

    parser["Integer"] = [](const peg::SemanticValues& vs) -> Integer {
        return vs.token_to_number<Integer>();
    };

    parser["Float"] = [](const peg::SemanticValues& vs) -> Float {
        return vs.token_to_number<Float>();
    };

    parser["List"] = [](peg::SemanticValues& vs) -> ListPtr {
        // Zero or more Value items
        ListPtr list = std::make_shared<List>();
        list->reserve(vs.size());
        for (auto& v : vs) {
            auto& item = std::any_cast<ValuePtr&>(v);
            list->emplace_back(std::move(*item));
        }
        return list;
    };

    parser["KeyVal"] = [](peg::SemanticValues& vs) -> KeyVal {
        // (Identifier / String) S? ':' S? Value
        auto& id = std::any_cast<std::string&>(vs[0]);
        auto& value = std::any_cast<ValuePtr&>(vs[1]);
        return { std::move(id), std::move(*value) };
    };

    parser["Dict"] = [](peg::SemanticValues& vs) -> DictPtr {
        // Zero or more KeyVal items
        DictPtr dct = std::make_shared<Dict>();
        dct->reserve(vs.size());
        for (auto& kv : vs) {
            auto& item = std::any_cast<KeyVal&>(kv);
            dct->emplace_back(std::move(item));
        }
        return dct;
    };

    parser["Value"] = [](peg::SemanticValues& vs) -> ValuePtr {
        // Object / Dict / List / String / Boolean / Expr
        switch (vs.choice()) {
        case 0:
            return valuePtrFrom(std::move(std::any_cast<ObjectPtr&>(vs[0])));
        case 1:
            return valuePtrFrom(std::move(std::any_cast<DictPtr&>(vs[0])));
        case 2:
            return valuePtrFrom(std::move(std::any_cast<ListPtr&>(vs[0])));
        case 3:
            return valuePtrFrom(std::move(std::any_cast<String&>(vs[0])));
        case 4:
            return valuePtrFrom(std::any_cast<Boolean>(vs[0]));
        default:
            return std::move(std::any_cast<ValuePtr&>(vs[0]));
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
            auto& reference = std::any_cast<Reference&>(vs[0]);
            // Substitute reference with constant if defined by context
            if (ctx.has_value()) {
                const auto& subs = *std::any_cast<ValueSubstitutions*>(ctx);
                if (auto it = subs.find(reference); it != subs.end()) {
                    const Value& value = *(it->second);
                    if (auto v = std::get_if<Float>(&value)) {
                        return Expression::fromConstant(*v);
                    }
                    if (auto v = std::get_if<Integer>(&value)) {
                        return Expression::fromConstant(*v);
                    }
                    throw std::runtime_error("Cannot use '" + reference + "' within expression");
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

    parser["Expr"] = [](peg::SemanticValues& vs) -> ValuePtr {
        struct Visitor {
            peg::SemanticValues& vs;

            ValuePtr operator()(Expression::Constant& c) {
                return std::visit([](const auto v) -> ValuePtr { return valuePtrFrom(v); }, c);
            }
            ValuePtr operator()(Reference& ref) {
                return valuePtrFrom(std::move(ref));
            }
            ValuePtr operator()(RawExpression& expr) {
                expr.str = vs.token();  // Remember full expression
                return valuePtrFrom(std::move(expr));
            }
        };
        return std::visit(Visitor { vs }, std::any_cast<Expression&>(vs[0]).value);
    };

    parser["AttrValue"] = [](peg::SemanticValues& vs) -> Attribute {
        const auto pos = static_cast<size_t>(vs.sv().data() - vs.ss);
        const std::pair<size_t, size_t> src_loc {pos, vs.sv().length()};
        return Attribute {
            {}, std::move(std::any_cast<ValuePtr&>(vs[0])), src_loc
        };
    };

    parser["Attribute"] = [](peg::SemanticValues& vs) -> Attribute {
        // Identifier ':' S? AttrValue
        auto& attribute = std::any_cast<Attribute&>(vs[1]);
        attribute.name = std::move(std::any_cast<std::string&>(vs[0]));
        return std::move(attribute);
    };

    parser["ObjectItem"] = [](peg::SemanticValues& vs) -> Object::Item {
        // Object / Attribute
        switch (vs.choice()) {
        case 0: return std::move(std::any_cast<ObjectPtr&>(vs[0]));
        default: return std::move(std::any_cast<Attribute&>(vs[0]));
        }
    };

    parser["ClassName"] = [](const peg::SemanticValues& vs) -> std::string {
        return vs.token_to_string();
    };

    parser["ObjectHead"] = [](peg::SemanticValues& vs) -> ObjectPtr {
        // ClassName (S Identifier)?
        ObjectPtr obj = std::make_shared<Object>();
        obj->classname = std::move(std::any_cast<std::string&>(vs[0]));
        if (vs.size() > 1) {
            obj->id = std::move(std::any_cast<std::string&>(vs[1]));
        }
        return obj;
    };

    parser["Object"] = [](peg::SemanticValues& vs) -> ObjectPtr {
        // ObjectHead S? '{' S? ObjectItem? (S ObjectItem)* S? '}'
        auto& obj = std::any_cast<ObjectPtr&>(vs[0]);
        for (size_t i = 1; i < vs.size(); ++i) {
            auto& item = std::any_cast<Object::Item&>(vs[i]);
            if (std::holds_alternative<ObjectPtr>(item)) {
                obj->children.emplace_back(std::move(std::get<ObjectPtr>(item)));
            } else {
                obj->attributes.emplace_back(std::move(std::get<Attribute>(item)));
            }
        }
        return std::move(obj);
    };

    parser["Document"] = [](peg::SemanticValues& vs) -> ObjectPtr {
        return std::move(std::any_cast<ObjectPtr&>(vs[0]));
    };

    // parser.enable_packrat_parsing();  // Seems to be broken in JS, heap corruption?

    return parser;
}

const peg::parser& default_parser_instance()
{
    static const peg::parser parser = create_parser();
    return parser;
}

// Utilities for working with profile structures

struct SemanticError : public std::runtime_error
{
    using runtime_error::runtime_error;
};

template<typename VisitorImpl>
struct Visitor
{
    template<typename T>
    void operator()([[maybe_unused]] const T& v) { }

    template<typename T>
    void operator()(const std::shared_ptr<T>& v) { return self()(static_cast<const T&>(*v)); }

    template<typename... Ts>
    void operator()(const std::variant<Ts...>& v) {
        std::visit([&](const auto& v_) { return self()(v_); }, v);
    }

    // TODO: Visit Object, List and Dict by default?

private:
    inline VisitorImpl& self() { return static_cast<VisitorImpl&>(*this); }
};

// Visitor for collecting Objects and Values with an identifier

struct IdCollection : protected Visitor<IdCollection>
{
    ObjectMap object_ids;
    AttributeMap value_ids;

    IdCollection(const ObjectPtr& objp) { (*this)(objp); }

    void operator()(const ObjectPtr& objp)
    {
        Object& obj = *objp;
        // Object with id, check for duplicates
        if (!obj.id.empty()) {
            if (!object_ids.emplace(obj.id, &obj).second) {
                throw SemanticError("Object id '" + obj.id + "' was declared multiple times");
            }
        }
        // If object is 'Value', store reference to value attribute
        if (obj.classname == "Value") {
            if (obj.id.empty()) {
                throw SemanticError("Value object is missing id");
            }
            auto* value_attr = obj.find_attribute("value");
            if (value_attr == nullptr) {
                throw SemanticError("Value object '" + obj.id + "' is missing 'value' attribute");
            }
            value_ids.emplace(obj.id, value_attr);
        }
        // If object is 'Profile', require description and application attribute
        if (obj.classname == "Profile") {
            auto* descr = obj.find_attribute("description");
            if (descr == nullptr) {
                throw SemanticError("Profile object is missing description attribute");
            }
            // TODO: Create method for checking attribute data types
            if (!std::holds_alternative<String>(*descr->data)) {
                throw SemanticError("Profile description must be a string");
            }
            Attribute* application = obj.find_attribute("application");
            if (application == nullptr) {
                throw SemanticError("Profile object is missing application attribute");
            }
            if (!std::holds_alternative<ObjectPtr>(*(application->data))) {
                throw SemanticError("Application must be an object");
            }
        }
        // Continue search for objects in children and attribute list
        // TODO: Move this to Visitor?
        for (const ObjectPtr& child: obj.children) { (*this)(child); }
        for (const Attribute& attr: obj.attributes) { (*this)(attr.data); }

        // TODO: Look for objects in List or Dict?
    }

protected:
    friend struct Visitor<IdCollection>;
    using Visitor::operator();
};

// Semantic checking of profile structure

struct SemanticVerifier : protected Visitor<SemanticVerifier>
{
    SemanticVerifier(const IdCollection& id_collection)
        : ids(id_collection)
    { }

    void operator()(const ObjectPtr& obj)
    {
        // Require unique attribute identifiers within object
        {
            std::unordered_set<std::string> attribute_names;
            for (const auto& a: obj->attributes) {
                if (!attribute_names.emplace(a.name).second) {
                    throw SemanticError("Attribute '" + a.name + "' was declared multiple times");
                };
            }
        }
        // Verify children and attributes
        // TODO: Move this to Visitor?
        for (const ObjectPtr& child : obj->children) { (*this)(child); }
        for (const Attribute& attr : obj->attributes) { (*this)(attr.data); }
    }

protected:
    friend struct Visitor<SemanticVerifier>;
    using Visitor::operator();

    const IdCollection& ids;

    void operator()(const Reference& ref)
    {
        if (ids.object_ids.find(ref) == ids.object_ids.end()) {
            throw SemanticError("Unresolved reference: " + ref);
        }
    }

    void operator()(const RawExpression& expr)
    {
        for (const auto& ref: expr.references) {
            if (ids.object_ids.find(ref) == ids.object_ids.end()) {
                throw SemanticError("Unresolved reference: " + ref);
            }
            if (ids.value_ids.find(ref) == ids.value_ids.end()) {
                throw SemanticError("Reference to '" + ref + "' not allowed in expression");
            }
        }
    }

    void operator()(const List& list)
    {
        // TODO: Move this to Visitor?
        for (const Value& item : list) { (*this)(item); }
    }

    void operator()(const Dict& dict)
    {
        // TODO: Move this to Visitor?
        for (const KeyVal& item : dict) { (*this)(item.second); }
    }
};

/**
 * @brief Printer is a base class for writing parsed objects to a (text) stream.
 * The default method implementation prints objects to original syntax.
 */
template <typename PrinterImpl>
struct Printer
{
    Printer(std::ostream& o_) : o(o_) { }

    struct Collection {
        PrinterImpl& p;
        const std::string_view start;
        const std::string_view sep;
        const std::string_view end;
        bool with_newlines = true;

        Collection(PrinterImpl* p_,
                   std::string_view start_, std::string_view sep_, std::string_view end_)
            : p(*p_)
            , start(start_)
            , sep(sep_)
            , end(end_)
        {
            ++p.level;
            p << start;
        }

        template<typename T>
        inline PrinterImpl& operator<<(const T& v)
        {
            if (!first) {
                p << sep;
                if (!with_newlines) { p << ' '; }
            }
            if (with_newlines) { p << '\n' << p.indent; }
            first = false;
            return p << v;
        }

        ~Collection()
        {
            --p.level;
            if (with_newlines) { p << '\n' << p.indent; };
            p << end;
        }
    private:
        bool first = true;
    };

    struct Indent {};
    static constexpr Indent indent {};

    inline PrinterImpl& operator<<([[maybe_unused]] const Indent& indent) {
        for (size_t i=0; i < level; ++i) { o << "    "; }
        return self();
    }

    template<typename T>
    PrinterImpl& operator<<(const T& v) { o << v; return self(); }

    template<typename T>
    PrinterImpl& operator<<(const std::shared_ptr<T>& v) { return self() << *v; }

    template<typename... Ts>
    PrinterImpl& operator<<(const std::variant<Ts...>& v)
    {
        std::visit([&](const auto& v_) { self() << v_; }, v);
        return self();
    }

    PrinterImpl& operator<<(const String& v)
    {
        o << '"' << v << '"';
        return self();
    }

    PrinterImpl& operator<<(const Boolean& v)
    {
        o << (v ? "true" : "false");
        return self();
    }

    PrinterImpl& operator<<(const Float& v)
    {
        o << std::showpoint << v;
        return self();
    }

    PrinterImpl& operator<<(const List& v)
    {
        Collection list(&self(), "["sv, ","sv, "]"sv);
        list.with_newlines = false;
        for (const auto& item: v) { list << item; }
        return self();
    }

    PrinterImpl& operator<<(const Dict& v)
    {
        Collection dict(&self(),  "{"sv, ","sv, "}"sv);
        for (const auto& item: v) { dict << item.first << ':' << ' ' << item.second; }
        return self();
    }

    PrinterImpl& operator<<(const Reference& v)
    {
        o << v;
        return self();
    }

    PrinterImpl& operator<<(const RawExpression& v)
    {
        o << v.str;
        return self();
    }

    PrinterImpl& operator<<(const Object& v)
    {
        const std::string start = v.classname + ((v.id.empty()) ? "" : ' ' + v.id) + " {";
        Collection obj(&self(), start, ""sv, "}"sv);

        for (const auto& item: v.children) {
            obj << item;
        }
        for (const auto& a: v.attributes) {
            obj << a.name.c_str() << ": " << a.data;
        }
        return self();
    }

    std::ostream& o;
    size_t level = 0;

private:
    PrinterImpl& self() { return static_cast<PrinterImpl&>(*this); }
};

struct DefaultPrinter : public Printer<DefaultPrinter>
{
    using Printer::Printer;
};

/**
 * @brief The JsonAstPrinter outputs an abstract syntax tree in JSON format.
 */
struct JsonAstPrinter : public Printer<JsonAstPrinter>
{
    using Printer::Printer;

    bool eval_epressions = true;

    void print(const ObjectPtr& profile)
    {
        if (eval_epressions) {
            IdCollection id_collection(profile);
            for (auto& kv: id_collection.value_ids) {
                substitutions.emplace(kv.first, kv.second->data);
            }
        }
        *this << profile;
    }

protected:
    friend struct Printer<JsonAstPrinter>;

    ValueSubstitutions substitutions;

    using Printer::operator<<;

    JsonAstPrinter& operator<<(const Object& v)
    {
        Collection dict(this, "{"sv, ","sv, "}"sv);

        dict << std::string {"_type_"} << ": " << std::string{"object"};
        dict << std::string {"_class_"} << ": " << v.classname;
        if (!v.id.empty()) { dict << std::string {"_id_"} << ": " << v.id; }

        if (!v.children.empty()) {
            dict << std::string {"_children_"} << ": ";
            Collection clist(this, "["sv, ","sv, "]"sv);
            for (const auto& item: v.children) {
                clist << item;
            }
        }

        for (const Profile::Attribute& a: v.attributes) {
            dict << a.name << ": " << a.data;
        }

        return *this;
    }

    JsonAstPrinter& operator<<(const Float& v)
    {
        // JSON doesn't allow trailing float dot.
        // Format to string and append 0 if this case is found.
        std::stringstream ss;
        ss << std::showpoint << v;
        std::string float_str = ss.str();
        if (float_str.back() == '.') { float_str.push_back('0'); }
        (*this) << float_str;
        return *this;
    }

    JsonAstPrinter& operator<<(const RawExpression& expr)
    {
        if (eval_epressions) {
            // Re-parse expression with variable substitution and emit constant
            ValuePtr value;
            const auto& parser = default_parser_instance();
            std::any ctx { &substitutions };
            const auto p = parser["Expr"].parse_and_get_value(expr.str.data(), expr.str.length(), ctx, value);
            (*this) << value;
        } else {
            // Emit raw expression
            Collection dict(this, "{"sv, ","sv, "}"sv);
            dict << std::string {"_type_"} << ": " << std::string{"expression"};
            dict << std::string {"value"} << ": " << expr.str;
        }
        return *this;
    }

    JsonAstPrinter& operator<<(const Reference& ref)
    {
        if (eval_epressions) {
            if (auto it = substitutions.find(ref); it != substitutions.end()) {
                return (*this) << it->second;
            }
        }
        Collection dict(this, "{"sv, ","sv, "}"sv);
        dict << std::string {"_type_"} << ": " << std::string{"reference"};
        dict << std::string {"reference"} << ": " << static_cast<const std::string&>(ref);
        return *this;
    }
};


// Utility functions

/**
 * @brief visit_values Visit profile value objects.
 * @param profile Profile object.
 * @param visitor Value object visitor.
 */
template <typename Visitor>
inline void visit_values(ObjectPtr& profile, Visitor&& visitor)
{
    const auto isValue = [](const ObjectPtr& obj) -> bool { return obj->classname == "Value"; };
    for (auto& obj : profile->children) {
        if (!isValue(obj)) { continue; }
        visitor(obj);
    }
}

/**
 * @brief visit_values Visit profile value objects.
 * @param profile Profile object.
 * @param visitor Value object visitor.
 */
template <typename Visitor>
inline void visit_values(const ObjectPtr& profile, Visitor&& visitor)
{
    const auto isValue = [](const ObjectPtr& obj) -> bool { return obj->classname == "Value"; };
    for (const auto& obj : profile->children) {
        if (!isValue(obj)) { continue; }
        visitor(obj);
    }
}


// External interface

/**
 * @brief parse Parse profile from source string.
 * @param src Profile source.
 * @return Profile root object and object ID map.
 */
std::tuple<ObjectPtr, IdCollection> parse(const std::string_view& src)
{
    // Parse source
    ObjectPtr profile;
    const auto& parser = default_parser_instance();
    std::stringstream errorstr;
    const auto logger = [&](size_t line, size_t pos, const std::string& msg, const std::string& rule) {
        errorstr << line << ':' << pos << ' ' << msg << '(' << rule << ')' << '\n';
    };
    auto result = parser["Document"].parse_and_get_value(src.data(), src.size(), profile, nullptr, logger);
    if (!result.ret) {
        result.error_info.output_log(logger, src.data(), src.size());
        throw std::runtime_error(errorstr.str());
    }
    // Pass 1: Collecting object ids
    IdCollection ids(profile);
    // Pass 2: Verifying profile structure
    SemanticVerifier verify(ids);
    verify(profile);

    return { profile, std::move(ids) };
}

/**
 * @brief to_profile_source Serialize profile object to profile source.
 * @param obj Profile object to serialize.
 * @return Profile source serialization.
 */
std::string to_profile_source(const ObjectPtr& obj)
{
    std::stringstream out;
    DefaultPrinter printer { out };
    printer << obj;
    return out.str();
}

/**
 * @brief to_json_ast Serialize profile object to JSON representation.
 * @param obj Profile object to serialize.
 * @param eval_epressions If true, serialize with expressions evaluated to constants.
 * @return JSON serialization.
 */
std::string to_json_ast(const ObjectPtr& obj, const bool eval_epressions)
{
    std::stringstream out;
    JsonAstPrinter printer { out };
    printer.eval_epressions = eval_epressions;
    printer.print(obj);
    return out.str();
}

/**
 * @brief to_json_ast Serialize profile source to JSON representation.
 * @param src Profile source string.
 * @param eval_epressions If true, serialize with expressions evaluated to constants.
 * @return JSON serialization.
 */
std::string to_json_ast(const std::string_view& src, const bool eval_epressions)
{
    const auto [profile, ids] = Profile::parse(src);
    return to_json_ast(profile, eval_epressions);
}

}  // namespace Profile
