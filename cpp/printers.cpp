#include <sstream>
#include <profileparser/utils.hpp>
#include <profileparser.hpp>
#include "parser.hpp"

namespace Profile {

using namespace std::string_view_literals;

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

    PrinterImpl& operator<<(const DocString& v)
    {
        for (auto& line : v.lines) {
            o << "# " << line;
            self() << '\n' << indent;
        }
        return self();
    }

    PrinterImpl& operator<<(const StringExpressionNode& v)
    {
        o << '"';
        for(auto& f : v.fragments) { 
            if(const ReferenceNodePtr* ref = std::get_if<ReferenceNodePtr>(&f)) {
                o << "${" << (*ref)->reference << "}";
            } else
                o << std::get<String>(f); 
        }
        o << '"';
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

    PrinterImpl& operator<<(const ListNode& v)
    {
        // TODO use id ?
        Collection list(&self(), "["sv, ","sv, "]"sv);
        list.with_newlines = false;
        for (const auto& item: v.items) { list << item; }
        return self();
    }

    PrinterImpl& operator<<(const DictNode& v)
    {
        // TODO use id ?
        Collection dict(&self(),  "{"sv, ","sv, "}"sv);
        for (const auto& [key, value]: v.items) { dict << key << ':' << ' ' << value; }
        return self();
    }

    PrinterImpl& operator<<(const ReferenceNode& v)
    {
        o << v.reference;
        return self();
    }

    PrinterImpl& operator<<(const NumericExpressionNode& v)
    {
        o << v.value;
        return self();
    }

    PrinterImpl& operator<<(const ObjectNode& v)
    {
        const std::string start = v.classname + ((v.id.empty()) ? "" : ' ' + v.id) + " {";
        Collection obj(&self(), start, ""sv, "}"sv);
        
        if(v.docstring.lines.size())
            obj << v.docstring;

        for (const auto& item: v.children) {
            obj << item;
        }
        for (const auto& a: v.attributes) {
            obj << a->name.c_str() << ": " << a->value;
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
    JsonAstPrinter(const ParsedProfile& profile, std::ostream& o, bool eval_expressions = true)
        : Printer<JsonAstPrinter>{o}
        , profile(profile)
        , eval_expressions(eval_expressions)
    {}

    const ParsedProfile& profile;
    bool eval_expressions = true;

    void print(const ObjectNodePtr& object)
    {
        if (eval_expressions) {
            for (const auto& [id, attribute]: profile.ids.value_ids) {
                substitutions.emplace(id, attribute->value);
            }
        }
        *this << object;
    }

protected:
    friend struct Printer<JsonAstPrinter>;
    using Printer::operator<<;

    ValueSubstitutions substitutions;

    JsonAstPrinter& operator<<(const ObjectNode& v)
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

        for (const Profile::AttributeNodePtr& a: v.attributes) {
            dict << a->name << ": " << a->value;
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

    JsonAstPrinter& operator<<(const NumericExpressionNode& expr)
    {
        // Try simplifying the expression using variable substitutions
        if (eval_expressions) {
            auto reduced_expression = parseExpression(expr.value, substitutions);
            if (!std::holds_alternative<NumericExpressionNodePtr>(reduced_expression)) {
                (*this) << reduced_expression;
                return *this;
            }
        }
        // Else emit raw expression as is
        Collection dict(this, "{"sv, ","sv, "}"sv);
        dict << std::string {"_type_"} << ": " << std::string{"expression"};
        dict << std::string {"value"} << ": " << expr.value;
        return *this;
    }

    JsonAstPrinter& operator<<(const ReferenceNode& ref)
    {
        if (eval_expressions) {
            if (auto it = substitutions.find(ref.reference); it != substitutions.end()) {
                return (*this) << it->second;
            }
        }
        Collection dict(this, "{"sv, ","sv, "}"sv);
        dict << std::string {"_type_"} << ": " << std::string{"reference"};
        dict << std::string {"reference"} << ": " << static_cast<const std::string&>(ref.reference);
        return *this;
    }
};

std::string to_profile_source(const ParsedProfile& profile)
{
    std::stringstream out;
    DefaultPrinter printer { out };
    printer << profile.root;
    return out.str();
}


std::string to_updated_source(const ParsedProfile& profile, const std::unordered_map<std::string, Value>& values)
{
    const auto& idToAttrib = profile.ids.value_ids;
    std::unordered_map<const AttributeNode*, Value> attribToValue;
    attribToValue.reserve(values.size());
    for (const auto& [id, value] : values) {
        if (!idToAttrib.contains(id)) {
            throw std::runtime_error("Invalid value-ID given for updating source");
        }
        const AttributeNode* attrib = idToAttrib.at(id);
        attribToValue.emplace(attrib, value);
    }
    return to_updated_source(profile, attribToValue);
}

std::string to_updated_source(const ParsedProfile& profile, const std::unordered_map<const AttributeNode*, Value>& values)
{
    // Sort attributes by source location
    std::vector<const AttributeNode*> attributes;
    attributes.reserve(values.size());
    for (const auto& kv : values) { attributes.push_back(kv.first); }
    std::sort(attributes.begin(), attributes.end(), [](const AttributeNode* a1, const AttributeNode* a2) {
        return a1->value_source_info.offset < a2->value_source_info.offset;
    });
    // Write segments of original source and new values
    std::stringstream out;
    Profile::DefaultPrinter printer { out };
    const char* const src = profile.source.data();
    size_t src_pos = 0;
    for (const AttributeNode* a: attributes) {
        // Copy from original source up to next attribute value
        const auto nUntilNextValue = a->value_source_info.offset - src_pos;
        out.write(src + src_pos, nUntilNextValue);
        src_pos += nUntilNextValue;
        // Print new value to output and skip over old value in source stream
        printer << values.at(a);
        src_pos += a->value_source_info.length;
    }
    // Copy remainder of source stream
    out.write(src + src_pos, profile.source.size() - src_pos);
    return out.str();
}

std::string to_json_ast(const ParsedProfile& profile, const bool eval_expressions)
{
    std::stringstream out;
    JsonAstPrinter printer { profile, out, eval_expressions };
    printer.print(profile.root);
    return out.str();
}

} // namespace Profile
