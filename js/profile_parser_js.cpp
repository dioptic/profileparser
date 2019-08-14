#include "../cpp/processprofile.h"

#include <string>
#include <emscripten/bind.h>

using namespace emscripten;

struct Convert {
    // Profile data types to emscripten val conversion

    template<typename T>
    static inline val to_val([[maybe_unused]] const T& v)
    {
        return val::undefined();
    }

    template<typename T>
    static inline val to_val(const std::shared_ptr<T>& v)
    {
        return to_val(*v);
    }

    template<typename... Ts>
    static inline val to_val(const std::variant<Ts...>& v)
    {
        return std::visit([&](const auto& v_) -> val { return to_val(v_); }, v);
    }

    static inline val to_val(const Profile::Integer& v)
    {
        return val(static_cast<long>(v));
    }

    static inline val to_val(const Profile::Float& v)
    {
        return val(static_cast<double>(v));
    }

    static inline val to_val(const Profile::Boolean& v)
    {
        return val(static_cast<bool>(v));
    }

    static inline val to_val(const Profile::String& v)
    {
        return val(static_cast<std::string>(v));
    }

    static inline val to_val(const Profile::Dict& v)
    {
        val map = val::object();
        for (const auto& item: v) {
            map.set(item.first, to_val(item.second));
        }
        return map;
    }

    static inline val to_val(const Profile::List& v)
    {
        std::vector<val> list;
        list.reserve(v.size());
        for (const auto& item: v) { list.emplace_back(to_val(item)); }
        return val::array(list);
    }

    static inline val to_val(const Profile::Object& v)
    {
        val obj = val::object();
        obj.set("_type_", val("object"));
        obj.set("_class_", val(v.classname));
        if (!v.id.empty()) { obj.set("_id_", val(v.id)); }
        if (!v.children.empty()) {
            std::vector<val> children;
            children.reserve(v.children.size());
            for (const auto& item: v.children) {
                children.emplace_back(Convert::to_val(item));
            }
            obj.set("_children_", val::array(children));
        }
        for (const Profile::Attribute& a: v.attributes) {
            obj.set(a.name, Convert::to_val(a.data));
        }
        return obj;
    }

    static inline val to_val(const Profile::RawExpression& expr)
    {
        val obj = val::object();
        obj.set("_type_", val("expression"));
        obj.set("value", val(expr.str));
        return obj;
    }

    static inline val to_val(const Profile::Reference& ref)
    {
        val obj = val::object();
        obj.set("_type_", val("reference"));
        obj.set("reference", val(static_cast<const std::string&>(ref)));
        return obj;
    }
};

struct ParseResult {
    val ast = val::undefined();
    std::string error;

    static ParseResult ast_result(val ast)
    {
        return { std::move(ast), {} };
    }

    static ParseResult error_result(std::string e)
    {
        return { val::undefined(), std::move(e) };
    }
};

ParseResult source_to_ast(const std::string& source, [[maybe_unused]] const bool eval_expressions)
try {
    const auto [profile, ids] = Profile::parse(source.data());
    return ParseResult::ast_result(Convert::to_val(profile));
} catch (const std::exception& e) {
    return ParseResult::error_result(std::string("Parse error: ") + e.what());
}

ParseResult source_to_json_ast(const std::string& source, const bool eval_expressions)
try {
    const auto json = Profile::to_json_ast(source.data(), eval_expressions);
    return ParseResult::ast_result(val(json));
} catch (const std::exception& e) {
    return ParseResult::error_result(std::string("Parse error: ") + e.what());
}

EMSCRIPTEN_BINDINGS(profile_parser_module)
{
    // Type definitions
    value_object<ParseResult>("ParseResult")
            .field("ast", &ParseResult::ast)
            .field("error", &ParseResult::error);
    // Methods
    emscripten::function("source_to_ast", &source_to_ast);
    emscripten::function("source_to_json_ast", &source_to_json_ast);
}

