#include "../cpp/processprofile.h"

#include <string>
#include <string_view>
#include <type_traits>
#include <emscripten.h>
#include <emscripten/bind.h>

using namespace emscripten;

namespace detail {

    // Profile value type names
    // ------------------------------------------------------------------------

    template<typename T> constexpr inline std::string_view type_name() { return "unknown"; }
    template<> constexpr inline std::string_view type_name<Profile::Integer>() { return "int"; }
    template<> constexpr inline std::string_view type_name<Profile::Float>() { return "float"; }
    template<> constexpr inline std::string_view type_name<Profile::String>() { return "string"; }
    template<> constexpr inline std::string_view type_name<Profile::Boolean>() { return "bool"; }
    template<> constexpr inline std::string_view type_name<Profile::List>() { return "list"; }
    template<> constexpr inline std::string_view type_name<Profile::Dict>() { return "dict"; }

    // Typename from profile value
    // ------------------------------------------------------------------------

    template<typename T> inline std::string_view type_of(const T& v);
    template<typename T> inline std::string_view type_of(const std::shared_ptr<T>& v);
    template<typename ... Ts> inline std::string_view type_of(const std::variant<Ts...>& v);

    template<typename T> inline std::string_view type_of([[maybe_unused]] const T& v)
    {
        return type_name<std::decay_t<T>>();
    }

    template<typename T> inline std::string_view type_of(const std::shared_ptr<T>& v)
    {
        return type_of(*v);
    }

    template<typename ... Ts> inline std::string_view type_of(const std::variant<Ts...>& v)
    {
        return std::visit([&](auto& v_) -> std::string_view { return type_of(v_); }, v);
    }

    // Profile values to emscripten vals
    // ------------------------------------------------------------------------

    template<typename T> inline val to_val(const T& v);
    template<typename T> inline val to_val(const std::shared_ptr<T>& v);
    template<typename ... Ts> inline val to_val(const std::variant<Ts...>& v);

    template<typename T> inline val to_val(const std::shared_ptr<T>& v)
    {
        return to_val(*v);
    }

    template<typename... Ts> inline val to_val(const std::variant<Ts...>& v)
    {
        return std::visit([&](const auto& v_) -> val { return to_val(v_); }, v);
    }

    template<typename T> inline val to_val([[maybe_unused]] const T& v) { return val::undefined(); }

    template<> inline val to_val(const Profile::Integer& v) { return val(static_cast<long>(v)); }

    template<> inline val to_val(const Profile::Float& v) { return val(static_cast<double>(v)); }

    template<> inline val to_val(const Profile::Boolean& v) { return val(static_cast<bool>(v)); }

    template<> inline val to_val(const Profile::String& v) { return val(static_cast<std::string>(v)); }

    template<> inline val to_val(const Profile::Dict& v)
    {
        val map = val::object();
        for (const auto& item: v) {
            map.set(item.first, to_val(item.second));
        }
        return map;
    }

    template<> inline val to_val(const Profile::List& v)
    {
        std::vector<val> list;
        list.reserve(v.size());
        for (const auto& item: v) { list.emplace_back(to_val(item)); }
        return val::array(list);
    }

    template<> inline val to_val(const Profile::Object& v)
    {
        val obj = val::object();
        obj.set("_type_", val("object"));
        obj.set("_class_", val(v.classname));
        if (!v.id.empty()) { obj.set("_id_", val(v.id)); }
        if (!v.children.empty()) {
            std::vector<val> children;
            children.reserve(v.children.size());
            for (const auto& item: v.children) {
                children.emplace_back(to_val(item));
            }
            obj.set("_children_", val::array(children));
        }
        for (const Profile::Attribute& a: v.attributes) {
            obj.set(a.name, to_val(a.data));
        }
        return obj;
    }

    template<> inline val to_val(const Profile::RawExpression& expr)
    {
        val obj = val::object();
        obj.set("_type_", val("expression"));
        obj.set("value", val(expr.str));
        return obj;
    }

    template<> inline val to_val(const Profile::Reference& ref)
    {
        val obj = val::object();
        obj.set("_type_", val("reference"));
        obj.set("reference", val(static_cast<const std::string&>(ref)));
        return obj;
    }


    // Set profile values from emscripten vals
    // ------------------------------------------------------------------------

    template<typename T> inline void set_value(T& v, const val& value);
    template<typename T> inline void set_value(std::shared_ptr<T>& v, const val& value);
    template<typename ... Ts> inline void set_value(std::variant<Ts...>& v, const val& value);

    template<typename T> inline void set_value(std::shared_ptr<T>& v, const val& value)
    {
        set_value(*v, value);
    }

    template<typename ... Ts> inline void set_value(std::variant<Ts...>& v, const val& value)
    {
        std::visit([&](auto& v_) { set_value(v_, value); }, v);
    }

    template<typename T> inline void set_value([[maybe_unused]] T& v, [[maybe_unused]] const val& value)
    {
        EM_ASM({console.log('Profile value type does not support `update_value()`');}, );
    }

    template<> inline void set_value(Profile::Integer& v, const val& value)
    {
        v = value.as<Profile::Integer>();
    }

    template<> inline void set_value(Profile::Float& v, const val& value)
    {
        v = value.as<Profile::Float>();
    }

    template<> inline void set_value(Profile::String& v, const val& value)
    {
        v = value.as<std::string>();
    }

    template<> inline void set_value(Profile::Boolean& v, const val& value)
    {
        v = value.as<bool>();
    }

}  // namespace Convert


struct ValueField {
    std::string id;
    std::string type;
    val value = val::undefined();
    val attrs = val::undefined();

    static ValueField from_value_object(const Profile::ObjectPtr& obj)
    {
        ValueField field {
            .id = obj->id,
            .type = {},
            .value = val::undefined(),
            .attrs = val::object(),
        };
        for (const Profile::Attribute& attribute: obj->attributes) {
            if (attribute.name == "value") {
                field.type = std::string { detail::type_of(attribute.data) };
                field.value = detail::to_val(attribute.data);
            } else {
                field.attrs.set(attribute.name, detail::to_val(attribute.data));
            }
        }
        return field;
    }
};

std::vector<ValueField> fields_from_profile(const Profile::ObjectPtr& profile)
{
    std::vector<ValueField> fields;
    fields.reserve(profile->children.size());
    Profile::visit_values(profile, [&](const Profile::ObjectPtr& valueObj) {
        fields.emplace_back(ValueField::from_value_object(valueObj));
    });
    return fields;
}

val source_to_fields(const std::string& source)
try {
    const auto [profile, ids] = Profile::parse(source.data());
    return val::array(fields_from_profile(profile));
} catch(...) {
    return val::global("Error").new_(val("Failed parsing profile"));
}

void update_profile_values(Profile::ObjectPtr& profile, const val& updateDict)
{
    Profile::visit_values(profile, [&](Profile::ObjectPtr& valueObj) {
        if (!updateDict.hasOwnProperty(valueObj->id.c_str())) { return; }
        auto& valueData = valueObj->find_attribute("value")->data;
        detail::set_value(valueData, updateDict[valueObj->id]);
    });
}

val source_to_json_ast(const std::string& source, const bool eval_expressions)
try {
    const auto json = Profile::to_json_ast(source.data(), eval_expressions);
    return val(json);
} catch (const std::exception& e) {
    return val::global("Error").new_(val(std::string(e.what())));
}

val source_with_updates(const std::string& source, const val& updateDict)
try {
    auto [profile, ids] = Profile::parse(source.data());
    update_profile_values(profile, updateDict);
    return val(Profile::to_profile_source(profile));
} catch (const std::exception& e) {
    return val::global("Error").new_(val(std::string(e.what())));
}


EMSCRIPTEN_BINDINGS(profile_parser_module)
{
    // Type definitions
    value_object<ValueField>("ValueField")
            .field("id", &ValueField::id)
            .field("type", &ValueField::type)
            .field("value", &ValueField::value)
            .field("attrs", &ValueField::attrs);
    // Methods
    emscripten::function("source_to_json_ast", &source_to_json_ast);
    emscripten::function("source_to_fields", &source_to_fields);
    emscripten::function("source_with_updates", &source_with_updates);
}
