#pragma once
#include <emscripten/val.h>
#include <emscripten/bind.h>

#include <profileparser/utils.hpp>
#include <profileparser.hpp>
#include "binding_ast.hpp"


struct ValueField
{
    std::string id;
    ValueType valueType;
    Profile::Value value;
    std::vector<Profile::KeyVal> attrs;
};

inline ValueField toValueField(const Profile::ObjectNodePtr& valueObj)
{
    ValueField field {
        .id=valueObj->id,
        .valueType={},
        .value={},
        .attrs={},
    };
    for (const auto& attribute: valueObj->attributes) {
        if (attribute->name == "value") {
            field.value = attribute->value;
        } else {
            field.attrs.push_back(Profile::KeyVal{.key=attribute->name, .value=attribute->value});
        }
    }
    field.valueType = valueTypeOf(field.value);
    return field;
}

inline std::vector<ValueField> getValueFields(const Profile::ObjectNodePtr& profileObj)
{
    std::vector<ValueField> fields;
    fields.reserve(profileObj->children.size());
    Profile::visit_values(profileObj, [&](const Profile::ObjectNodePtr& valueObj) {
        fields.emplace_back(toValueField(valueObj));
    });
    return fields;
}

EMSCRIPTEN_DECLARE_VAL_TYPE(EmValueFields);
EMSCRIPTEN_DECLARE_VAL_TYPE(EmValueSubstitutions);

namespace detail {
    using namespace emscripten;

    // Write simple `emscripten::val`s to `Profile::Value`

    template<typename T> inline void set_value(T& v, const val& value);

    template<typename ... Ts> inline void set_value(std::variant<Ts...>& v, const val& value)
    {
        std::visit([&](auto& v_) { set_value(v_, value); }, v);
    }

    template<typename T> inline void set_value([[maybe_unused]] T& v, [[maybe_unused]] const val& value)
    {
        EM_ASM({console.log("Profile value type does not support writing from JS");}, );
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
        v = value.as<Profile::String>();
    }

    template<> inline void set_value(Profile::Boolean& v, const val& value)
    {
        v = value.as<Profile::Boolean>();
    }

    // Exception to Error conversion

    inline val as_error_val(auto&& msg)
    {
        const auto Error = val::global("Error");
        return Error.new_(std::forward<decltype(msg)>(msg));
    }

    inline val as_error_val(const std::exception& e)
    {
        return as_error_val(std::string(e.what()));
    }
};

struct JsProfile
{
    JsProfile(std::string source)
        : profile(Profile::parse(std::move(source)))
        , valueFields(getValueFields(profile.root))
    { }

    Profile::ParsedProfile profile;
    std::vector<ValueField> valueFields;

    const Profile::ObjectNode& root() { return *profile.root; }

    std::string source() const { return profile.source; }

    Profile::LineRange sourceRange(const Profile::SourceInfo& source_info) const
    {
        return Profile::get_line_range(profile.source, source_info);
    }

    std::string updatedSource(const EmValueSubstitutions& newValues)
    {
        std::unordered_map<std::string, Profile::Value> substitutions;
        for (const auto& field : valueFields) {
            const auto& id = field.id;
            if (!newValues.hasOwnProperty(id.c_str())) { continue; }
            Profile::Value value = field.value;
            detail::set_value(value, newValues[id]);
            substitutions.emplace(id, value);
        }
        return Profile::to_updated_source(profile, substitutions);
    }
};

EMSCRIPTEN_DECLARE_VAL_TYPE(EmProfileOrError);

EmProfileOrError parseProfile(std::string source)
try {
    const auto profile = std::make_shared<JsProfile>(std::move(source));
    return EmProfileOrError(emscripten::val(profile));
} catch (const std::exception& e) {
    return EmProfileOrError(detail::as_error_val(e));
}

EMSCRIPTEN_DECLARE_VAL_TYPE(EmCheckRefCallback);
EMSCRIPTEN_DECLARE_VAL_TYPE(EmErrorOrUndefined);

EmErrorOrUndefined validateProfile(const JsProfile& profile, EmCheckRefCallback refCheckCallback)
try {
    if (refCheckCallback.as<bool>()) {
        Profile::validate_profile(profile.profile, [&](const std::string& ref) -> bool {
            return refCheckCallback(emscripten::val{ref}).as<bool>();
        });
    } else {
        Profile::validate_profile(profile.profile);
    }
    return EmErrorOrUndefined(emscripten::val());
} catch (const std::exception& e) {
    return EmErrorOrUndefined(detail::as_error_val(e));
}

// ----------------------------------------------------------------------------

inline void register_api_bindings()
{
    using namespace emscripten;

    register_type<EmValueFields>("ValueField[]");
    register_type<EmValueSubstitutions>("{[id: string]: (number | boolean | string)}");
    register_type<EmProfileOrError>("Profile | Error");
    register_type<EmCheckRefCallback>("((string) => boolean) | (null | undefined)");
    register_type<EmErrorOrUndefined>("Error | undefined");

    value_object<Profile::LineInfo>("LineInfo")
        .field("line", &Profile::LineInfo::line)
        .field("column", &Profile::LineInfo::column)
        ;

    value_object<Profile::LineRange>("LineRange")
        .field("begin", &Profile::LineRange::begin)
        .field("end", &Profile::LineRange::end)
        ;

    value_object<ValueField>("ValueField")
        .field("id", &ValueField::id)
        .field("valueType", &ValueField::valueType)
        .EMVAL_FIELD("value", ValueMember<&ValueField::value>)
        .EMVAL_FIELD("attrs", KeyValsMember<&ValueField::attrs>)
        ;

    class_<JsProfile>("Profile")
        .smart_ptr<std::shared_ptr<JsProfile>>("Profile")
        .function("root", &JsProfile::root)
        .function("source", &JsProfile::source)
        .function("sourceRange", &JsProfile::sourceRange)
        .function("updatedSource", &JsProfile::updatedSource)
        .property("valueFields", &EmValMember<EmValueFields, &JsProfile::valueFields>::get)
        ;

    emscripten::function("parseProfile", &parseProfile);
    emscripten::function("validateProfile", &validateProfile);
}
