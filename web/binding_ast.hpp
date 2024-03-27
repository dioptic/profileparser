#pragma once
#include <emscripten/val.h>
#include <emscripten/bind.h>

#include <profileparser.hpp>
#include "binding_helpers.hpp"

/**
 * @brief Enum for distinguishing `Profile::Value` variants in JS.
 */
enum class ValueType {
    ObjectNode,
    ReferenceNode,
    NumericExpressionNode,
    StringExpressionNode,
    ListNode,
    DictNode,
    Boolean,
    Integer,
    Float,
    String,
    Unknown,
};

/**
 * @brief `ValueType` enum value from `Profile::Value` types (auto unwrap `shared_ptr<T>`).
 */
template <typename T> constexpr ValueType valueType_v = ValueType::Unknown;
template<> constexpr ValueType valueType_v<Profile::ObjectNode> = ValueType::ObjectNode;
template<> constexpr ValueType valueType_v<Profile::ReferenceNode> = ValueType::ReferenceNode;
template<> constexpr ValueType valueType_v<Profile::NumericExpressionNode> = ValueType::NumericExpressionNode;
template<> constexpr ValueType valueType_v<Profile::StringExpressionNode> = ValueType::StringExpressionNode;
template<> constexpr ValueType valueType_v<Profile::ListNode> = ValueType::ListNode;
template<> constexpr ValueType valueType_v<Profile::DictNode> = ValueType::DictNode;
template<> constexpr ValueType valueType_v<Profile::Boolean> = ValueType::Boolean;
template<> constexpr ValueType valueType_v<Profile::Integer> = ValueType::Integer;
template<> constexpr ValueType valueType_v<Profile::Float> = ValueType::Float;
template<> constexpr ValueType valueType_v<Profile::String> = ValueType::String;
template<typename V> constexpr ValueType valueType_v<std::shared_ptr<V>> = valueType_v<V>;

/**
 * @brief Get type of `Profile::Value` value.
 */
constexpr ValueType valueTypeOf(const Profile::Value& value) {
    return std::visit([](const auto& v) { return valueType_v<std::remove_cvref_t<decltype(v)>>; }, value);
}

/**
 * @brief Getter for AST-node-type specific `ValueType`.
 */
template <typename NodeT>
struct ValueTypeMember
{
    static constexpr ValueType valueType = valueType_v<NodeT>;
    static constexpr ValueType get(const NodeT&) { return valueType; }
    static constexpr void set(NodeT&, const ValueType&) {}
};

// To-JS-Value conversion
// ----------------------------------------------------------------------------

template <typename T>
inline emscripten::val asJsValue(const T& value) { return emscripten::val(value); }

template <typename T>
inline emscripten::val asJsValue(const std::shared_ptr<T>& ptr) { return emscripten::val(*ptr); }

template <typename ...Ts>
inline emscripten::val asJsValue(const std::variant<Ts...>& variant)
{
    return std::visit([](const auto& v) -> emscripten::val { return asJsValue(v); }, variant);
}

template <typename T>
inline emscripten::val asJsValue(const std::vector<T>& vector)
{
    auto out = emscripten::val::array();
    for (const auto& v : vector) {
        out.call<void>("push", asJsValue(v));
    }
    return out;
}

// Generic member getters
// ----------------------------------------------------------------------------

template <typename T>
concept IsEmVal = std::is_base_of_v<emscripten::val, T>;

// Getter for members requiring `asJsValue` conversion.
template <IsEmVal EmT, auto P, typename C = member_ptr_class_t<P>, typename V = member_ptr_value_t<P>>
requires IsMemberObjectPointer<P>
struct EmValMember
{
    static EmT get(const C& obj) { return EmT { asJsValue(obj.*P) }; }
    static void set(C&, const EmT&) {}
};

// Getter for unwrapping `shared_ptr<T>` members, where type `T` is known to embind.
template <auto P, typename C = member_ptr_class_t<P>, IsSharedPtr V = member_ptr_value_t<P>>
requires IsMemberObjectPointer<P>
struct SharedPtrMember
{
    using Value = V::element_type;
    static const Value& get(const C& obj) { return *(obj.*P); }
    static void set(C&, const Value&) {}
};

// ----------------------------------------------------------------------------

// Binding for members embind can't handle (vectors, variants)
// ----------------------------------------------------------------------------

EMSCRIPTEN_DECLARE_VAL_TYPE(EmStrings);
template<auto P>
using StringsMember = EmValMember<EmStrings, P>;

EMSCRIPTEN_DECLARE_VAL_TYPE(EmValue);
template<auto P>
using ValueMember = EmValMember<EmValue, P>;

EMSCRIPTEN_DECLARE_VAL_TYPE(EmValues);
template<auto P>
using ValuesMember = EmValMember<EmValues, P>;

EMSCRIPTEN_DECLARE_VAL_TYPE(EmStringFragments);
template<auto P>
using StringFragmentsMember = EmValMember<EmStringFragments, P>;

EMSCRIPTEN_DECLARE_VAL_TYPE(EmKeyVals);
template<auto P>
using KeyValsMember = EmValMember<EmKeyVals, P>;

EMSCRIPTEN_DECLARE_VAL_TYPE(EmObjectNodes);
template<auto P>
using ObjectNodesMember = EmValMember<EmObjectNodes, P>;

EMSCRIPTEN_DECLARE_VAL_TYPE(EmAttributeNodes);
template<auto P>
using AttributeNodesMember = EmValMember<EmAttributeNodes, P>;

EMSCRIPTEN_DECLARE_VAL_TYPE(EmReferenceNodes);
template<auto P>
using ReferenceNodesMember = EmValMember<EmReferenceNodes, P>;

#define EMVAL_FIELD(name, accessor) field(name, &accessor::get, &accessor::set)
#define TYPE_FIELD(name, T) field(name, &ValueTypeMember<T>::get, &ValueTypeMember<T>::set)

// ----------------------------------------------------------------------------

inline void register_ast_bindings()
{
    using namespace emscripten;

    register_type<EmStrings>("string[]");
    register_type<EmValue>("ListNode | DictNode | NumericExpressionNode | StringExpressionNode | ObjectNode | ReferenceNode | number | string | boolean");
    register_type<EmValues>("(ListNode | DictNode | NumericExpressionNode | StringExpressionNode | ObjectNode | ReferenceNode | number | string | boolean)[]");
    register_type<EmStringFragments>("(string | ReferenceNode)[]");
    register_type<EmKeyVals>("KeyVal[]");
    register_type<EmObjectNodes>("ObjectNode[]");
    register_type<EmAttributeNodes>("AttributeNode[]");
    register_type<EmReferenceNodes>("ReferenceNode[]");

    enum_<ValueType>("ValueType")
        .value("ObjectNode", ValueType::ObjectNode)
        .value("ReferenceNode", ValueType::ReferenceNode)
        .value("NumericExpressionNode", ValueType::NumericExpressionNode)
        .value("StringExpressionNode", ValueType::StringExpressionNode)
        .value("ListNode", ValueType::ListNode)
        .value("DictNode", ValueType::DictNode)
        .value("Boolean", ValueType::Boolean)
        .value("Integer", ValueType::Integer)
        .value("Float", ValueType::Float)
        .value("String", ValueType::String)
        .value("Unknown", ValueType::Unknown)
        ;

    value_object<Profile::SourceInfo>("SourceInfo")
        .field("offset", &Profile::SourceInfo::offset)
        .field("length", &Profile::SourceInfo::length)
        ;

    value_object<Profile::DocString>("DocString")
        .field("source_info", &Profile::DocString::source_info)
        .EMVAL_FIELD("lines", StringsMember<&Profile::DocString::lines>)
        ;

    value_object<Profile::ListNode>("ListNode")
        .TYPE_FIELD("type", Profile::ListNode)
        .field("source_info", &Profile::ListNode::source_info)
        .field("id", &Profile::ListNode::id)
        .EMVAL_FIELD("items", ValuesMember<&Profile::ListNode::items>)
        ;

    value_object<Profile::KeyVal>("KeyVal")
        .field("key", &Profile::KeyVal::key)
        .EMVAL_FIELD("value", ValueMember<&Profile::KeyVal::value>)
        ;

    value_object<Profile::DictNode>("DictNode")
        .TYPE_FIELD("type", Profile::DictNode)
        .field("source_info", &Profile::DictNode::source_info)
        .field("id", &Profile::DictNode::id)
        .EMVAL_FIELD("items", KeyValsMember<&Profile::DictNode::items>)
        ;

    value_object<Profile::NumericExpressionNode>("NumericExpressionNode")
        .TYPE_FIELD("type", Profile::NumericExpressionNode)
        .field("source_info", &Profile::NumericExpressionNode::source_info)
        .field("value", &Profile::NumericExpressionNode::value)
        .EMVAL_FIELD("references", ReferenceNodesMember<&Profile::NumericExpressionNode::references>)
        ;

    value_object<Profile::StringExpressionNode>("StringExpressionNode")
        .TYPE_FIELD("type", Profile::StringExpressionNode)
        .field("source_info", &Profile::StringExpressionNode::source_info)
        .EMVAL_FIELD("fragments", StringFragmentsMember<&Profile::StringExpressionNode::fragments>)
        ;

    value_object<Profile::ObjectNode>("ObjectNode")
        .TYPE_FIELD("type", Profile::ObjectNode)
        .field("source_info", &Profile::ObjectNode::source_info)
        .field("docstring", &Profile::ObjectNode::docstring)
        .field("classname", &Profile::ObjectNode::classname)
        .field("id", &Profile::ObjectNode::id)
        .EMVAL_FIELD("children", ObjectNodesMember<&Profile::ObjectNode::children>)
        .EMVAL_FIELD("attributes", AttributeNodesMember<&Profile::ObjectNode::attributes>)
        ;

    value_object<Profile::ReferenceNode>("ReferenceNode")
        .TYPE_FIELD("type", Profile::ReferenceNode)
        .field("source_info", &Profile::ReferenceNode::source_info)
        .field("reference", &Profile::ReferenceNode::reference)
        ;

    value_object<Profile::AttributeNode>("AttributeNode")
        .field("source_info", &Profile::AttributeNode::source_info)
        .field("value_source_info", &Profile::AttributeNode::value_source_info)
        .field("name", &Profile::AttributeNode::name)
        .EMVAL_FIELD("value", ValueMember<&Profile::AttributeNode::value>)
        ;
}
