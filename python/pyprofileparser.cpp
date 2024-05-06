#include <utility>

#include <Python.h>
#include <nanobind/nanobind.h>
#include <nanobind/stl/function.h>
#include <nanobind/stl/pair.h>
#include <nanobind/stl/shared_ptr.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/unordered_map.h>
#include <nanobind/stl/variant.h>
#include <nanobind/stl/vector.h>

#include <profileparser.hpp>
#include <profileparser/utils.hpp>

namespace py = nanobind;
using namespace nanobind::literals;
using namespace std::string_view_literals;

constexpr auto VERSION = "0.3.2"sv;

struct PyProfile {
    PyProfile(std::string source)
        : m_profile(Profile::parse(std::move(source)))
    { }

    Profile::ObjectNodePtr root() 
    {
        return m_profile.root;
    }

    using AnySourceInfo = std::variant<
        Profile::ObjectNodePtr, Profile::ReferenceNodePtr, Profile::NumericExpressionNodePtr,
        Profile::StringExpressionNodePtr, Profile::ListNodePtr, Profile::DictNodePtr,
        Profile::SourceInfo
        >;

    Profile::LineRange line_info(const AnySourceInfo& info)
    {
        return std::visit([&](const auto& n) -> Profile::LineRange {
            if constexpr (std::is_same_v<decltype(n), const Profile::SourceInfo&>) {
                return Profile::get_source_range(m_profile, n);
            } else {
                return Profile::get_source_range(m_profile, n->source_info);
            }
        }, info);
    }

    std::string to_json_ast(const bool eval_expressions) const
    {
        return Profile::to_json_ast(m_profile, eval_expressions);
    }

    std::string to_source() const
    {
        return Profile::to_profile_source(m_profile);
    }

    using AnySimpleValue = std::variant<int, double, std::string, bool>;

    std::string to_updated_source(const std::unordered_map<std::string, AnySimpleValue>& anyValues) const
    {
        std::unordered_map<std::string, Profile::Value> values;
        for (const auto& [id, value] : anyValues) {
            values.emplace(id, std::visit([](const auto& v) -> Profile::Value { return v; }, value));
        }
        return Profile::to_updated_source(m_profile, values);
    }

    py::dict values() const
    {
        py::dict dct;
        for (const auto& [id, attribute] : m_profile.ids.value_ids) {
            dct[py::str(id.c_str())] = attribute->value;
        }
        return dct;
    }

    void validate(Profile::CheckRefCallback is_allowed_ref = Profile::default_ref_callback) const
    {
        Profile::validate_profile(m_profile, is_allowed_ref);
    }

private:
    Profile::ParsedProfile m_profile;
};


NB_MODULE(profileparser, m) {
    m.attr("__version__") = py::str(VERSION.data(), VERSION.size());
    
    py::class_<Profile::SourceInfo> PySourceInfo(m, "SourceInfo");
    PySourceInfo.def_ro("offset", &Profile::SourceInfo::offset);
    PySourceInfo.def_ro("length", &Profile::SourceInfo::length);

    py::class_<Profile::LineInfo> PyLineInfo(m, "LineInfo");
    PyLineInfo.def_ro("line", &Profile::LineInfo::line);
    PyLineInfo.def_ro("column", &Profile::LineInfo::column);

    py::class_<Profile::LineRange> PyLineRange(m, "LineRange");
    PyLineRange.def_ro("begin", &Profile::LineRange::begin);
    PyLineRange.def_ro("end", &Profile::LineRange::end);

    py::class_<Profile::DocString> PyDocString(m, "DocString");
    PyDocString.def_ro("source_info", &Profile::DocString::source_info);
    PyDocString.def_ro("lines", &Profile::DocString::lines);

    py::class_<Profile::ListNode> PyListNode(m, "ListNode");
    PyListNode.def_ro("source_info", &Profile::ListNode::source_info);
    PyListNode.def_ro("items", &Profile::ListNode::items);

    py::class_<Profile::KeyVal> PyKeyVal(m, "KeyVal");
    PyKeyVal.def_ro("key", &Profile::KeyVal::key);
    PyKeyVal.def_ro("value", &Profile::KeyVal::value);

    py::class_<Profile::DictNode> PyDictNode(m, "DictNode");
    PyDictNode.def_ro("source_info", &Profile::DictNode::source_info);
    PyDictNode.def_ro("items", &Profile::DictNode::items);

    py::class_<Profile::NumericExpressionNode> PyNumericExpressionNode(m, "NumericExpressionNode");
    PyNumericExpressionNode.def_ro("source_info", &Profile::NumericExpressionNode::source_info);
    PyNumericExpressionNode.def_ro("value", &Profile::NumericExpressionNode::value);
    PyNumericExpressionNode.def_ro("references", &Profile::NumericExpressionNode::references);

    py::class_<Profile::StringExpressionNode> PyStringExpressionNode(m, "StringExpressionNode");
    PyStringExpressionNode.def_ro("source_info", &Profile::StringExpressionNode::source_info);
    PyStringExpressionNode.def_ro("fragments", &Profile::StringExpressionNode::fragments);

    py::class_<Profile::ObjectNode> PyObjectNode(m, "ObjectNode");
    PyObjectNode.def_ro("source_info", &Profile::ObjectNode::source_info);
    PyObjectNode.def_ro("docstring", &Profile::ObjectNode::docstring);
    PyObjectNode.def_ro("classname", &Profile::ObjectNode::classname);
    PyObjectNode.def_ro("id", &Profile::ObjectNode::id);
    PyObjectNode.def_ro("children", &Profile::ObjectNode::children);
    PyObjectNode.def_ro("attributes", &Profile::ObjectNode::attributes);

    py::class_<Profile::AttributeNode> PyAttributeNode(m, "AttributeNode");
    PyAttributeNode.def_ro("source_info", &Profile::AttributeNode::source_info);
    PyAttributeNode.def_ro("value_source_info", &Profile::AttributeNode::value_source_info);
    PyAttributeNode.def_ro("name", &Profile::AttributeNode::name);
    PyAttributeNode.def_ro("value", &Profile::AttributeNode::value);

    py::class_<Profile::ReferenceNode> PyReferenceNode(m, "ReferenceNode");
    PyReferenceNode.def_ro("source_info", &Profile::ReferenceNode::source_info);
    PyReferenceNode.def_ro("reference", &Profile::ReferenceNode::reference);

    py::exception<Profile::ProfileError> PyProfileError(m, "ProfileError");
    py::class_<Profile::ErrorLine> PyErrorLine(m, "ErrorLine");
    PyErrorLine.def_ro("where", &Profile::ErrorLine::where);
    PyErrorLine.def_ro("message", &Profile::ErrorLine::message);

    py::class_<PyProfile> cls(m, "Profile");
    cls.def(py::init<const std::string&>(), "source"_a);
    cls.def("values", &PyProfile::values, "Profile values.");
    cls.def("root", &PyProfile::root, "Profile AST root node.");
    cls.def("line_info", &PyProfile::line_info, "Get line info for node.", "info"_a);
    cls.def("to_source", &PyProfile::to_source, "Profile AST source representation.");
    cls.def("to_json_ast", &PyProfile::to_json_ast, "Profile AST JSON representation.", "eval_expressions"_a = true);
    cls.def("to_updated_source", &PyProfile::to_updated_source, "Original source with value substitutions.", "values"_a);

    m.def(
        "parse",
        [](std::string src) -> PyProfile { return PyProfile(std::move(src)); },
        "Parse profile source and create AST.",
        "src"_a
    );

    m.def(
        "validate", 
        [](const PyProfile& profile, const Profile::CheckRefCallback& is_allowed_ref = Profile::default_ref_callback) { profile.validate(is_allowed_ref); },
        "Validate profile structure.",
        "profile"_a,
        "is_allowed_ref"_a.none(false).sig("lambda ref: False") = Profile::default_ref_callback
    );
}
