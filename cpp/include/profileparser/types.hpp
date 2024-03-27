#pragma once

#include <memory>
#include <variant>
#include <vector>
#include <string>
#include <tuple>
#include <functional>
#include <unordered_map>
#include <stdexcept>

namespace Profile {

// AST elements

struct SourceInfo {
    size_t offset;
    size_t length;
};

using Boolean = bool;
using Integer = long;
using Float = double;
using String = std::string;

struct ListNode;
using ListNodePtr = std::shared_ptr<ListNode>;
struct DictNode;
using DictNodePtr = std::shared_ptr<DictNode>;

struct NumericExpressionNode;
using NumericExpressionNodePtr = std::shared_ptr<NumericExpressionNode>;
struct StringExpressionNode;
using StringExpressionNodePtr = std::shared_ptr<StringExpressionNode>;

struct ObjectNode;
using ObjectNodePtr = std::shared_ptr<ObjectNode>;
struct ReferenceNode;
using ReferenceNodePtr = std::shared_ptr<ReferenceNode>;
struct AttributeNode;
using AttributeNodePtr = std::shared_ptr<AttributeNode>;

struct ReferenceNode {
    SourceInfo source_info;
    std::string reference;
};

using Value = std::variant<ObjectNodePtr, ReferenceNodePtr, NumericExpressionNodePtr, StringExpressionNodePtr, ListNodePtr, DictNodePtr, Boolean, Integer, Float, String>;

struct KeyVal {
    std::string key;
    Value value;
};

struct DocString {
    SourceInfo source_info;
    std::vector<std::string> lines; 
};

struct AttributeNode {
    SourceInfo source_info;
    SourceInfo value_source_info;
    std::string name;
    Value value;
};

struct ObjectNode {
    using Item = std::variant<AttributeNodePtr, ObjectNodePtr>;

    SourceInfo source_info;
    DocString docstring;
    std::string classname;
    std::string id;
    std::vector<ObjectNodePtr> children;
    std::vector<AttributeNodePtr> attributes;
};

struct ListNode {
    SourceInfo source_info;
    std::string id;
    std::vector<Value> items;
};

struct DictNode {
    SourceInfo source_info;
    std::string id;
    std::vector<KeyVal> items;
};

struct NumericExpressionNode {
    SourceInfo source_info;
    std::string value;
    std::vector<ReferenceNodePtr> references;
};

using StringFragment = std::variant<String, ReferenceNodePtr>;

struct StringExpressionNode {
    SourceInfo source_info;
    std::vector<StringFragment> fragments;
};

// Parser API

using CheckRefCallback = std::function<bool(const std::string&)>;
static inline const CheckRefCallback default_ref_callback = [](const std::string&) { return false; };

struct IdCollection {
    std::unordered_map<std::string, const ObjectNode*> object_ids;
    std::unordered_map<std::string, const AttributeNode*> value_ids;
};

struct ParsedProfile {
    std::string source;
    ObjectNodePtr root;
    IdCollection ids;
};

struct LineInfo {
    size_t line;
    size_t column;
};

struct LineRange {
    LineInfo begin;
    LineInfo end;
};

struct ErrorLine {
    LineInfo where;
    std::string message;
};

struct ProfileError : public std::runtime_error {
    ProfileError(std::string message, std::vector<ErrorLine> errors)
        : std::runtime_error{std::move(message)}
        , errors{std::move(errors)}
    {}

    std::vector<ErrorLine> errors;
};

}
