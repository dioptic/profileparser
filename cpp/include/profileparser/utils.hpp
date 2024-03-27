#pragma once
#include <algorithm>
#include <sstream>
#include <profileparser/types.hpp>

namespace Profile {

/**
 * @brief Locate line & column number of given source info.
 */
LineInfo get_line_info(const std::string_view& source, const size_t offset);

/**
 * @brief Locate line & column number of given source info.
 */
inline LineInfo get_line_info(const std::string_view& source, const SourceInfo& info)
{
    return get_line_info(source, info.offset);
}

/**
 * @brief Locate line & column range of given source info.
 */
inline LineRange get_line_range(const std::string_view& source, const SourceInfo& info)
{
    return LineRange{
        .begin=get_line_info(source, info.offset),
        .end=get_line_info(source, info.offset + info.length),
    };
}

/**
 * @brief Find named attribute field in parsed object node.
 */
inline AttributeNodePtr find_attribute(const ObjectNode& obj, const std::string& name)
{
    auto it = std::find_if(obj.attributes.begin(), obj.attributes.end(), [&](const auto& a) {
        return a->name == name;
    });
    if (it == obj.attributes.end()) {
        return nullptr;
    }
    return (*it);
}

/**
 * @brief visit_values Visit profile value objects.
 * @param profile Profile object.
 * @param visitor Value object visitor.
 */
inline void visit_values(ObjectNodePtr& profile, auto&& visitor)
{
    const auto isValue = [](const ObjectNodePtr& obj) -> bool {
        return obj->classname == "Value";
    };
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
inline void visit_values(const ObjectNodePtr& profile, auto&& visitor)
{
    const auto isValue = [](const ObjectNodePtr& obj) -> bool {
        return obj->classname == "Value";
    };
    for (const auto& obj : profile->children) {
        if (!isValue(obj)) { continue; }
        visitor(obj);
    }
}

/**
 * @brief Template for implementing Profile AST visitors.
 */
template<typename VisitorImpl>
struct Visitor
{
    template<typename T>
    void operator()([[maybe_unused]] const T& v) { }

    template<typename T>
    void operator()(const std::shared_ptr<T>& v) {
        return self()(static_cast<const T&>(*v));
    }

    template<typename... Ts>
    void operator()(const std::variant<Ts...>& v) {
        std::visit([&](const auto& v_) { return self()(v_); }, v);
    }

    void operator()(const ListNode& r) {
        for(const auto& v: r.items) { self()(v); };
    }

    void operator()(const DictNode& r) {
        for(const auto& v: r.items) { self()(v); };
    }

    void operator()(const ObjectNode& obj) {
        for (const ObjectNodePtr& child: obj.children) { self()(child); }
        for (const AttributeNodePtr& attr: obj.attributes) { self()(attr->value); }
    }

private:
    inline VisitorImpl& self() { return static_cast<VisitorImpl&>(*this); }
};

/**
 * @brief Visitor for collecting Objects and Values with an identifier
 */
struct IdCollector : protected Visitor<IdCollector>
{
    IdCollection& collection;
    std::string_view source;
    std::vector<ErrorLine>& errors;

    IdCollector(IdCollection& collection, std::string_view source, std::vector<ErrorLine>& errors)
        : Visitor<IdCollector>{}
        , collection{collection}
        , source{source}
        , errors{errors}
    { }

    void operator()(const ObjectNode& obj)
    {
        // Object with id, check for duplicates
        if (!obj.id.empty()) {
            if (!collection.object_ids.emplace(obj.id, &obj).second) {
                add_error("Object id '" + obj.id + "' was declared multiple times", obj.source_info);
            }
        }
        // If object is 'Value', store reference to value attribute
        if (obj.classname == "Value") {
            if (obj.id.empty()) {
                add_error("Value object is missing id", obj.source_info);
            }
            auto value_attr = find_attribute(obj, "value");
            if (value_attr == nullptr) {
                add_error("Value object '" + obj.id + "' is missing 'value' attribute", obj.source_info);
            } else
                collection.value_ids.emplace(obj.id, &(*value_attr));
        }
        // Continue search for objects in children and attribute list
        Visitor<IdCollector>::operator()(obj);
    }

    inline void add_error(auto&& message, SourceInfo where)
    {
        errors.emplace_back(ErrorLine{
            .where=get_line_info(source, where),
            .message=std::forward<decltype(message)>(message),
        });
    }

protected:
    friend struct Visitor<IdCollector>;
    using Visitor::operator();
};


inline ProfileError format_error(const std::string_view& message, std::vector<ErrorLine>&& errors)
{
    std::stringstream ss;
    ss << std::move(message);
    for (auto& line : errors) {
        ss << '\n' << line.where.column << ':' << line.where.column << ' ' << line.message;
    }
    return ProfileError { ss.str(), std::move(errors) };
}

/**
 * @brief Create ID collection from parsed profile.
 * 
 * @throws ProfileError Thrown if profile contains invalid ID information.
 */
inline IdCollection collect_ids(const ObjectNodePtr& obj, std::string_view source)
{
    IdCollection collection;
    std::vector<ErrorLine> errors;
    IdCollector collect(collection, source, errors);
    collect(*obj);
    if (!errors.empty()) {
        throw format_error("Error assembling object ID information", std::move(errors));
    }
    return collection;
}

}  // namespace Profile
