#include <functional>
#include <unordered_set>
#include <unordered_map>
#include <profileparser/utils.hpp>
#include <profileparser.hpp>

namespace Profile {

// Semantic checking of profile structure

struct SemanticVerifier : protected Visitor<SemanticVerifier>
{
    SemanticVerifier(
        const ParsedProfile& profile,
        const CheckRefCallback& is_allowed_ref,
        std::vector<ErrorLine>& errors
        )
        : Visitor<SemanticVerifier>{}
        , profile(profile)
        , is_allowed_ref(is_allowed_ref)
        , errors{errors}
    { }

    void operator()(const ObjectNode& obj)
    {
        // If object is 'Profile', require description and application attribute
        if (obj.classname == "Profile") {
            auto descr = find_attribute(obj, "description");
            if (descr == nullptr) {
                add_error("Profile object is missing description attribute", obj.source_info);
            } else {
                // TODO: Create method for checking attribute data types
                if (!std::holds_alternative<String>(descr->value)) {
                    add_error("Profile description must be a (plain) string", obj.source_info);
                }
            }
            auto application = find_attribute(obj, "application");
            if (application == nullptr) {
                add_error("Profile object is missing application attribute", obj.source_info);
            } else {
                if (!std::holds_alternative<ObjectNodePtr>(application->value)) {
                    add_error("Application must be an object", application->source_info);
                }
            }
        }
        if (obj.classname == "Include") {
            if (obj.children.size() > 0) {
                add_error("Include object does not support children", obj.source_info);
            }
            if (!find_attribute(obj, "path")) {
                add_error("Include object is missing path attribute", obj.source_info);
            }
            if (obj.attributes.size() != (find_attribute(obj, "values") ? 2 : 1)) {
                add_error("Include object contains unsupported attributes", obj.source_info);
            }
        }
         // Require unique attribute identifiers within object
        {
            std::unordered_set<std::string> attribute_names;
            for (const auto& a: obj.attributes) {
                if (!attribute_names.emplace(a->name).second) {
                    add_error("Attribute '" + a->name + "' was declared multiple times", a->source_info);
                };
            }
        }
        // Verify children and attributes
        Visitor<SemanticVerifier>::operator()(obj);
    }

    inline void add_error(auto&& message, SourceInfo where)
    {
        errors.emplace_back(ErrorLine{
            .where=get_line_info(profile.source, where),
            .message=std::forward<decltype(message)>(message),
        });
    }

protected:
    friend struct Visitor<SemanticVerifier>;
    using Visitor::operator();

    const ParsedProfile& profile;
    const CheckRefCallback& is_allowed_ref;
    std::vector<ErrorLine>& errors;

    void operator()(const ReferenceNode& ref)
    {
        if(is_allowed_ref(ref.reference))
            return;
        if (!profile.ids.object_ids.contains(ref.reference)) {
            add_error("Unresolved reference: " + ref.reference, ref.source_info);
        }
    }

    void operator()(const NumericExpressionNode& expr)
    {
        for (const auto& ref: expr.references) {
            if(is_allowed_ref(ref->reference))
                continue;
            if (!profile.ids.object_ids.contains(ref->reference)) {
                add_error("Unresolved reference: " + ref->reference, ref->source_info);
            }
            if (!profile.ids.value_ids.contains(ref->reference)) {
                add_error("Reference to '" + ref->reference + "' not allowed in expression", ref->source_info);
            }
        }
    }

    void operator()(const StringExpressionNode& tstr)
    {
        for (const StringFragment& fragment : tstr.fragments) { (*this)(fragment); }
    }
};

void validate_profile(
    const ParsedProfile& profile,
    const CheckRefCallback& is_allowed_ref
    )
{
    std::vector<ErrorLine> errors;
    SemanticVerifier verify(profile, is_allowed_ref, errors);
    verify(*profile.root);
    if (!errors.empty()) {
        throw format_error("Failed semantic validation", std::move(errors));
    }
}

} // namespace Profile
