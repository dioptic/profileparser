#pragma once
#include <profileparser/types.hpp>

namespace Profile {

/**
 * @brief Parse profile from source string.
 * 
 * @param source Profile source.
 * @throws ProfileError Thrown if parsing profile from source failed.
 * @return Parsed profile.
 */
ParsedProfile parse(std::string source);

/**
 * @brief Get source information from node source reference
 * @param profile Parsed profile.
 * @param info Source reference from node.
 * @return Corresponding line/column source range.
 */
LineRange get_source_range(const ParsedProfile& profile, SourceInfo info);

/**
 * @brief Serialize profile object to profile source.
 * @param profile Parsed profile.
 * @return Profile source serialization.
 */
std::string to_profile_source(const ParsedProfile& profile);

/**
 * @brief Re-emit profile source with given value substitutions.
 * 
 * @param profile Parsed profile.
 * @param values Value substitutions.
 * @return Updated profile source.
 */
std::string to_updated_source(const ParsedProfile& profile, const std::unordered_map<std::string, Value>& values);
std::string to_updated_source(const ParsedProfile& profile, const std::unordered_map<const AttributeNode*, Value>& values);

/**
 * @brief Serialize profile object to JSON representation.
 * @param profile Parsed profile.
 * @param eval_expressions If true, serialize with expressions evaluated to constants.
 * @return JSON serialization.
 */
std::string to_json_ast(const ParsedProfile& profile, const bool eval_expressions);

/**
 * @brief Validate profile for use in `argosprocess`.
 * 
 * @param profile Parsed profile.
 * @param is_allowed_ref Callback for validating external references.
 * 
 * @throws ProfileError Thrown if semantic validation failed.
 */
void validate_profile(const ParsedProfile& profile, const CheckRefCallback& is_allowed_ref = default_ref_callback);

}  // namespace Profile
