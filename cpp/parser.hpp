#pragma once

namespace Profile {

using ValueSubstitutions = std::unordered_map<std::string, Value>;

/**
 * @brief Parse numeric expression (i.e. not a whole profile) with value substitutions.
 */
Value parseExpression(const std::string_view& expr, const ValueSubstitutions& values);

} // namespace Profile
