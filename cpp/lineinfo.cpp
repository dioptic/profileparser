#include <profileparser.hpp>
#include <profileparser/utils.hpp>
#include "peglib.h"

namespace Profile
{

LineInfo get_line_info(const std::string_view& source, const size_t offset)
{
    // TODO: `peg::line_info` correctly counts unicode characters, but lacks line-cache like `vs.line_info`.
    const auto* sourceStart = source.data();
    auto [line, column] = peg::line_info(sourceStart, sourceStart + offset);
    return LineInfo{ .line=line, .column=column };
}

LineRange get_source_range(const ParsedProfile& profile, SourceInfo info)
{
    return get_line_range(profile.source, info);
}

} // namespace Profile
