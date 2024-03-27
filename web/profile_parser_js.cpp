#include <type_traits>
#include <emscripten.h>
#include <emscripten/bind.h>

#include <profileparser.hpp>
#include "binding_ast.hpp"
#include "binding_api.hpp"

using namespace emscripten;
using namespace std::string_view_literals;


// Embind module definition
// ------------------------------------------------------------------------------------------------------

EMSCRIPTEN_BINDINGS(profile_parser_module)
{
    register_ast_bindings();
    register_api_bindings();
}
