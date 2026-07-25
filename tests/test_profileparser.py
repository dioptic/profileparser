
from dioptic import profileparser

PROFILE_SOURCE = """
Profile {
    description: "Example profile for testing"

    Value x {
        value: 42
    }

    application: SomeApplication {
        a: 2 * x
        b: "hello world"
    }
}
"""


def test_parse_and_validate_roundtrip():
    profile = profileparser.parse(PROFILE_SOURCE)
    profileparser.validate(profile)

    source = profile.to_source()
    assert isinstance(source, str)
    assert "Profile" in source
    assert "SomeApplication" in source


def test_json_and_line_info():
    profile = profileparser.parse(PROFILE_SOURCE)

    json_ast = profile.to_json_ast()
    assert isinstance(json_ast, str)
    assert "Profile" in json_ast

    line_range = profile.line_info(profile.root())
    assert line_range.begin.line >= 1
    assert line_range.end.line >= line_range.begin.line
