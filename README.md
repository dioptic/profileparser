# Dioptic declarative profile parser

Python parser library for ARGOS declarative profiles.

## Build prerequisites

- Python `3.12+`
- CMake `>=3.24`
- Ninja
- C++20 compiler

## Build with dependencies from lockfile

```shell
uv sync --no-install-project
uv build --wheel --no-build-isolation
```

## Develop with auto rebuild

```shell
uv sync
uv run ruff check .
uv run pytest
```

## Usage

```python
import dioptic.profileparser as profileparser

profile = profileparser.parse("""
Profile {
    description: "Example profile"
    application: MyApp {
        hello: "world"
    }
}
"""
)

profileparser.validate(profile)
print(profile.to_json_ast())
```
