# Build instructions

## Local build with Emscripten SDK

Install and activate Emscripten (see https://emscripten.org/docs/getting_started/downloads.html).

Within the activated Emscripten environment, configure and build the module with CMake:

```sh
mkdir -p build
cd build
emcmake cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
```

## Local build with Docker

You can build without a local Emscripten installation by using Emscripten docker images:

```sh
docker run --rm \
  -v "$PWD:/src" \
  -w /src \
  emscripten/emsdk:6.0.3 \
  bash -lc "rm -rf web/build && mkdir -p web/build && cd web/build && emcmake cmake -DCMAKE_BUILD_TYPE=Release .. && cmake --build ."
```

Build artifacts are written to:

- `web/build/ProfileParserJs.mjs`
- `web/build/ProfileParserJs.wasm`
- `web/build/ProfileParserJs.d.ts`
