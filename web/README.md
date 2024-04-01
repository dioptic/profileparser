# Build instructions

Install and activate Emscripten (see https://emscripten.org/docs/getting_started/downloads.html).

Within the activated emscripten environment, configure and build the module with cmake:

```shell
mkdir build && cd build
cmake -DCMAKE_TOOLCHAIN_FILE="${EMSDK}/upstream/emscripten/cmake/Modules/Platform/Emscripten.cmake" -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
```
