# For roslyn omnsharp
export LD_LIBRARY_PATH="$HOME/libuvinstall/lib"
export LIBRARY_PATH="$HOME/libuvinstall/lib"

if [ "${YCM_BENCHMARK}" == "true" ]; then
  ./benchmark.py
elif [ "${YCM_CLANG_TIDY}" == "true" ]; then
  ./build.py --clang-completer --clang-tidy --quiet --no-regex
else
  ./run_tests.py
fi
