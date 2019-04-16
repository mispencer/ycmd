if [ "${YCM_BENCHMARK}" == "true" ]; then
  ./benchmark.py
elif [ "${YCM_CLANG_TIDY}" == "true" ]; then
  ./build.py --clang-completer --clang-tidy --quiet --no-regex
else
  ./run_tests.py
  ./run_tests.py --skip-build
  ./run_tests.py --skip-build
  ./run_tests.py --skip-build
fi
