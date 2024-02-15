#!/bin/bash

cnt=1

check_file () {
  if [[ ! -f $1 ]]; then
    echo "[^] $1 file is missing, cannot continue"
    exit 1
  fi
}

# Create build files
run_cmake () {
  check_file "$REPO_PATH/CMakeLists.txt"
  mkdir "$1"
  if ! cmake -S "$REPO_PATH" -B "$1" "${@:2}"; then
    exit 1
  fi
}

# Build after running cmake
run_make () {
  if ! make -C "$1"; then
    exit 1
  fi
}

# Run unit tests after building
run_test () {
  if [[ -f "$1/$2/runUnitTests" ]]; then
	cd "$1"
   	if ! "$1/$2/runUnitTests"; then
      exit 1
    fi
  else
    cd "$1/$2"
    if ! ctest -VV; then
      exit 1
    fi
  fi
  cd "$OLDPWD"
}

build () {
  if [[ -z $REPO_PATH || ! -d $REPO_PATH ]]; then
    echo "Can't find repository, quitting"
    exit 1
  fi

  echo "[$cnt] Generating build files with cmake"
  run_cmake "$1" "${@:2}"
  echo "[+] Cmake success"
  (( cnt++ ))

  echo "[$cnt] Building with make"
  run_make "$1"
  echo "[+] Make success"
  (( cnt++ ))
}

tests () {
  if [[ -d "/build/test" ]]; then
    testdir="test"
  elif [[ -d "/build/tests" ]]; then
    testdir="tests"
  else
    echo "[^] Can't find test directory (tried test and tests)"
    exit 1
  fi

  echo "[$cnt] Running normal tests"
  run_test /build $testdir
  echo "[+] Normal test success"
  (( cnt++ ))

  echo "[$cnt] Running tests with ASAN (Address Sanitizer)"
  build /build_asan -DCMAKE_BUILD_TYPE=ASAN
  run_test /build_asan $testdir
  echo "[+] ASAN test success"
  (( cnt++ ))

  echo "[$cnt] Running tests with USAN (Undefined Behaviour Sanitizer)"
  build /build_usan -DCMAKE_BUILD_TYPE=USAN
  run_test /build_usan $testdir
  echo "[+] USAN test success"
  (( cnt++ ))
}

fmt () {
  if [[ -z $REPO_PATH || ! -d $REPO_PATH ]]; then
    echo "Can't find repository, quitting"
    exit 1
  fi

  # find matching files for clang-format
  file_list="$(find "$REPO_PATH" -regextype posix-extended -regex '.*\.(cpp|h)' -not -regex '.*/.*?(test|debug|cmake).*?/.*')"
  # format using specified clang-format settings (directory with .clang-format)
  if [[ "$1" == "grep" ]]; then
    clang-format "${@:2}" $file_list 1>/tmp/clang-format 2>&1
    status=$?
    cat /tmp/clang-format | grep -v "Formatting"
  else
    clang-format "${@:2}" $file_list
    status=$?
  fi
  return $status
}

help () {
  cat << EOF
usage: $0 [action]
action:
  build - just try to run build and linter on the repo
  test - run the build action and then run tests as well
EOF
}

if [[ $# -lt 1 || $0 == "-h" || $0 == "--help" ]]; then
  help
fi

case $1 in
  build)
    build /build -DUSE_CLANG_TIDY=TRUE -DPATH_TO_ICA=/env/libica-plugin.so
    ;;

  test)
    build /build -DUSE_CLANG_TIDY=TRUE -DPATH_TO_ICA=/env/libica-plugin.so
    tests
    ;;

  checkfmt)
    fmt grep --dry-run --Werror --verbose -i -style=file
    exit $?
    ;;

  fmt)
    fmt nogrep --verbose -i -style=file
    ;;
  *)
    echo -ne "Invalid argument.\n\n"
    help
    exit 1
esac
