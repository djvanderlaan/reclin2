#!/bin/bash

function build {
  copy_lib
  document
  echo "Building package"
  R CMD build ./
}

function check {
  build
  package=$(ls reclin2*.tar.gz | tail -n 1)
  echo "Checking package $package"
  R CMD check --as-cran $package
}

function clean {
  echo "Cleaning up binaries"
  rm -f ./src/*.o ./src/*.so
}

function document {
  echo "Generating documentation..."
  Rscript  -e "roxygen2::roxygenise()"
}

function test {
  copy_lib
  echo "Running tests"
  for file in tests/test*; do \
    Rscript -e "devtools::load_all(); source('$file')";\
  done
}


case $1 in 
  build) 
    build
    ;;
  check)
    check
    ;;
  clean)
    clean
    ;;
  document)
    document
    ;;
  test)
    test
    ;;
  *)
    echo $"Usage: $0 {build|check|clean|document|test}"
esac

