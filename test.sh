#!/bin/bash

# Define the versions of Lua and LuaJIT you want to test
lua_versions=("lua5.1" "lua5.2" "lua5.3" "lua5.4" "luajit")

# Loop through each version and run the tests
for version in "${lua_versions[@]}"; do
  # Check if the version is installed
  if command -v $version > /dev/null 2>&1; then
    echo "Running tests with $version..."
    $version test.lua
    if [ $? -ne 0 ]; then
      echo "Tests failed with $version"
      exit 1
    fi
  else
    echo "$version is not installed"
  fi
done

lua test.lua

echo "All tests passed for all installed Lua versions"