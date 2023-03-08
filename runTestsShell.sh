#!/bin/bash

# the purpose of this program is to automate running test cases for shell scripts
# this program takes in a file with the names of test case files as an argument, and a shell script as the other
# note that the names of test case files have the same stem, and there must exist a .args, .in, .expect file
# for each of the test case's arguments, input, and expected output
# it then runs the program to compare outputs with the expected values, and prints when there is a discrepancy

if [ $# -ne 2 ]; then
  echo "Not enough arguments"
  exit 1
fi

filename1="$1"
filename2="$2"

if [ ! -r "$filename1" ]; then
  echo "file is not readable or does not exist"
  exit 1
fi

if [ ! -x "$filename2" ]; then
  echo "file is not executable"
  exit 1
fi

while read -r stem; do
  args="${stem}.args"
  input="${stem}.in"
  expect="${stem}.expect"

  if [ ! -r "$expect" ]; then
    echo "expect file is not readable or does not exist"
    exit 1
  else
    temp=$(mktemp)
    if [ -r "$input" ]; then
      "$filename2" $(<"$args") <"$input" >"$temp"
    else
      "$filename2" $(<"$args") >"$temp"
    fi
    if ! diff -q  "$expect" "$temp" >/dev/null; then
      echo "Test failed: $stem"
      echo "Args:"
      if [ -r "$args" ]; then
        cat "$args"
      fi
      echo "Input:"
      if [ -r "$input" ]; then
        cat "$input"
      fi
      echo "Expected:"
      cat "$expect"
      echo "Actual:"
      cat "$temp"
    fi
    rm "$temp"
  fi
done < "$filename1"
