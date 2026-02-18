#!/usr/bin/env bash
set -euo pipefail

tests=(37 add add1 negate negate_nested complex sub1 mix zero big num example)

make clean
: > transcript.txt

for t in "${tests[@]}"; do
  {
    printf '$ cat test/%s.snek\n' "$t"
    cat "test/$t.snek"
    printf '\n$ make test/%s.run\n' "$t"
    make "test/$t.run"
    printf '\n$ cat test/%s.s\n' "$t"
    cat "test/$t.s"
    printf '\n$ ./test/%s.run\n' "$t"
    "./test/$t.run"
    printf '\n'
  } >> transcript.txt 2>&1
done

cat transcript.txt
