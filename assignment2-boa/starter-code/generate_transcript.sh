#!/usr/bin/env bash
set -euo pipefail

shopt -s nullglob
tests=(test/*.snek)
if [ ${#tests[@]} -eq 0 ]; then
  echo "No .snek tests found in test/." >&2
  exit 1
fi

make clean
: > transcript.txt

for f in "${tests[@]}"; do
  t="${f#test/}"
  t="${t%.snek}"
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