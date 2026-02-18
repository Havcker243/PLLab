#!/usr/bin/env bash
set -euo pipefail

labs=(assignment1-adder assignment2-boa assignment3-cobra assignment4-diamondback)
for lab in "${labs[@]}"; do
  dir="$lab/starter-code"
  echo "=== $lab ==="
  if [ ! -d "$dir" ]; then
    echo "Missing directory: $dir" >&2
    exit 1
  fi
  (
    cd "$dir"
    if [ ! -f generate_transcript.sh ]; then
      echo "Missing script: $dir/generate_transcript.sh" >&2
      exit 1
    fi
    bash generate_transcript.sh
  )
done