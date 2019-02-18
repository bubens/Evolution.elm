#!/bin/bash

ID=$(date +%s)
SOURCE="./src/Main.elm"
TMP_OUT_ELM="/tmp/$(date +%s)_evolution.elm.js"
TMP_OUT_UJS="/tmp/$(date +%s)_evolution.ujs.js"
OUTPUT="./rel/evolution.min.js"

echo "Making Evolution..."
echo "Step 1: Make"
elm make --optimize --output=$TMP_OUT_ELM $SOURCE
echo "Step 2: Compress"
uglifyjs $TMP_OUT_ELM --output=$TMP_OUT_UJS --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe'
echo "Step 3: Mangle"
uglifyjs $TMP_OUT_UJS --output=$OUTPUT --mangle
echo "Results:"
echo "   Orginal size: $(cat $SOURCE | wc -c) bytes ($SOURCE)"
echo "   Compiled size: $(cat $TMP_OUT_ELM | wc -c) bytes ($TMP_OUT_ELM)"
echo "   Compressed size: $(cat $TMP_OUT_UJS | wc -c) bytes ($TMP_OUT_UJS)"
echo "   Mangeled size: $(cat $OUTPUT | wc -c) bytes ($OUTPUT)"
echo "Done"
