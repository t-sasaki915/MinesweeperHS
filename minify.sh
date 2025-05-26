#!/bin/bash

set -e

for i in $(find ./build/ -name '*.html'); do
    echo $(minify $i) > $i

    echo Minified HTML $i
done

for i in $(find ./build/ -name '*.js'); do
    echo $(minify $i) > $i

    echo Minified JavaScript $i
done

for i in $(find ./build/ -name '*.css'); do
    echo $(csso $i) > $i

    echo Minified CSS $i
done

echo ""
echo MINIFICATION SUCCESSFUL.
