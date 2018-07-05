#!/bin/sh
for folder in test/*/; do
    for filename in $folder*.lgl; do
        echo $filename
        jbuilder exec bin/ex.bc $filename v p > "${filename%.*lgl}.out"
    done
done
for folder in test/fails/*/; do
    for filename in $folder*.lgl; do
        echo $filename
        jbuilder exec bin/ex.bc $filename v p &> "${filename%.*lgl}.out"
    done
done
