#!/bin/sh
for folder in test/*/; do
    for filename in $folder*.lgl; do
        echo $filename
        jbuilder exec bin/lingc.bc $filename v > "${filename%.*lgl}.out"
    done
done
for folder in test/fails/*/; do
    for filename in $folder*.lgl; do
        echo $filename
        jbuilder exec bin/lingc.bc $filename 2> "${filename%.*lgl}.out"
    done
done
