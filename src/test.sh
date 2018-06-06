for folder in test/*; do
    for filename in $folder/*.lgl; do
        echo $filename
        cat $filename | jbuilder exec bin/ex.bc a > "${filename%.*lgl}.expect"
    done
done