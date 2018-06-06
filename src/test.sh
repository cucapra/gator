for filename in test/basics/*.lgl; do
    echo $filename
    cat $filename | jbuilder exec bin/ex.bc a > "${filename%.*lgl}.expect"
done