#!/usr/bin/env python3
import sys
from subprocess import check_output, call

def jsonify(path, filename):
    """Jsonify vertex and fragment shader."""

    # retrieve and generate appropriate JSON string
    vert = check_output(["jbuilder", "exec", "bin/ex.bc", "examples/" + path + "/" filename + "_v.lgl"])
    vert = vert.decode("utf-8") 
    frag = check_output(["jbuilder", "exec", "bin/ex.bc", "examples/" + path + "/" filename + "_f.lgl"])
    frag = frag.decode("utf-8")
    json = "{\n\t\"vertex\":" + vert + ",\n\t\"fragment\":" + frag + "\n}"
    
    # write into JSON file
    file = open(path + "/" + filename + ".json", "w+") 
    file.write(json) 
    file.close() 

    # move Linguine source files under subfoler
    call(["mv", "$@_f.lgl", "$@/"])
	call(["mv", "$@_v.lgl", "$@/"])

if __name__ == '__main__':
    path = sys.argv[1]
    filename = sys.argv[2]
    jsonify(path, filename)
    init_index(path, filename)