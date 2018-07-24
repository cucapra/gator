#!/usr/bin/env python3
import sys
from subprocess import check_output, call

def jsonify(path):
    """Jsonify vertex and fragment shader."""

    # retrieve and generate appropriate JSON string
    vert = check_output(["jbuilder", "exec", "bin/lingc.bc", path + "/vertex.lgl"])
    vert = vert.decode("utf-8") 
    frag = check_output(["jbuilder", "exec", "bin/lingc.bc", path + "/fragment.lgl"])
    frag = frag.decode("utf-8")
    json = "{\n\t\"vertex\":" + vert + ",\n\t\"fragment\":" + frag + "\n}"
    
    # write into JSON file
    file = open(path + "/data.json", "w+") 
    file.write(json) 
    file.close() 

if __name__ == '__main__':
    path = sys.argv[1]
    jsonify(path)
