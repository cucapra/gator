#!/usr/bin/env python3
import sys
from subprocess import check_output, call
import json

def jsonify(path):
    """Jsonify vertex and fragment shader."""

    # retrieve and generate appropriate JSON string
    vert = check_output(["dune", "exec", "bin/lingc.bc", path + "/vertex.lgl"])
    vert = vert.decode("utf-8")
    frag = check_output(["dune", "exec", "bin/lingc.bc", path + "/fragment.lgl"])
    frag = frag.decode("utf-8")

    # write into JSON file
    with open(path + "/data.json", "w+") as file:
        json.dump({
            'vertex': vert,
            'fragment': frag,
        }, file)

if __name__ == '__main__':
    path = sys.argv[1]
    jsonify(path)
