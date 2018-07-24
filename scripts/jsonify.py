#!/usr/bin/env python3
"""Read the contents of a list of files specified on the command line
and dump a JSON object containing their text, keyed by their basenames.
"""

import sys
import json
import os


def jsonify(paths):
    """Dump a JSON object to stdout whose keys are the basenames of the
    given paths (sans extension) and whose values are the corresponding
    file contents.
    """
    out = {}
    for path in paths:
        name, _ = os.path.splitext(os.path.basename(path))
        with open(path) as f:
            out[name] = f.read()
    json.dump(out, sys.stdout)


if __name__ == '__main__':
    jsonify(sys.argv[1:])
