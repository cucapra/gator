#!/usr/bin/env python
# -*- coding: utf-8 -*-

import subprocess
import os
import filecmp
import random
import argparse
import sys
import re
from shutil import copyfile
from decimal import Decimal

parser = argparse.ArgumentParser(description="Flip a switch by setting a flag")
# Flag for CircleCI build
parser.add_argument('-b', action='store_true')
# Flag for typescript
parser.add_argument('-t', action='store_true')

args = parser.parse_args()

TEST_SYMBOLS = "test/symbols2.txt"
SUCCESS_COUNT = 5

# Error messages we look for.
PARSING_ERROR = "Fatal error: exception Failure(\"Parsing error"
EXTERN_ERROR = "Fatal error: exception Failure(\"Unimplemented function"
EXCEPTION_ERROR = "Fatal error: exception"


def get_symbols():
    fail = False
    success_symbols = []
    fail_symbols = []
    with open(TEST_SYMBOLS, "r") as f:
        for line in f:
            line = line.strip()
            if line == "":
                continue
            if line.lower() == "fails":
                fail = True
                continue
            if fail:
                fail_symbols.append(line)
            else:
                success_symbols.append(line)
    return success_symbols, fail_symbols


def test_exception(tempname, expectname):
    # Special case for exceptions
    # Ignore the text of the exception. we just want the type of
    # exception that occurred.
    # (And, of course, that an exception occured)
    try:
        with open(tempname, "r") as outf:
            with open(expectname, "r") as expf:
                outval = next(outf)
                expval = next(expf)
                # Special case party!
                if expval.startswith(PARSING_ERROR):
                    return outval.startswith(PARSING_ERROR)
                if expval.startswith(EXTERN_ERROR):
                    return outval.startswith(EXTERN_ERROR)
                if expval.startswith(EXCEPTION_ERROR):
                    pindex = expval.index("(")
                    return outval[:pindex] == expval[:pindex]
                return False
    except:
        return False

def js_matrix_format(mat):
    # Given a matrix in list format, produces a js-style matrix

    # Make a square matrix
    mat_dim = max(len(mat), max(map(len, mat)))
    pad_list = lambda l : l + [0.0 for _ in range(mat_dim - len(l))]
    mat = list(map(pad_list, mat))
    for _ in range(mat_dim - len(mat)):
        mat.append([0.0 for _ in range(mat_dim)])

    # Transpose the matrix
    for i in range(mat_dim):
        for j in range(i):
            temp = mat[i][j]
            mat[i][j] = mat[j][i]
            mat[j][i] = temp

    # Flatten the matrix into a list
    return [item for row in mat for item in row]

def js_format(line):
    # "Square" matrices, transpose them, and order as a list

    # Ugly but fast way to convert a matrix of floats to a matrix
    # Note that they must all be floats per Lathe's requirements on arrays
    as_float = lambda x : list(map(float, re.findall(r"-?[0-9]+\.[0-9]*", x)))
    as_mat = lambda exp : list(map(as_float, \
        filter(lambda x : len(x) > 0, re.split("\[", exp))))
    # Apply the matrix changes
    to_js = lambda exp : "{}".format(js_matrix_format(as_mat(exp.groups()[0])))
    # And run the whole mess on every matrix
    line = re.sub(r"(\[\[.*\]\])", to_js, line)

    # Replace floats with integers as allowed (eg 5.0 and 5. with 5)
    line = re.sub(r"([0-9]+)\.0([^0-9])", r"\1\2", line)
    line = re.sub(r"([0-9]+)\.([^0-9])", r"\1\2", line)
    # Add Float32Array to precede every matrix and space out bounds
    line = re.sub(r"(\[)", "Float32Array [ ", line)
    line = re.sub(r"(\])", " ]", line)
    return line

def main():
    use_typescript = args.t
    success_symbols, fail_symbols = get_symbols()
    any_fails = False  # Trick to avoid printing excess successes
    for path, _, files in os.walk("test/"):
        lglfiles = [x for x in files if x.endswith(".lgl")]
        if len(lglfiles) > 0:
            print("🏃‍   Running tests in " + path + ":")
        for filename in lglfiles:
            filename = path + "/" + filename
            basename = filename[:-4]  # Remove the extension
            outname = basename + ".out"
            expectname = basename + ".expect"
            gator_args = [] if path == "test/compiler" else \
                ["-t"] if path == "test/compiler_ts" or use_typescript else ["-i"]
            compiler_test = path == "test/compiler" or path == "test/compiler_ts"
            with open(outname, "w") as f:
                if use_typescript and not compiler_test:
                    # https://stackoverflow.com/questions/19020557/redirecting-output-of-pipe-to-a-file-in-python
                    p1 = subprocess.Popen(
                        ["gatorc"] + gator_args + [filename], 
                        stdout=subprocess.PIPE, stderr=f
                    )
                    p2 = subprocess.Popen(
                        ["ts-node"], 
                        stdin=subprocess.PIPE, stdout=f, stderr=f
                    )
                    p1_out = p1.communicate()[0]
                    if (len(p1_out) > 0): # Errors go straight to the file
                        p2.communicate(p1_out)
                    else:
                        # Super important to avoid crashing your computer...
                        p2.kill()
                else:
                    subprocess.call(
                        ["gatorc"] + gator_args + [filename],
                        stdout=f, stderr=f,
                    )
            # We write and then read to avoid memory shenanigans
            # (this might be worse actually, but I don't think it matters)
            try:
                if test_exception(outname, expectname):
                    continue
                if compiler_test and filecmp.cmp(outname, expectname):
                    continue
                tempname = basename + ".temp"
                copyfile(outname, tempname)
                failed = False
                with open(tempname) as tempfile, open(expectname) as expectfile, open(outname, "w") as outfile:
                    for templine, expectline in zip(tempfile, expectfile):
                        error_line = templine.startswith("Fatal")
                        # Round all decimals
                        rounder = lambda exp : "{}".format(Decimal(exp.groups()[0]).quantize(Decimal(10) ** -1))
                        templine = re.sub(r"(-?[0-9]+[\.?][0-9]*)", rounder, templine)
                        expectline = re.sub(r"(-?[0-9]+[\.?][0-9]*)", rounder, expectline)
                        if use_typescript:
                            # It's easier to convert our expect format to typescript output, so we do that
                            expectline = js_format(expectline)
                            error_line = error_line or templine.startswith("[eval]")
                        line_failed = templine != expectline and not error_line
                        failed = failed or line_failed or error_line
                        outfile.write(templine \
                            + (("ERROR: Expected: " + expectline) if line_failed else ""))
                    nextline = next(expectfile, None)
                    # Check to make sure we finished writing the file
                    if nextline is not None:
                        failed = True
                        outfile.write("ERROR: Expected: " + nextline)
                os.remove(tempname)
                if failed:
                    any_fails = True
                    print("\t❌ " + basename + " " +
                        random.choice(fail_symbols))
            except IOError:
                any_fails = True
                print("\t❌ " + expectname + " not found " +
                      random.choice(fail_symbols))
    if not any_fails:
        print("No 👏 Failures 👏")
        for _ in range(SUCCESS_COUNT):
            print(random.choice(success_symbols) + "  "),
        print("")  # newline
    elif args.b:  # for CircleCI builds
        sys.exit(420)


if __name__ == "__main__":
    main()
