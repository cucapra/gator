#!/usr/bin/env python
# -*- coding: utf-8 -*-

import subprocess
import os
import filecmp
import random


TEST_SYMBOLS = "test/symbols2.txt"
SUCCESS_COUNT = 5


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


def test_exception(outname, expectname):
    # Special case for exceptions
    # Ignore the text of the exception, we just want the type of exception that occured
    # (And, of course, that an exception occured)
    try:
        with open(outname, "r") as outf:
            with open(expectname, "r") as expf:
                outval = next(outf)
                expval = next(expf)
                # Special case party!
                parsing_error = "Fatal error: exception Failure(\"Parsing error"
                if expval.startswith(parsing_error):
                    return outval.startswith(parsing_error)
                if expval.startswith("Fatal error: exception"):
                    pindex = expval.index("(")
                    return outval[:pindex] == expval[:pindex]
                return False
    except:
        return False


def main():
    success_symbols, fail_symbols = get_symbols()
    any_fails = True  # Trick to avoid printing excess successes
    for path, _, files in os.walk("test/"):
        lglfiles = filter(lambda x: x.endswith(".lgl"), files)
        if len(lglfiles) > 0:
            print("🏃‍   Running tests in " + path + ":")
            any_fails = False
        for filename in lglfiles:
            filename = path + "/" + filename
            basename = filename[:-4]  # Remove the extension
            outname = basename + ".out"
            expectname = basename + ".expect"
            with open(outname, "w") as f:
                subprocess.call(
                    ("jbuilder", "exec", "bin/lingc.bc", filename, "v"), stdout=f, stderr=f)
            # We write and then read to avoid memory shenanigans
            # (this might be worse actually, but I don't think it matters)
            try:
                if not filecmp.cmp(outname, expectname)\
                        and not test_exception(outname, expectname):
                    any_fails = True
                    print("\t❌  " + basename + " " + random.choice(fail_symbols))
            except IOError:
                any_fails = True
                print("\t❌  " + expectname + " not found " + 
                    random.choice(fail_symbols))
    if not any_fails:
        print("No 👏   Failures 👏")
        for _ in range(SUCCESS_COUNT):
            print(random.choice(success_symbols) + "  "),
        print("")  # newline


if __name__ == "__main__":
    main()
