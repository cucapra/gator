#!/usr/bin/env python
# -*- coding: utf-8 -*-

import subprocess
import os
import filecmp
import random


TEST_SYMBOLS = "test/symbols.txt"
SUCCESS_COUNT = 3


def get_symbols():
    fail = False
    success_symbols = []
    fail_symbols = []
    with open(TEST_SYMBOLS, "r") as f:
        for line in f:
            line = line.strip()
            print(line)
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


def main():
    success_symbols, fail_symbols = get_symbols()
    for path, _, files in os.walk("test/"):
        lglfiles = filter(lambda x: x.endswith(".lgl"), files)
        any_fails = True  # Trick to avoid printing excess successes
        if len(lglfiles) > 0:
            print("Running tests in " + path + ":")
            any_fails = False
        for filename in lglfiles:
            filename = path + "/" + filename
            basename = filename[:-4]  # Remove the extension
            outname = basename + ".out"
            with open(outname, "w") as f:
                subprocess.call(
                    ("jbuilder", "exec", "bin/lingc.bc", filename, "v"), stdout=f, stderr=f)
            # We write and then read to avoid memory shenanigans
            # (this might be worse actually, but I don't think it matters)
            try:
                if not filecmp.cmp(basename + ".expect", outname):
                    any_fails = True
                    print(random.choice(fail_symbols) + " " + basename)
            except IOError:
                any_fails = True
                print(random.choice(fail_symbols) + " " +
                      basename + ".expect not found")
        if not any_fails:
            for _ in range(SUCCESS_COUNT):
                print(random.choice(success_symbols)),
            print()


if __name__ == "__main__":
    main()
