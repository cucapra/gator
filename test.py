#!/usr/bin/env python
# -*- coding: utf-8 -*-

import subprocess
import os
import filecmp
import random

def main():
	for path,_,files in os.walk("test/"):
		lglfiles = filter(lambda x : x.endswith(".lgl"), files)
		if len(lglfiles) > 0:
			print("Running tests in " + path + ":")
		for filename in lglfiles:
			filename = path + "/" + filename
			basename = filename[:-4]  # Remove the extension
			outname = basename + ".out"
			with open(outname, "w") as f:
				subprocess.call(("jbuilder", "exec", "bin/lingc.bc", filename, "v"), stdout=f, stderr=f)
			# We write and then read to avoid memory shenanigans (this might be worse actually, but I don't think it matters)
			try:
				if not filecmp.cmp(basename + ".expect", outname):
					print("❌ " + basename)
			except FileNotFoundError:
				print("❌ " + basename + ".expect not found")
				
if __name__=="__main__":
	main()