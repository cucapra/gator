### File Structure

examples/
--> .gitignore
	Makefile
	README.md
	tsconfig.json
	package.json
	types.d.ts
	'example'/ (e.g. lighting/)
	--> index.html
		main.ts
		types.d.ts (optional, includes a reference to ../types.d.ts)
		'example'_v.lgl
		'example'_f.lgl
		'example'.json

Run `SRC=<example_directory_name> npm start` to run example directory