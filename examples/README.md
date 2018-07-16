### File Structure

examples/
--> .gitignore
	Makefile
	README.md
	tsconfig.json
	types.d.ts
	webpack.config.js
	package.json
	'example'/ (e.g. lighting/)
	--> index.html
		'example'.ts
		types.d.ts (includes a reference to ../types.d.ts)
		'example'_v.lgl
		'example'_f.lgl
		'example'.json

Run `SRC=<example_directory_name> npm start` to run example directory