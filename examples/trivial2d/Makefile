dist/trivial.bundle.js: trivial.ts
	yarn run build

install:
	yarn

# Command to open a file in a browser.
OPEN := explorer  # Windows.
ifeq ($(shell uname),Darwin)
	OPEN := open  # MacOS.
endif

.PHONY: view
view: 
	tsc trivial2d.ts
	$(OPEN) index.html
