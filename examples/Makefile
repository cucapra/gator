dist/lighting.bundle.js: lighting.ts ../color.json
	yarn run build

install:
	yarn

# Command to open a file in a browser.
OPEN := explorer  # Windows.
ifeq ($(shell uname),Darwin)
	OPEN := open  # MacOS.
endif

.PHONY: view
view: dist/lighting.bundle.js
	$(OPEN) index.html
