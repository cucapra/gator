dist/trivial.bundle.js: trivial.js
	yarn run build

install:
	npm install

# Command to open a file in a browser.
OPEN := explorer  # Windows.
ifeq ($(shell uname),Darwin)
	OPEN := open  # MacOS.
endif

.PHONY: view
view: dist/trivial.bundle.js
	$(OPEN) index.html
