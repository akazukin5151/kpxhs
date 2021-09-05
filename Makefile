none:
	$(error Run either `make build` or `make install`)

build:
	cd docs && make theme
	stack build

install:
	cd docs && make theme
	stack install

clean:
	$(warning WARNING: docs/out/ SHOULD be committed to the repo)
	$(warning because it provides online documentation)
	$(warning Please do not immediately commit; run `make docs` first)
	$(warning Only use to test the above commands)
	rm -r docs/out/

.PHONY: docs
docs:
	cd docs && make
