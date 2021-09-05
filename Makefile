none:
	$(error Run either `make build` or `make install`)

build:
	cd docs && make
	stack build

install:
	cd docs && make
	stack install

clean:
	$(warning WARNING: docs/out/ SHOULD be committed to the repo)
	$(warning because it provides online documentation)
	$(warning Please do not immediately commit; run `make build` first)
	$(warning Only use to test the above commands)
	rm -r docs/out/
