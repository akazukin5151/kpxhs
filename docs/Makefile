# Run `make` to convert md into man; `make copy` to copy that
# file to your local manual dir (no superuser needed)

build: theme
	cat src/part-1.md out/default_theme.hs src/part-2.md > out/kpxhs.1.md
	pandoc --standalone --to man out/kpxhs.1.md -o out/kpxhs.1

copy:
	mkdir -p ~/.local/share/man/man1/
	cp out/kpxhs.1 ~/.local/share/man/man1/kpxhs.1
	# note that mandb might need to be refreshed, eg for `whatis` to work
	# just run `mandb`
	# commented out because this is a copy command, not an install command

theme:
	python3 src/section.py
