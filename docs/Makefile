# Run `make` to convert md into man; `make copy` to copy that
# file to your local manual dir (no superuser needed)

build:
	pandoc --standalone --to man kpxhs.1.md -o kpxhs.1

copy:
	mkdir -p ~/.local/share/man/man1/
	cp kpxhs.1 ~/.local/share/man/man1/kpxhs.1
	# note that mandb might need to be refreshed, eg for `whatis` to work
	# just run `mandb`
	# commented out because this is a copy command, not an install command
