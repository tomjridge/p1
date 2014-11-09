all:
	cd build && make

clean:
	cd build && make clean

index.html: README.md
	pandoc -s $< > $@

