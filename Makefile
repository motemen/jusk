SRC = *.hs

FLAGS = -W

all: js

js: $(SRC)
	ghc --make Main.hs -o js $(FLAGS)

tags: $(SRC)
	hasktags -c *.hs

clean:
	rm *.hi *.o *.exe js

test:
	./js -e "load('../t/mochikit-test.js')"
