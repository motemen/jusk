SRC = *.hs

FLAGS = -W -O

all: js

js: $(SRC)
	ghc --make Main.hs -o js $(FLAGS)

tags: $(SRC)
	hasktags -c *.hs

clean:
	rm *.hi *.o *.exe js
