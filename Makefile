SRC = *.hs

FLAGS = -fwarn-overlapping-patterns \
        -fwarn-unused-binds \
        -fwarn-unused-imports \
        -fwarn-unused-matches \
		-O3 \
		-v

all: js

js: $(SRC)
	ghc --make Main.hs -o js $(FLAGS)

tags: $(SRC)
	hasktags -c *.hs

clean:
	rm *.hi *.o *.exe js
