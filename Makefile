SRC = *.hs

FLAGS = -fwarn-overlapping-patterns \
        -fwarn-unused-binds \
        -fwarn-unused-imports \
        -fwarn-unused-matches \
		-O \
		-v

all: main tags

main: $(SRC)
	ghc --make Main.hs -o main $(FLAGS)

tags: $(SRC)
	hasktags -c *.hs

clean:
	rm *.hi *.o *.exe main
