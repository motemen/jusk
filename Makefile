SRC = *.hs

FLAGS = -fwarn-overlapping-patterns \
        -fwarn-unused-binds \
        -fwarn-unused-imports \
        -fwarn-unused-matches \
		-v0

all: js tags

js: $(SRC)
	ghc --make Main.hs -o js $(FLAGS)

test: $(SRC) js
	ruby test.rb

tags: $(SRC)
	hasktags -c *.hs

clean:
	rm *.hi *.o
