SRC  = Tests_Lab1.hs
TEST = test_lab1
OPTS = -W

default: compile test

compile:
	ghc ${OPTS} ${SRC} -o ${TEST} -package base

test:
	./${TEST}

clean:
	rm -f *.hi *.o ${TEST}
