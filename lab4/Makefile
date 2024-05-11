OUT_REV	= reverse
OUT_COL	= columns
OUT_SUD	= sudoku
PKGB	= -package base -package containers
PKGC	= -package base -package containers -package array
OPTS	= -W

all: reverse columns #sudoku

reverse:
	ghc ${OPTS} -o ${OUT_REV} Reverse.hs ${PKGB}

columns:
	ghc ${OPTS} -o ${OUT_COL} Columns.hs ${PKGB}

sudoku:
	ghc ${OPTS} -o ${OUT_SUD} Sudoku.hs ${PKGC} 

clean:
	rm -f *.hi *.o *dyn_o *dyn_hi ${OUT_REV} ${OUT_COL} ${OUT_SUD}
