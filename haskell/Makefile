
# TODO
# if flag, add profile option
# ------------------------------------------------------------

EXE_FILE =  ./dist/build/playHangmanGame/playHangmanGame

PROF_RUN_OPT = +RTS -p -RTS
PROF_BUILD_OPT = --enable-executable-profiling -fprof

cabal-config = cabal configure
cabal-build  = cabal build

config:
	$(cabal-config)

build:
	$(cabal-build)

rebuild: config
	$(cabal-build)

dist: config
	cabal sdist

run: rebuild
	$(EXE_FILE)

doc: config
	cabal haddock --executables

prof:
	$(cabal-config) $(PROF_BUILD_OPT) && $(cabal-build)
	$(EXE_FILE) $(PROF_RUN_OPT)
clean:
	cabal clean

all: clean build doc dist

.PHONY: build

# ------------------------------------------------------------
