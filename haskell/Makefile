
# TODO
# if flag, add profile option
# ------------------------------------------------------------

EXE_FILE =  ./dist/build/playHangmanGame/playHangmanGame
PROF_RUN_OPT = +RTS -p -RTS
PROF_BUILD_OPT = --enable-executable-profiling -fprof

OPTIONS = 
RUN_OPTITONS = 

config:
	cabal configure $(OPTIONS)

build: config
	cabal build

dist: config
	cabal sdist

run: build
	$(EXE_FILE) $(RUN_OPTITONS)

doc: config
	cabal haddock --executables

clean:
	cabal clean

all: clean build doc dist

.PHONY: build

# ------------------------------------------------------------
