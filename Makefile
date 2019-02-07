repl: \
	dist/build/autogen/uuagc/Analysis/Complete.hs
	cabal repl app

clean:
	cabal clean

AG		  = uuagc
AG_OPTS	  = -Hdcfws --self
AG_OUT_DIR= dist/build/autogen/uuagc

# uuagc:
#  -H              --haskellsyntax                 Use Haskell like syntax (equivalent to --lckeywords and --doublecolons --genlinepragmas)
#  -d              --data                          generate data type definition
#  -c              --catas                         generate catamorphisms
#  -f              --semfuns                       generate semantic functions
#  -w              --wrappers                      generate wappers for semantic domains
#  -s              --signatures                    generate signatures for semantic functions

dist/build/autogen/uuagc/Analysis/Complete.hs:	
	mkdir -p `dirname $@`					
	$(AG) $(AG_OPTS) src/Analysis/Complete.ag --output=$@