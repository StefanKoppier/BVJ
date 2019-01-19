default: all

clean: 								
	rm -f 							\
		src/Analysis/Complete.hs

all:						\
	src/Analysis/Complete.hs	

app: 				\
	clean			\
	all				
	cabal repl app
	
# uuagc:
#  -H              --haskellsyntax                 Use Haskell like syntax (equivalent to --lckeywords and --doublecolons --genlinepragmas)
#  -d              --data                          generate data type definition
#  -c              --catas                         generate catamorphisms
#  -f              --semfuns                       generate semantic functions
#  -w              --wrappers                      generate wappers for semantic domains
#  -s              --signatures                    generate signatures for semantic functions

src/Analysis/Complete.hs:
	cd src/Analysis && uuagc -Hdcfws --self Complete.ag

.PHONY: default all clean