repl:											\
	dist/build/autogen/uuagc/Analysis/Syntax.hs	\
	dist/build/autogen/uuagc/Analysis/CFA.hs
	cabal repl app

clean:
	cabal clean

# uuagc:
#  -H              --haskellsyntax                 Use Haskell like syntax (equivalent to --lckeywords and --doublecolons --genlinepragmas)
#  -d              --data                          generate data type definition
#  -c              --catas                         generate catamorphisms
#  -f              --semfuns                       generate semantic functions
#  -w              --wrappers                      generate wappers for semantic domains
#  -s              --signatures                    generate signatures for semantic functions

dist/build/autogen/uuagc/Analysis/CFA.hs:
	mkdir -p `dirname $@`					
	uuagc -Hcfws --self --module=Analysis.CFA src/Analysis/CFA.ag --output=$@
	
dist/build/autogen/uuagc/Analysis/Syntax.hs:	
	mkdir -p `dirname $@`					
	uuagc -Hd --module=Analysis.Syntax src/Analysis/Syntax.ag --output=$@