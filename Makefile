repl:																		\
	dist/build/autogen/uuagc/Parsing/Syntax.hs								\
	dist/build/autogen/uuagc/Compilation/Compiler/InformationGathering.hs	\
	dist/build/autogen/uuagc/Analysis/CFA.hs	
	cabal repl app

clean:
	cabal clean

cleanag:
	rm -r dist/build/autogen/uuagc/

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
		
dist/build/autogen/uuagc/Compilation/Compiler/InformationGathering.hs:	
	mkdir -p `dirname $@`					
	uuagc -Hcfws --self --module=Compilation.Compiler.InformationGathering src/Compilation/Compiler/InformationGathering.ag --output=$@

dist/build/autogen/uuagc/Parsing/Syntax.hs:	
	mkdir -p `dirname $@`					
	uuagc -Hd --module=Parsing.Syntax src/Parsing/Syntax.ag --output=$@