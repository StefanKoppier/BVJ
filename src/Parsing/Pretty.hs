module Parsing.Pretty where

import Text.PrettyPrint
import Auxiliary.Phase
import Auxiliary.Pretty
import Parsing.Syntax

instance Pretty Type' where
    pretty (PrimType' ty) = pretty ty
    pretty (RefType' ty)  = pretty ty

instance Pretty PrimType' where
    pretty BooleanT' = text "bool"
    pretty ByteT'    = text "byte"
    pretty ShortT'   = text "short"
    pretty IntT'     = text "int"
    pretty LongT'    = text "long"
    pretty CharT'    = text "char"
    pretty FloatT'   = text "float"
    pretty DoubleT'  = text "double"

instance Pretty RefType' where
    pretty (ArrayType' ty) = pretty ty <> brackets empty

instance Pretty CompoundStmt' where
    pretty (Seq' s1 s2)              = pretty s1 $+$ pretty s2
    pretty (Block' s)                = lbrace $+$ nest 4 (pretty s) $+$ rbrace
    pretty (IfThenElse' g s1 s2)     = text "if" <+> parens (pretty g) $+$ pretty s1 $+$ text "else" <+> pretty s2
    pretty (While' (Just ident) g s) = text ident <> text ": while" <+> parens (pretty g) <+> nest 4 (pretty s)
    pretty (While' Nothing g s)      = text "while" <+> parens (pretty g) <+> nest 4 (pretty s)
    pretty (Stmt' s)                 = pretty s

instance Pretty Stmt' where
    pretty (Decl' ms ty vars)       = pretty ms <+> pretty ty <+> pretty vars <> semi
    pretty Empty'                   = semi
    pretty (ExpStmt' exp)           = pretty exp <> semi
    pretty (Assert' exp (Just err)) = text "assert" <+> pretty exp <+> colon <+> pretty err <> semi
    pretty (Assert' exp Nothing)    = text "assert" <+> pretty exp <> semi
    pretty (Assume' exp)            = text "assume" <+> pretty exp <> semi
    pretty (Break' (Just ident))    = text "break:" <+> text ident <> semi
    pretty (Break' Nothing)         = text "break" <> semi
    pretty (Continue' (Just ident)) = text "continue:" <+> text ident <> semi
    pretty (Continue' Nothing)      = text "continue" <> semi
    pretty (Return' (Just exp))     = text "return" <+> pretty exp <> semi
    pretty (Return' Nothing)        = text "return" <> semi

instance Pretty [VarDecl'] where
    pretty = hcat . punctuate (comma <> space) . map pretty

instance Pretty VarDecl' where
    pretty (VarDecl' id (InitArray' Nothing)) = pretty id
    pretty (VarDecl' id init)                 = pretty id <> equals <> pretty init
    
instance Pretty VarDeclId' where
    pretty (VarId' id) = text id

instance Pretty VarInit' where
    pretty (InitExp' exp)         = pretty exp
    pretty (InitArray' (Just es)) = braces $ (hcat . punctuate comma . map pretty) es
    pretty (InitArray' Nothing)   = empty
    
instance Pretty Exp' where
    pretty (Lit' x)               = pretty x
    pretty (ArrayCreate' ty ss n) = text "new" <+> pretty ty <> (hcat . map (brackets . pretty)) ss <> hcat (replicate n (brackets empty))
    pretty (ArrayAccess' n es)    = text n <> (hcat . map (brackets . pretty)) es
    pretty (ExpName' n)           = pretty n
    pretty (PostIncrement' e)     = pretty e <> text "++"
    pretty (PostDecrement' e)     = pretty e <> text "--"
    pretty (PreIncrement' e)      = text "++" <> pretty e
    pretty (PreDecrement' e)      = text "--" <> pretty e
    pretty (PrePlus' e)           = char '+' <> pretty e
    pretty (PreMinus' e)          = char '-' <> pretty e
    pretty (PreBitCompl' e)       = char '~' <> pretty e
    pretty (PreNot' e)            = char '!' <> pretty e
    pretty (BinOp' e1 op e2)      = pretty e1 <> pretty op <> pretty e2
    pretty (Cond' g e1 e2)        = pretty g <> char '?' <> pretty e1 <> char ':' <> pretty e2
    pretty (Assign' t op e)       = pretty t <> pretty op <> pretty e

instance Pretty Literal' where
    pretty (Int' x)         = text $ show x
    pretty (Float' x)       = float x
    pretty (Double' x)      = float x
    pretty (Boolean' True)  = text "true"
    pretty (Boolean' False) = text "false"
    pretty (Char' x)        = quotes $ char x
    pretty (String' x)      = doubleQuotes $ text x 
    pretty Null'            = text "null"

instance Pretty Op' where
    pretty Mult'    = text "*"
    pretty Div'     = text "/"
    pretty Rem'     = text "%"
    pretty Add'     = text "+"
    pretty Sub'     = text "-"
    pretty LShift'  = text "<<"
    pretty RShift'  = text ">>"
    pretty RRShift' = text ">>>"
    pretty LThan'   = text "<"
    pretty GThan'   = text ">"
    pretty LThanE'  = text "<="
    pretty GThanE'  = text ">="
    pretty Equal'   = text "=="
    pretty NotEq'   = text "!="
    pretty And'     = text "&"
    pretty Or'      = text "|"
    pretty Xor'     = text "^"
    pretty CAnd'    = text "&&"
    pretty COr'     = text "||"

instance Pretty AssignOp' where
    pretty EqualA'   = equals
    pretty MultA'    = text "*="
    pretty DivA'     = text "/="
    pretty RemA'     = text "%="
    pretty AddA'     = text "+="
    pretty SubA'     = text "-="
    pretty LShiftA'  = text "<<="
    pretty RShiftA'  = text ">>="
    pretty RRShiftA' = text ">>>="
    pretty AndA'     = text "&="
    pretty XorA'     = text "^="
    pretty OrA'      = text "|="

instance Pretty Lhs' where
    pretty (Name' name) = pretty name

instance Pretty [Modifier'] where
    pretty = hcat . punctuate space . map pretty

instance Pretty Modifier' where
    pretty Static' = text "static"

instance Pretty Name' where
    pretty = hcat . punctuate (comma <> space) . map text