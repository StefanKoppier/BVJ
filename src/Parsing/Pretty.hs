module Parsing.Pretty where

import Text.PrettyPrint
import Auxiliary.Phase
import Auxiliary.Pretty
import Parsing.Syntax

--------------------------------------------------------------------------------
-- Files
--------------------------------------------------------------------------------

instance Pretty CompilationUnit' where
    pretty (CompilationUnit' package decls)
        = package' $+$ pretty decls
        where
            package' = maybe empty (\ name -> text "package" <+> dots name <> semi) package

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

instance Pretty TypeDecls' where
    pretty = foldr (($+$) . pretty) empty

instance Pretty TypeDecl' where
    pretty (ClassTypeDecl' decl) = pretty decl

instance Pretty ClassDecl' where
    pretty (ClassDecl' ms n body) = header' $+$ body'
        where
            header' = pretty ms <+> text "class" <+> text n
            body'   = braces (pretty body)

instance Pretty Decls' where
    pretty = foldr (($+$) . pretty) empty

instance Pretty Decl' where
    pretty (MemberDecl' decl) = pretty decl

instance Pretty MemberDecl' where
    pretty (FieldDecl' ms ty var) = pretty ms <+> pretty ty <+> pretty var

    pretty (MethodDecl' ms ty n ps b)   
        = header' $+$ body'
        where
            header' = pretty ms <+> pretty ty <+> pretty n <> parens (pretty ps)
            body'   = braces (pretty b)

    pretty (ConstructorDecl' ms n ps b) 
        = header' $+$ (braces . newlines . pretty) b
        where
            header' = pretty ms <+> pretty n <> parens (pretty ps)

instance Pretty [FormalParam'] where
    pretty = commas

instance Pretty FormalParam' where
    pretty (FormalParam' ms ty id) = pretty ms <+> pretty ty <+> pretty id

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

instance Pretty (Maybe Type') where
    pretty (Just ty) = pretty ty
    pretty Nothing   = text "void"

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

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

instance Pretty CompoundStmt' where
    pretty (Seq' s1 s2)              = pretty s1 $+$ pretty s2
    pretty (Block' s)                = lbrace $+$ nest 4 (pretty s) $+$ rbrace
    pretty (IfThenElse' g s1 s2)     = text "if" <+> parens (pretty g) $+$ pretty s1 $+$ text "else" <+> pretty s2
    pretty (While' (Just ident) g s) = text ident <> text ": while" <+> parens (pretty g) <+> nest 4 (pretty s)
    pretty (While' Nothing g s)      = text "while" <+> parens (pretty g) <+> nest 4 (pretty s)
    pretty (Switch' e cs)            = text "switch" <+> parens (pretty e) $+$ braces (nest 4 (pretty cs))
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
    pretty (ReturnExp' exp)         = text "return" <+> pretty exp <> semi
    pretty Return'                  = text "return" <> semi

instance Pretty SwitchBlocks' where
    pretty = foldr (($+$) . pretty) empty

instance Pretty SwitchBlock' where
    pretty (SwitchBlock' (Just e) stat) = text "case" <+> pretty e <> colon $+$ nest 4 (pretty stat)
    pretty (SwitchBlock' Nothing stat)  = text "default:" $+$ nest 4 (pretty stat)

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

instance Pretty [VarDecl'] where
    pretty = hcat . punctuate (comma <> space) . map pretty

instance Pretty VarDecl' where
    pretty (VarDecl' id (InitArray' Nothing)) = pretty id
    pretty (VarDecl' id init)                 = pretty id <> equals <> pretty init
    
instance Pretty VarDeclId' where
    pretty (VarId' id) = text id

instance Pretty VarInit' where
    pretty (InitExp' exp)         = pretty exp
    pretty (InitArray' (Just es)) = braces $ commas es
    pretty (InitArray' Nothing)   = empty

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

instance Pretty Exp' where
    pretty (Lit' x)               = pretty x
    pretty (ArrayCreate' ty ss n) = text "new" <+> pretty ty <> (hcat . map (brackets . pretty)) ss <> hcat (replicate n (brackets empty))
    pretty (MethodInv' inv)       = pretty inv
    pretty (ArrayAccess' n es)    = text n <> (hcat . map (brackets . pretty)) es
    pretty (ExpName' n)           = (hcat . punctuate dot . map text) n
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
    pretty (Name' name) = dots name

instance Pretty MethodInvocation' where
    pretty (MethodCall' name args) = name' <> parens args'
        where
            name' = dots name
            args' = commas args

--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

instance Pretty [Modifier'] where
    pretty = hcat . punctuate space . map pretty

instance Pretty Modifier' where
    pretty Public'    = text "public"
    pretty Private'   = text "private"
    pretty Protected' = text "protected"
    pretty Final'     = text "final"
    pretty Static'    = text "static"
