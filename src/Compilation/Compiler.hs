module Compilation.Compiler where

import System.Directory
import System.FilePath.Posix
import System.Command
import System.IO
import Data.List
import Data.Maybe
import Data.Function         (on)
import Auxiliary.Phase
import Parsing.Syntax
import Parsing.Utility
import Auxiliary.Pretty
import Linearization.Path
import Compilation.CompiledUnit

import Debug.Trace

compile :: ProgressBar -> CompilationUnit' -> FilePath -> Int -> ProgramPath -> PhaseResult CompiledUnit
compile progress unit dir id path = do
    program <- build unit path
    file    <- create progress program dir id
    return (program, file)

create :: ProgressBar -> CompilationUnit' -> FilePath -> Int -> PhaseResult FilePath
create progress program dir id = do
    let actualDir   = dir ++ "/" ++ show id
    let programPath = actualDir ++ "/Program.java"
    let jarPath     = actualDir ++ "/Program.jar"
    liftIO $ createDirectory (dropFileName programPath)
    liftIO $ writeFile programPath (toString program)
    javac programPath
    jar jarPath
    liftIO $ incProgress progress
    return jarPath

build :: CompilationUnit' -> ProgramPath -> PhaseResult CompilationUnit'
build unit@(CompilationUnit' package originalImports _) path = do
    let classGroups = (groupBy ((==) `on` className) . sortOn (callName . snd)) path
    classes <- mapM (buildClass unit) classGroups
    let imports = defaultImports ++ originalImports
    return $ CompilationUnit' package imports classes
    where
        defaultImports = [ImportDecl' False ["org", "cprover"] True]

buildClass :: CompilationUnit' -> [PathStmt] -> PhaseResult TypeDecl'
buildClass unit path = do
    let methodGroups = (groupBy ((==) `on` (callName . snd)) . sortOn (callName . snd)) path
    methods <- mapM (buildMethod unit) methodGroups
    return (ClassTypeDecl' (ClassDecl' modifiers name methods))
    where
        name                       = className (head path)
        (ClassDecl' modifiers _ _) = fromJust $ findClass name unit

buildConstructor :: CompilationUnit' -> [PathStmt] -> PhaseResult Decl'
buildConstructor = undefined

buildMethod :: CompilationUnit' -> [PathStmt] -> PhaseResult Decl'
buildMethod unit path = do
    let name = callName . snd . head $ path
    let body = map (Stmt' . fst) path
    return (MemberDecl' (MethodDecl' modifiers ty name params body))
    where
        scope                                 = (origin . snd . head) path
        (MethodDecl' modifiers ty _ params _) = fromJust $ getMethod unit scope

className :: PathStmt -> String
className (_,PathStmtInfo{origin})
    = c
    where
        (Scope _ c _) = origin
  
methodName :: PathStmt -> String
methodName (_,PathStmtInfo{origin})
    = m
    where
        (Scope _ _ m) = origin

--------------------------------------------------------------------------------
-- Java compiler and jar calls
--------------------------------------------------------------------------------

javac :: FilePath -> PhaseResult ()
javac file = do
    (Stdout _, Exit _, Stderr _) <- liftIO $ command [] "javac" args
    return ()
    where
        dir  = dropFileName file
        args = [ file
               , "-d", dir
               , "-classpath", "./lib/core-models.jar"
               ]

jar :: FilePath -> PhaseResult ()
jar file = liftIO $ command_ [Cwd dir] "jar" args
    where
        dir = dropFileName file
        args = [ "cfe"
               ,  takeFileName file
               , "-C", "./"
               ]