module Compilation.Compiler where

import System.Directory
import System.FilePath.Posix
import System.Command
import System.Exit
import System.IO
import Data.List
import Data.Function         (on)
import Auxiliary.Phase
import Parsing.Syntax
import Parsing.Utility
import Parsing.Fold
import Auxiliary.Pretty
import Linearization.Path
import Compilation.CompiledUnit
import Compilation.Compiler.Class
import Compilation.Compiler.Method

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
    let classGroups = (groupBy ((==) `on` className) . sortOn (snd . snd)) path
    classes <- mapM (buildClass unit) classGroups
    let imports = defaultImports ++ originalImports
    return $ CompilationUnit' package imports classes
    where
        defaultImports = [ImportDecl' False ["org", "cprover"] True]

--------------------------------------------------------------------------------
-- Java compiler and jar calls
--------------------------------------------------------------------------------

javac :: FilePath -> PhaseResult ()
javac file = do
    (Stdout _, Exit e, Stderr _) <- liftIO $ command [] "javac" args
    case e of
        ExitSuccess   -> return ()
        ExitFailure _ -> throwExternalError "java compilation to bytecode (javac) failed"
    where
        dir  = dropFileName file
        args = [ file
               , "-d", dir
               , "-classpath", "./lib/core-models.jar"
               ]

jar :: FilePath -> PhaseResult ()
jar file = do
    (Stdout _, Exit e, Stderr _) <- liftIO $ command [Cwd dir] "jar" args
    case e of
        ExitSuccess   -> return ()
        ExitFailure _ -> throwExternalError "jar creation (jar) failed."
    where
        dir = dropFileName file
        args = [ "cfe"
               ,  takeFileName file
               , "-C", "./"
               ]