{-|
Module      : Compilation.Compiler
Description : Module containing the top level compilation functionality.
-}
module Compilation.Compiler where

import System.Directory
import System.FilePath.Posix
import System.Command
import System.Exit
import Data.List
import Data.Function         (on)
import Auxiliary.Phase
import Auxiliary.Pretty
import Compilation.CompiledUnit
import Compilation.Compiler.Class
import Compilation.Compiler.Method

-- | Compiles the given program path and returns the compiled unit, which can be
-- filtered.
compile :: Arguments -> ProgressBar -> CompilationUnit' -> FilePath -> Int -> ProgramPath -> IO (Either PhaseError CompiledUnit)
compile Arguments{pathFilter} progress unit dir identifer path = do
    program <- runExceptT $ build unit path
    case program of
        Left  fProgram -> return $ Left fProgram
        Right sProgram ->
            case pathFilter sProgram of
                Just program' -> do
                    file <- runExceptT $ create progress program' dir identifer
                    case file of
                        Left  fFile -> return $ Left fFile
                        Right sFile -> return $ Right (CompiledUnit program' sFile)
                Nothing ->
                    return $ Right FilteredUnit

-- | Compiles and packages the given compilation unit and increments the progress
-- bar. Returns the path to the jar file.
create :: ProgressBar -> CompilationUnit' -> FilePath -> Int -> PhaseResult FilePath
create progress program dir identifier = do
    let actualDir   = dir ++ "/" ++ show identifier
    let programPath = actualDir ++ "/Program.java"
    let jarPath     = actualDir ++ "/Program.jar"
    liftIO $ createDirectory (dropFileName programPath)
    liftIO $ writeFile programPath (toString program)
    javacResult <- liftIO $ javac programPath
    case javacResult of
        Left  javacFailure -> throwE javacFailure
        Right _            -> do
            jarResult <- liftIO $ jar jarPath
            case jarResult of
                Left  jarFailure -> throwE jarFailure
                Right _          -> do
                    liftIO $ incProgress progress
                    return jarPath

-- | Transforms the program path to the compilation unit.
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

-- | Runs the command `javac`.
javac :: FilePath -> IO (Either PhaseError ())
javac file = do
    (Stdout _, Exit e, Stderr _) <- command [] "javac" args
    case e of
        ExitSuccess   -> return $ Right ()
        ExitFailure _ -> return $ Left (ExternalError "java compilation to bytecode (javac) failed")
    where
        dir  = dropFileName file
        args = [ file
               , "-d", dir
               , "-classpath", "./lib/core-models.jar"
               ]

-- | Runs the command `jar`.
jar :: FilePath -> IO (Either PhaseError ())
jar file = do
    (Stdout _, Exit e, Stderr _) <- command [Cwd dir] "jar" args
    case e of
        ExitSuccess   -> return $ Right ()
        ExitFailure _ -> return $ Left (ExternalError "jar creation (jar) failed.")
    where
        dir  = dropFileName file
        args = [ "cfe"
               ,  takeFileName file
               , "-C", "./"
               ]