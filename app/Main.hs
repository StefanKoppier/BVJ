module Main where

import SimpleGetOpt
import Text.Read
import Complete
 
--------------------------------------------------------------------------------
-- Verification interface.
--------------------------------------------------------------------------------

-- | The (default) arguments to be used in the verification.
arguments :: Arguments
arguments = Arguments {
      verbosity            = Informative
    , function             = Nothing  
    , numberOfThreads      = 4
    , removeOutputFiles    = False
    , maximumDepth         = 10
    , pathFilter           = Just
    , jbmcEnableAssertions = True
    , jbmcDepth            = Nothing
    , jbmcUnwind           = Nothing
    }

-- | Verify the given source file with a maximum depth.
verifyWithMaximumDepth :: Int -> FilePath -> IO ()
verifyWithMaximumDepth depth file 
    = run arguments{maximumDepth=depth, program=file}

-- | Verify the given source file with a maximum depth and function to verify.
verifyFunctionWithMaximumDepth :: Int -> String -> FilePath -> IO ()
verifyFunctionWithMaximumDepth depth func file
    = run arguments{maximumDepth=depth, program=file, function=Just func}

-- | Verify the given source file.
verify :: FilePath -> IO ()
verify file = run arguments{program=file}

--------------------------------------------------------------------------------
-- Main program and argument parsing.
--------------------------------------------------------------------------------

options :: OptSpec Arguments
options = OptSpec {
      progDefaults = arguments

    , progOptions = [
        -- Verbosity argument.
          Option ['c'] ["compact"]
          "Display less information."
          $ NoArg $ \ a -> Right a { verbosity = Compact }

        -- Remove the output file.
        , Option ['r'] ["remove"]
          "Remove the output files."
          $ NoArg $ \ a -> Right a { removeOutputFiles = True }

        -- Verify a different function than main.
        , Option ['f'] ["function"]
          "Verify the function."
          $ OptArg "FUNCTION" $ \ f a
            -> case f of
                Just f' -> Right a { function = Just f' }
                Nothing -> Right a

        -- Maximum program path generation depth. 
        , Option ['k'] ["depth"]
          "Maximum program path generation depth."
          $ OptArg "DEPTH" $ \ v a 
            -> case v of
                Just v' -> case readMaybe v' of
                                Just k | k > 0 -> Right a { maximumDepth = k }
                                _              -> Left "negative value for `--depth`."
                Nothing -> Right a
            
        -- Number of compilation and verification threads.
        , Option ['t'] ["threads"]
          "Number of threads."
          $ OptArg "THREADS" $ \ v a 
            -> case v of
                Just v' -> case readMaybe v' of
                                Just t | t > 0 -> Right a { numberOfThreads = t }
                                _              -> Left "negative value for `--threads`."
                Nothing -> Right a

        -- Maximum loop unwinding (in JBMC).
        , Option ['u'] ["unwind"]
          "Maximum loop unwinding in JBMC."
          $ OptArg "UNWIND" $ \ v a
            -> case v of
                Just v' -> case readMaybe v' of
                                Just u | u > 0 -> Right a { jbmcUnwind = Just u }
                                _              -> Left "negative value for `--unwind`."
                Nothing -> Right a

        -- Maximum verification depth (in JBMC).
        , Option [] ["verification-depth"]
          "Maximum depth in JBMC."
          $ OptArg "VER-DEPTH" $ \ v a
            -> case v of
                Just v' -> case readMaybe v' of
                                Just d | d > 0 -> Right a { jbmcDepth = Just d }
                                _              -> Left "negative value for `--verification-depth`."
                Nothing -> Right a
    ]
    
    , progParamDocs = [
        ( "File", "Path to the file to be verified.")
    ]

    , progParams = \ p s -> Right s { program = p }
}

-- | Main program which parses the arguments and runs the verification.
main :: IO ()
main = do
    opts <- getOpts options
    run opts