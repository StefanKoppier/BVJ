module Main where
    
import Complete
 
--------------------------------------------------------------------------------
-- Verification interface.
--------------------------------------------------------------------------------

-- | The (default) arguments to be used in the verification.
arguments :: Arguments
arguments = defaultArgs {
      verbosity       = Informative {-Compact-}
    , numberOfThreads = 4
    , keepOutputFiles = True
    , maximumDepth    = 10
    , pathFilter      = const id
    }

-- | Verify the given source file with a maximum depth.
verifyWithMaximumDepth :: Int -> FilePath -> IO ()
verifyWithMaximumDepth depth = run arguments{maximumDepth=depth}

-- | Verify the given source file.
verify :: FilePath -> IO ()
verify = run arguments

--------------------------------------------------------------------------------
-- Main program.
--------------------------------------------------------------------------------

main :: IO ()
main = run arguments "examples/Test.java"