# BVJ

BVJ is a Bounded Model Checking tool for Java source code, supporting a subset
of the Java language. The tool uses [JBMC](https://github.com/diffblue/cbmc) as
its verification back-end.

## Requirements
The tool is tested using GHC 8.2.2 and JBMC 5.11. To install the tool and its requirements:

1. Download and compile JBMC, ensure that jbmc can be found in the PATH variable. The manual for compilation of JBMC can be found [here](https://github.com/diffblue/cbmc/blob/develop/COMPILING.md).
2. Install the required libraries used in the tool:
    * [`fgl 5.7.0.0`](http://hackage.haskell.org/package/fgl-5.7.0.0)
    * [`directory 1.3.0.2`](http://hackage.haskell.org/package/directory-1.3.0.2)
    * [`terminal-progress-bar`](hackage.haskell.org/package/terminal-progress-bar)
    * [`command`](http://hackage.haskell.org/package/command)
    * [`filepath`](http://hackage.haskell.org/package/filepath)
    * [`dates`](http://hackage.haskell.org/package/dates)
    * [`pretty`](http://hackage.haskell.org/package/pretty)
    * [`xeno`](http://hackage.haskell.org/package/xeno)
    * [`utf8-string`](http://hackage.haskell.org/package/utf8-string)
    * [`language-java`](http://hackage.haskell.org/package/language-java)
    * [`transformers`](http://hackage.haskell.org/package/transformers)
    * [`containers`](http://hackage.haskell.org/package/containers)
    * [`parallel-io`](http://hackage.haskell.org/package/parallel-io)

## Usage

The tool can be used in two ways: using GHCi, and by execution of the executable.

### Using GHCi

When using GHCi, the function `verify` and `verifyWithMaximumDepth`, defined in  [Main.hs](app/Main.hs), can be used. The `arguments` constant can be used to directly tweak the verification tool.

### Running the executable

When compiling and running the executable, the following parameters can be set to tweak the verification tool:

```
Parameters:
  File    Path to the file to be verified.

Flags:
  -c           --compact                         Display less information.
  -r           --remove                          Remove the output files.
  -k[DEPTH]    --depth[=DEPTH]                   Maximum program path generation depth.
  -t[THREADS]  --threads[=THREADS]               Number of threads.
  -u[UNWIND]   --unwind[=UNWIND]                 Maximum loop unwinding in JBMC.
               --verification-depth[=VER-DEPTH]  Maximum depth in JBMC.
```

For example, the command `bvj "examples/Test.java" -c -k10 -t4` verifies the file `examples/Test.java` with minimal information, with a maximum program path depth of 10, and uses 4 threads.