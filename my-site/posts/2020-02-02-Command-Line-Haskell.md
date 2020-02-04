---
title: stack script: Compile and run Haskell with a single file
header: A report on stack script: the how and why...
date: February 2, 2020
---

## Introduction
#### Why stack script ?
If you share small, single module, self contained haskell examples, stack script gives us an easy way to get reproducible builds, by pinning the dependencies to a Stackage snapshot within a comment at the top of your Haskell code.

#### Intoduction 
This post is about building Haskell source code within a single file using The Haskell Tool Stack's scripting interpreter, which can compile and run single file Haskell codebases, or "scripts".
Stack is a build tool primarily designed for reproducible builds, done by specifying a resolver in a configuration file, usually your projects `stack.yaml` and `package.yaml`
With Stack's scripting feature, we still get reproducible builds by specifying a resolver, but move this specification to the file we are compiling, or as a command line argument. 
Therefore, for the sake of simplicity, we'll assume that these scripts are run outside of a stack project, and stack is invoked in the same directory as the script file.    
When running a stack script inside of a stack project, it's important to consider that stack will read settings from your `project.yaml` and `stack.yaml`, which may cause issues.    

There are at least two additional motivations, besides reproducible builds, that you might want to use Stack's scripting feature:
 - Lower the configuration barrier: write an independently compiling Haskell source code file with package dependencies without having to configure a new stack or cabal project. Personally, I find this helpful when exploring new libraries or writing small programs. 
 - Using Haskell as a scripting language, or replacement for Shell/Bash/Zsh. This use case pairs well with the `Turtle` library, although this approach does have downsides. 

This article contains the following examples of using scripting with stack:
 - A basic example
 - A simple Servant server that statically serves your current working directory
 - An example of a bash install script, and Haskell replacement that can be run as a cron job. 
 - Using stack script to launch ghci

## Stack Scripting Interpreter
#### Intro
For our first example, we'll use stack to run a single file of Haskell source code as a script.

Here's the source code we want to run, in a filed called `simple.hs`:
```
main :: IO ()
main = putStrLn "compiled & run"
```
To run this with the stack script interpreter, we can do the following:
```
$ stack script simple.hs --resolver lts-14.18
```
The resolver argument is mandatory, and Stack will compile and run the `simple.hs` file immediately after invocation using the `lts-14.18` Stackage snapshot.      
Alternatively, we can put all of the configuration information into the script itself, like this: 
```
{- stack script 
 --resolver lts-14.18
-}
main :: IO ()
main = putStrLn "compiled & run"
```
which can be compiled and run with `$ stack simple.hs`. 


#### Building with packages
The "killer feature" for scripting with stack is probably the ability to pull in packages without having to a `stack.yaml` or  
This can probably be best seen with `stack ghci`, where the following command will drop you into a ghci repl where you have `lens` and `text` packages available from the specificied resolver. 
```
stack ghci --package text --package lens --resolver lts-14.18
```
An example of this concept with the stack scripting engine, is a quick and dirty file server, `explore.hs` would be as follows:
```
~/projects/stack-script$ cat explore.hs
#!/usr/bin/env stack
{- stack script
 --resolver nightly-2019-12-22
 --install-ghc
 --package "servant-server warp"
 --ghc-options -Wall
-}
{-# LANGUAGE DataKinds, TypeOperators, TypeApplications #-}

module FileServer where

import Network.Wai.Handler.Warp( defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant (Proxy(Proxy), Raw, serve, serveDirectoryWebApp)

main :: IO ()
main = runSettings settings . serve (Proxy @Raw) $ serveDirectoryWebApp "."
  where port = 8080
        msg = "serving on http://localhost:" ++ show port ++ "/{pathToFile}"
        settings = setPort port $ setBeforeMainLoop (putStrLn msg) defaultSettings
```

Noting a couple of features
 - `--install-ghc` is the flag to install ghc, if it is not already available.
 - The addition of the hash bang, (line 1), `#!/usr/bin/env stack`, let's you run this as an executable, `$ ./explore.hs`
 - If running, this script will let you see it's source code at `localhost:8080/static/explore.hs`, along with any other files within the current working directory the script was run.
 - The snapshot here is a nightly from the day the script was written, [nightly-2019-12-22](https://www.stackage.org/nightly-2019-12-22), which ensures the most up to date version of libraries are used when the script is written while still pinning us to a specific snapshot.
 - We pass in `-Wall` to ghc-options, and can give additional ghc options here.

On a fresh compilation, this will take a few minutes to run, as Stack needs to go and grab about 255Mb worth of source code in ~86 dependent packages, compile and link it in order for the above code to run.
However, on subsequent runs, Stack can use a local cache of of the packages, and we can reproduce our project build without downloading and building all the dependencies! 


#### Stack Script as a Bash Replacement
It's possible to use haskell, and Stack scripting feature, along with the Turtle library as a drop in replacement for sshell scripting!     
To do this, we need the following at the top of our Haskell file:    
```
#!/usr/bin/env stack
{- stack script
 --compile
 --copy-bins
 --resolver lts-14.17
 --install-ghc
 --package "turtle text foldl async"
 --ghc-options=-Wall
-}
```
This stack script does a couple of things
 - `--compile` and `--copy-bins` create a binary executable based on the filename.    
 - installs ghc, if needed with `install-ghc`    
 - builds the scripts with the set of packages from `lts-14.17`    

With [tutle](https://hackage.haskell.org/package/turtle), we get a portable way to to run external shell commands, 
and I was able to create a nice haskell program to replace the shell script I used to automate the server tasks needed to deploy this blog!     
The basics my deploy turtle script are as follows, and you can see the [full example on github here](https://gist.github.com/adamwespiser/25b0af28529a6de1272af6af6275f2a4#file-updatesite-hs)    
```
import qualified Turtle as Tu
import qualified Control.Foldl as L
import qualified Data.Text as T
import Control.Concurrent.Async
import System.IO

argParser :: Tu.Parser Tu.FilePath
argParser = Tu.argPath "html" "html destination directory"

main :: IO ()
main = do
  -- 53 files copied over into destinationDir
  hSetBuffering stdout NoBuffering
  destinationDir <- Tu.options "Build blog and copy to directory" argParser
  Tu.with (Tu.mktempdir "/tmp" "deploy") (mainLoop destinationDir)
```
One nice thing about turtle is the `Tu.with` function, which lets use run our the main logic of our program with a tmp directory which is subsequently cleaned up after the `mainLoop` function returns.    
Despite turtle being a handy library, I did find some downsides
 - Use of `FilePath`, which uses a pretty clunky, `String` based file representation
 - Often times clunkier semantics than just writing bash: for instance, `cp -r SRC TRG` is requires a fold over the result of `ls SRC` and construction of an explicit `cp`  with each file, instead, you need to use `cptree`, which took me a while to figure out, so it would be nice if the semantics matched better!
 - Turtle is a monolithic framework for interacting with OS through a set of mirroed shell commands trying to match `coreutiles`, and it's tighlty couple parts makes it not very easy to pick the parts you like, and disregard the rest!

#### Stack script w/ ghci
We've already seen a few examples of stack script, but there is one more that should be in every Haskeller's toolkit.
Stack script can be used to launch a ghci repl.
Let's say we are working with a new ADT, and want to write a new QuickCheck instance, how can stack script help us?    
The following header will load the listed packages into a ghci repl:
```
{- stack 
 --resolver nightly
 --install-ghc
 exec ghci
 --package "QuickCheck checkers"
moduble XTest where
-}
```
There is one note to make here about the order of the arguments: 
 - The file will compile, then drop you into with module `XTest` is loaded
 - If `exec ghci` does not imeadiately follow `stack`, then the `--packages` must be before `exec ghci`

## Conclusion
I often find myself coding up small Haskell snippets, whether it's playing around with a new ADT, trying out a library, or reproducing an example from a paper or a book. 
In these cases, Stack' scripting feature shines at giving me a self contained file where I can specify the dependencies via a snapshot in the file header, and not have to worry about breaking changes, or setting up a project with all the correct dependencies. 
Thus, I would urge my fellow Haskellers to consider using stack's scripting feature when they share code online, to help others run their code today, and keep in runnable far into the future! 

## Additional Information
 - [Stack Docs: Script Interpreter](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter)    
 - [FPComplete: How to Script with Stack](https://tech.fpcomplete.com/haskell/tutorial/stack-script)
 - [Hackage: Stack.Script](http://hackage.haskell.org/package/stack-1.9.3/docs/Stack-Script.html) Useful for figuring out what is going on underneath the hood!    
 - [Richard Odone: Scripting in Haskell and PureScript](https://odone.io/posts/2019-07-08-scripting-in-haskell-and-purescript.html)    

