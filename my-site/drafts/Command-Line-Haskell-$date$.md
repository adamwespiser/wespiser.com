---
title: Stack Script: Run Haskell with less config
header: A report about using the stack script interpreter to run single file haskell scripts
date: December 19, 2019
---

This post is about building Haskell source code within a single file using The Haskell Tool Stack's scripting interpreter, which can compile and run single file Haskell codebases, or "scripts".
Stack is a build tool primarily designed for reproducible builds, done by specifying a resolver in a configuration file, usually your projects `stack.yaml`. 
With Stack's scripting feature, we still get reproducible builds by specifying a resolver, but move this specification to the file we are compiling, or as a command line argument. 

There are at least two key motivations behind why you might want to use Stack's scripting interpreter:
 - Lower the configuration barrier: write an independently compiling Haskell source code file with package dependencies without having to configure a new stack or cabal project. Personally, I find this helpful when exploring new libraries or writing small programs. 
 - Using Haskell as a scripting language, or replacement for Shell/Bash/Zsh. This use case pairs well with the `Turtle` library, although this approach does have downsides. 

This article contains the following examples of using scripting with stack:
 - A basic example
 - A simple Servant server that statically serves your current working directory
 - An example of a bash install script, and Haskell replacement that can be run as a cron job. 

## Stack Script Interpreter
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
 --package servant-server
 --package warp
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
 - The snapshot here is the nightly update, as of time of writing, [nightly-2019-12-22](https://www.stackage.org/nightly-2019-12-22), to ensure we grab the most up to date version of every library.
 - We pass in `-Wall` to ghc-options, which can help us

On a fresh compilation, this will take a few minutes to run, as Stack needs to go and grad about 255Mb worth of source code in ~86 dependent packages, compile and link it in order for the above code to run.
However, on subsequent runs, Stack can use a local cache of of the packages, and we can reproduce our project build without downloading and building all the dependencies! 

