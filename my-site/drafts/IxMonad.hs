{- stack script
 --resolver lts-14.20
 -}
{-# Language RebindableSyntax
           , ScopedTypeVariables
           , NoMonomorphismRestriction
           , OverloadedStrings
           , RoleAnnotations
#-}


module IxMonadParser where

import Control.Applicative (pure, (<$>))
import qualified Control.Monad as CM
import Data.Coerce (Coercible, coerce)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.String (fromString)
import Data.Text (Text)
import Data.Tuple (fst, snd)
import GHC.Generics (Generic)
import Prelude (fromIntegral, fromInteger, IO, Show, print, putStrLn)
{-
   Motivation: Is it possible to compose parsers and state monads of different input and output types?
   where a parser/state monad is the form
       data M m i o a = M { uM :: i -> m (o, a) }
   and
       given   M m A B ()   and   M m B C ()
       create  M m A C ()

   YES! with Indexed Monads?

    If we look at IxMonads, especially indexed state, we notice it's isomorphic to state.

    Starting with our trusty old parser, let's derive an indexed monad, with input/output types,
    where we can also run IO effects, if need be.

    newtype OldParser a         = OldParser { unParser :: String -> Maybe (a, String) }
    newtype OldParser' m a s    = OldParser' { unParser' :: s -> m (a, s) }
    newtype OldParser' m a i o  = OldParser' { unParser' :: i -> m (a, o) }

   <insert isomorphism to StateT>

-}
{-
    For our computation, we want to start with `SourceCode`, and produce a `Core`.
    We have the following types, as part of a compilation pipeline for the
    programming language "Arbitrary"

-}
newtype SourceCode = SourceCode Text
newtype Tokenized = Tokenized [Text]
data    Expr = EInt Int | EStr Text | EVar Text | EApp Expr Expr deriving (Show)
newtype Syntax = Syntax { unSyntax :: Expr } deriving (Show)
newtype Core  = Core { unCore :: Expr } deriving (Show)

{-
Here we can see the transformation functions:
For the sake of this demonstration, I just used coerce, or stubbed out a const function.
-}


-- IParser IO SourceCode Tokenized ()
source2Toke :: SourceCode -> Tokenized
source2Toke (SourceCode txt) = Tokenized [txt] -- can we coerce here as well?

toke2Syntax :: Tokenized -> Syntax
toke2Syntax _ = Syntax $ EApp (EVar "Fn") $ EInt . fromIntegral  $ 42

syntax2Core :: Syntax -> Core
syntax2Core = coerce -- "safe" newtype coerce

{-
  Thus, we want a pipeline with the following...
  (SourceCode) => (Tokenized) => (Syntax) => (Core)
  which is done
  source2Toke . toke2Syntax . syntax2core
-}

{-
 Now, we need to derive all the types for our indexed monad.
 Credit to Stephen Diehl for providing a great starting point:
   http://dev.stephendiehl.com/hask/#indexed-monads
-}

newtype IParser m i o a = IParser { runParser :: i -> m (a, o) }

evalIParser :: (CM.Functor m) => IParser m i o a -> i -> m a
evalIParser st i = fst <$> runParser st i

execIParser :: (CM.Functor m) => IParser m i o a -> i -> m o
execIParser st i = snd <$> runParser st i

return :: (CM.Monad m) => a -> IParser m s s a
return a = IParser $ \s -> CM.return (a, s)

-- make it a functor instance
-- convert to instance Functor, think about going with IParser i o m a
fmap :: (CM.Monad m) => (a -> b) -> IParser m i o a -> IParser m i o b
fmap f v = IParser $ \i ->
  runParser v i CM.>>= \(a', o') -> CM.return (f a', o')

-- i -> o, o -> o' composition in the enriched category
(>>=) :: (CM.Monad m) => IParser m i o a -> (a -> IParser m o o' b) -> IParser m i o' b
(>>=) v f = IParser $ \i -> runParser v i CM.>>= \(a', o') -> runParser (f a') o'

-- erase a/b same as composition!
(>>) :: (CM.Monad m) => IParser m i c a -> IParser m c o b -> IParser m i o b
v >> w = v >>= \_ ->  w

lift :: (CM.Monad m) => m a -> IParser m s s a
lift ma = IParser $ \s -> ma CM.>>= (\a -> CM.return (a, s))

liftIO :: IO a -> IParser IO s s a
liftIO ma = IParser $ \s -> ma CM.>>= (\a -> CM.return (a, s))

put :: (CM.Monad m) =>  o -> IParser m i o ()
put o = IParser $ \_ -> CM.return ((), o)

modify :: (CM.Monad m) => (i -> o) -> IParser m  i o ()
modify f = IParser $ \i -> CM.return ((), f i)

get :: CM.Monad m => IParser m s s s
get = IParser $ \x -> CM.return (x, x)

gets :: CM.Monad m => (a -> o) -> IParser m a o a
gets f = IParser $ \s -> CM.return (s, f s)

      -- IxMonad
run :: IParser IO SourceCode Core ()
run = do                             -- XXX check all of these types with type holes
  toke <- gets source2Toke           -- :: IParser IO SourceCode Tokenized ()
  liftIO $ putStrLn "inside IxMonad" -- :: IParser IO Tokenized Tokenized ()
  syn <- gets toke2Syntax            -- :: IParser IO Tokenized Syntax ()
  modify syntax2Core                 -- :: IParser IO Syntax Core ()

main :: IO ()
main = do
  let srcCode = SourceCode "here is my source code"
   in  execIParser run srcCode CM.>>= \core -> print core

{- additional comments:
 - 1. about imports, rebindable do syntax
 - 2. use of "gets source2Toke", "gets toke2Syntax" to create parsers
 -   2.1 maybe create these parsers/ixmonads explicitly to better make the point?
 - 3. Rename IParser to IxMonad
 -   3.1 add section on IxMonad isomorphism to Parser/StateT
 -   3.2 motivate inclusion of m into IxMonad, (liftIO, Ghc, et cetera)
 - 4. check which bind is used
 - 5. state, contT ix monad transformer
 - 6. IxMonads: enriched category in the monoidal category of endofunctor
 - session(state with phantom state), state -> uses
 - http://www.ccs.northeastern.edu/home/tov/pubs/haskell-session-types/session08.pdf
 - https://github.com/morphismtech/squeal/blob/dev/squeal-postgresql/src/Squeal/PostgreSQL/PQ/Indexed.hs
 - https://mail.haskell.org/pipermail/haskell-cafe/2004-July/006448.html
 - https://hackage.haskell.org/package/ixdopp
-}
