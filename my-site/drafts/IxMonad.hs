{- stack script
 --resolver lts-14.20
 -}
{-# Language RebindableSyntax
           , ScopedTypeVariables
           , FlexibleInstances
           , NoMonomorphismRestriction
           , OverloadedStrings
           , InstanceSigs
           , RoleAnnotations
#-}


module IxMonadParser where

import Control.Applicative (pure, (<$>))
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Control.Monad.IO.Class as CM
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


-- IxMonadT IO SourceCode Tokenized ()
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

newtype IxMonadT i o m a = IxMonadT { runIx :: i -> m (a, o) }

evalIxMonadT :: (CM.Functor m) => IxMonadT i o m a -> i -> m a
evalIxMonadT st i = fst <$> runIx st i

execIxMonadT :: (CM.Functor m) => IxMonadT i o m a -> i -> m o
execIxMonadT st i = snd <$> runIx st i

return :: (CM.Monad m) => a -> IxMonadT s s m a
return a = IxMonadT $ \s -> CM.return (a, s)

-- i -> o, o -> o' composition in the enriched category
(>>=) :: (CM.Monad m) => IxMonadT i c m a -> (a -> IxMonadT c o m b) -> IxMonadT i o m b
(>>=) v f = IxMonadT $ \i -> runIx v i CM.>>= \(a', o') -> runIx (f a') o'

-- erase a/b same as composition!
(>>) :: (CM.Monad m) => IxMonadT i c m a -> IxMonadT c o m b -> IxMonadT i o m b
v >> w = v >>= \_ ->  w

instance MonadTrans (IxMonadT s s) where
  lift :: (CM.Monad m) => m a -> IxMonadT s s m a
  lift ma = IxMonadT $ \s -> ma CM.>>= (\a -> CM.return (a, s))

liftIO :: CM.MonadIO m => IO a -> IxMonadT s s m a
liftIO = lift . CM.liftIO

put :: (CM.Monad m) =>  o -> IxMonadT i o m ()
put o = IxMonadT $ \_ -> CM.return ((), o)

modify :: (CM.Monad m) => (i -> o) -> IxMonadT  i o m ()
modify f = IxMonadT $ \i -> CM.return ((), f i)

get :: CM.Monad m => IxMonadT s s m s
get = IxMonadT $ \x -> CM.return (x, x)

gets :: CM.Monad m => (a -> o) -> IxMonadT a o m a
gets f = IxMonadT $ \s -> CM.return (s, f s)
--
-- make it a functor instance
-- convert to instance Functor, think about going with IxMonadT i o m a
instance (CM.Monad m) => CM.Functor (IxMonadT i o m) where
  fmap :: (CM.Monad m) => (a -> b) -> IxMonadT i o m a -> IxMonadT i o m b
  fmap f v = IxMonadT $ \i ->
    runIx v i CM.>>= \(a', o') -> CM.return (f a', o')

      -- IxMonad
run :: IxMonadT SourceCode Core IO Core
run = do                             -- XXX check all of these types with type holes
  toke <- gets source2Toke           -- :: IxMonadT IO SourceCode Tokenized ()
  liftIO $ putStrLn "inside IxMonad" -- :: IxMonadT IO Tokenized Tokenized ()
  syn <- gets toke2Syntax            -- :: IxMonadT IO Tokenized Syntax ()
  modify syntax2Core                 -- :: IxMonadT IO Syntax Core ()
  result <- get                      -- :: IxMonadT Syntax Core IO Core
                                     --    with get we can manipulate the value of the transformation
  liftIO $ print result              -- :: IxMonadT Syntax Core IO Core
  return result                      -- :: IxMonadT SourceCode Core IO Core -- (final type)

main :: IO ()
main = do
  let srcCode = SourceCode "here is my source code"
   in execIxMonadT run srcCode CM.>> print "done"

{- additional comments:
 x 1. about imports, rebindable do syntax
 x 2. use of "gets source2Toke", "gets toke2Syntax" to create parsers
 -   2.1 maybe create these parsers/ixmonads explicitly to better make the point?
 - 3. Rename IxMonadT to IxMonad
 -   3.1 add section on IxMonad isomorphism to Parser/StateT
 -   3.2 motivate inclusion of m into IxMonad, (liftIO, Ghc, et cetera)
 x 4. check which bind is used
 - 5. state, contT ix monad transformer
 - 6. IxMonads: enriched category in the monoidal category of endofunctor
 - session(state with phantom state), state -> uses
  http://www.ccs.northeastern.edu/home/tov/pubs/haskell-session-types/session08.pdf
  https://github.com/morphismtech/squeal/blob/dev/squeal-postgresql/src/Squeal/PostgreSQL/PQ/Indexed.hs
  https://mail.haskell.org/pipermail/haskell-cafe/2004-July/006448.html
  https://hackage.haskell.org/package/ixdopp
  https://github.com/ocharles/json-assertions/blob/master/src/Test/JSON/Assertions.hs
-}
