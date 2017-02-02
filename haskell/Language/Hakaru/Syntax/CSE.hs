{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Language.Hakaru.Syntax.CSE (cse) where

import           Control.Monad.Reader
import qualified Data.IntMap                     as IM
import           Data.Maybe
import           Data.Number.Nat
import           Data.Sequence                   ((<|))
import qualified Data.Sequence                   as S
import           Prelude                         hiding ((+))

import           Language.Hakaru.Syntax.ABT
import           Language.Hakaru.Syntax.AST
import           Language.Hakaru.Syntax.AST.Eq
import           Language.Hakaru.Syntax.Datum
import           Language.Hakaru.Syntax.IClasses
import           Language.Hakaru.Syntax.TypeOf
import           Language.Hakaru.Types.DataKind

import           Language.Hakaru.Syntax.Prelude  hiding (fst, (.), (>>=))

-- What we need is an environment like data structure which maps Terms (or
-- general abts?) to other abts. Can such a mapping be implemented efficiently?
-- This would seem to require a hash operation to make efficient.

data EAssoc (abt :: [Hakaru] -> Hakaru -> *)
  = forall a . EAssoc !(abt '[] a) !(abt '[] a)

-- An association list for now
newtype Env (abt :: [Hakaru] -> Hakaru -> *) = Env [EAssoc abt]

emptyEnv :: Env a
emptyEnv = Env []

-- Attempt to find a new expression in then environment. The lookup is chained
-- to iteratively perform lookup until no match is found, resulting in an
-- equivalence-relation in the environment. This could be made faster with path
-- compression and a more efficient lookup structure.
-- NB: This code could potentially produce an infinite loop depending on how
-- terms are added to the environment. How do we want to prevent this?
lookupEnv
  :: forall abt a . (ABT Term abt)
  => abt '[] a
  -> Env abt
  -> abt '[] a
lookupEnv ast (Env env) = go ast env
  where
    go :: abt '[] a -> [EAssoc abt] -> abt '[] a
    go ast []                = ast
    go ast (EAssoc a b : xs) =
      case jmEq1 (typeOf ast) (typeOf a) of
        Nothing                   -> go ast xs
        Just Refl | alphaEq ast a -> go b env
                  | otherwise     -> go ast xs

insertEnv
  :: forall abt a . (ABT Term abt)
  => abt '[] a
  -> abt '[] a
  -> Env abt
  -> Env abt
insertEnv ast1 ast2 (Env env) =
  case viewABT ast1 of
    -- Point new variables to the older ones, this does not affect the amount of
    -- work done, since ast2 is always a variable. This allows the pass to
    -- eliminate redundant variables, as we only eliminate binders during CSE.
    Var v -> Env (EAssoc ast2 ast1 : env)
    -- Otherwise map expressions to their binding variables
    _     -> Env (EAssoc ast1 ast2 : env)

newtype CSE (abt :: [Hakaru] -> Hakaru -> *) a = CSE { runCSE :: Reader (Env abt) a }
  deriving (Functor, Applicative, Monad, MonadReader (Env abt))

replaceCSE
  :: (ABT Term abt)
  => abt '[] a
  -> CSE abt (abt '[] a)
replaceCSE abt = lookupEnv abt `fmap` ask

cse :: forall abt a . (ABT Term abt) => abt '[] a -> abt '[] a
cse abt = runReader (runCSE (cse' abt)) emptyEnv

cse' :: forall abt xs a . (ABT Term abt) => abt xs a -> CSE abt (abt xs a)
cse' = loop . viewABT
  where
    loop :: View (Term abt) ys a ->  CSE abt (abt ys a)
    loop (Var v)    = cseVar v
    loop (Syn s)    = cseTerm s
    loop (Bind v b) = fmap (bind v) (loop b)

-- Variables can be equivalent to other variables
-- TODO: A good sanity check would be to ensure the result in this case is
-- always a variable or constant. A variable should never be substituted for
-- a more complex expression.
cseVar
  :: (ABT Term abt)
  => Variable a
  -> CSE abt (abt '[] a)
cseVar v = replaceCSE (var v)

getVar :: (ABT Term abt) => abt '[] a -> Maybe (Variable a)
getVar abt = case viewABT abt of
               Var v -> Just v
               _     -> Nothing

mklet :: ABT Term abt => Variable b -> abt '[] b -> abt '[] a -> abt '[] a
mklet v rhs body = syn (Let_ :$ rhs :* bind v body :* End)

-- Thanks to A-normalization, the only case we need to care about is let bindings.
-- Everything else is just structural recursion.
cseTerm
  :: (ABT Term abt)
  => Term abt a
  -> CSE abt (abt '[] a)

cseTerm (Let_ :$ rhs :* body :* End) = do
  rhs' <- cse' rhs
  caseBind body $ \v body' ->
    local (insertEnv rhs' (var v)) $
      case getVar rhs' of
        Just _ -> cse' body'
        _      -> fmap (mklet v rhs') (cse' body')

cseTerm term = traverse21 cse' term >>= replaceCSE . syn

