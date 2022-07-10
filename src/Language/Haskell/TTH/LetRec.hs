{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.TTH.LetRec (
    letrecE,
) where

import Control.Monad.Fix          (MonadFix)
import Data.GADT.Compare          (GCompare)
import Data.Some                  (Some (..))
import Language.Haskell.TH.Syntax (Code, Quote, unTypeCode, unsafeCodeCoerce)

import qualified Language.Haskell.TH.LetRec as TH.LetRec

-- $setup
-- >>> :set -XGADTs
-- >>> import Data.GADT.Compare
-- >>> import Data.Type.Equality
-- >>> import Language.Haskell.TH.Syntax as TH
-- >>> import Language.Haskell.TH.Ppr    as TH

-- | Generate potentially recursive let expression.
--
-- Example of generating a list ofg alternative 'True' and 'False' values.
--
-- First we need a tag. In this example all bindings have same type.
-- the needed type is relatively simple.
-- Note: we will generate expressions of type @[Bool]@, so our @Tag@ GADT
-- has to have it as final param:
-- 
-- >>> data Tag x where Tag :: Bool -> Tag [Bool]
-- >>> instance GEq Tag where geq (Tag x) (Tag y) = if x == y then Just Refl else Nothing
-- >>> instance GCompare Tag where gcompare (Tag x) (Tag y) = case compare x y of { EQ -> GEQ; LT -> GLT; GT -> GGT }
--
-- Then we can generate the expression:
--
-- >>> let trueFalse = letrecE (\(Tag tag) -> "go" ++ show tag) (\rec (Tag tag) -> rec (Tag (not tag)) >>= \next -> return [|| $$(TH.liftTyped tag) : $$next ||]) (\rec -> rec (Tag True))
--
-- The generated let-bindings looks like:
--
-- >>> TH.ppr <$> TH.unTypeCode trueFalse
-- let {goFalse_0 = GHC.Types.False GHC.Types.: goTrue_1;
--      goTrue_1 = GHC.Types.True GHC.Types.: goFalse_0}
--  in goTrue_1
--
-- And when spliced it produces a list of alternative 'True' and 'False' values:
--
-- >>> take 10 $$trueFalse
-- [True,False,True,False,True,False,True,False,True,False]
--
letrecE
    :: forall q tag r. (GCompare tag, Quote q, MonadFix q)
    => (forall x. tag x -> String)                                                             -- ^ tag naming function
    -> (forall m y. Monad m => (forall x. tag x -> m (Code q x)) -> (tag y -> m (Code q y)))   -- ^ bindings generator (with recursive function)
    -> (forall m.   Monad m => (forall x. tag x -> m (Code q x)) -> m (Code q r))              -- ^ final expression generator
    -> Code q r                                                                                -- ^ generated let expression
letrecE nameOf bindf exprf = unsafeCodeCoerce $ TH.LetRec.letrecE
    (\(Some tag) -> nameOf tag)
    (\recf (Some tag) -> unTypeCode <$> bindf (\tag' -> unsafeCodeCoerce <$> recf (Some tag')) tag)
    (\recf            -> unTypeCode <$> exprf (\tag' -> unsafeCodeCoerce <$> recf (Some tag')))
