{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.TTH.LetRec (
    letrecE,
    letrecH,
) where

import Control.Monad.Fix          (MonadFix)
import Data.GADT.Compare          (GCompare)
import Data.Some                  (Some (..))
import Language.Haskell.TH.Syntax (Code, Quote, unTypeCode, unsafeCodeCoerce)

import qualified Language.Haskell.TH.LetRec as TH.LetRec

-- $setup
-- >>> :set -XGADTs -XTypeOperators -XDataKinds -XPolyKinds -XRankNTypes
-- >>> import Control.Monad.Fix (MonadFix)
-- >>> import Data.GADT.Compare
-- >>> import Data.Type.Equality
-- >>> import Language.Haskell.TH.Syntax as TH
-- >>> import Language.Haskell.TH.Ppr    as TH

-- | Generate potentially recursive let expression.
--
-- Example of generating a list ofg alternative 'True' and 'False' values.
--
-- >>> let trueFalse = letrecE (\tag -> "go" ++ show tag) (\rec tag -> rec (not tag) >>= \next -> return [|| $$(TH.liftTyped tag) : $$next ||]) (\rec -> rec True)
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
    :: forall q tag r a. (Ord tag, Quote q, MonadFix q)
    => (forall. tag -> String)                                                 -- ^ tag naming function
    -> (forall m. Monad m => (tag -> m (Code q a)) -> (tag -> m (Code q a)))   -- ^ bindings generator (with recursive function)
    -> (forall m. Monad m => (tag -> m (Code q a)) -> m (Code q r))            -- ^ final expression generator
    -> Code q r                                                                -- ^ generated let expression
letrecE nameOf bindf exprf = unsafeCodeCoerce $ TH.LetRec.letrecE
    nameOf
    (\recf tag -> unTypeCode <$> bindf (\tag' -> unsafeCodeCoerce <$> recf tag') tag)
    (\recf     -> unTypeCode <$> exprf (\tag' -> unsafeCodeCoerce <$> recf tag'))

-- | Generate potentially recursive let expression with heterogenously typed bindings.
--
-- A simple example is consider a case where you have a @NP@ (from @sop-core@) of @Code@ values
--
-- >>> :{
-- data NP f xs where
--    Nil  :: NP f '[]
--    (:*) :: f x -> NP f xs -> NP f (x : xs)
-- infixr 5 :*
-- :}
--
-- >>> :{
-- let values :: TH.Quote q => NP (Code q) '[ Bool, Char ]
--     values = [|| True ||] :* [|| 'x' ||] :* Nil
-- :}
--
-- and function from that to a single @Code@
--
-- >>> :{
-- let gen :: TH.Quote q => NP (Code q) '[ Bool, Char ] -> Code q String
--     gen (x :* y :* Nil) = [|| $$y : $$y : show $$x ||]
-- :}
--
-- We can apply @gen@ to @values@ to get a code expression:
--
-- >>> TH.ppr <$> TH.unTypeCode (gen values)
-- 'x' GHC.Types.: ('x' GHC.Types.: GHC.Show.show GHC.Types.True)
--
-- But if @values@ where big, we would potentially duplicate the computations.
-- Better to first let-bind them.
--
-- We'll need a type to act as a tag:
--
-- >>> :{
-- data Idx xs x where
--    IZ :: Idx (x ': xs) x
--    IS :: Idx xs x -> Idx (y ': xs) x
-- instance GEq (Idx xs) where geq = defaultGeq
-- instance GCompare (Idx xs) where
--     gcompare IZ     IZ     = GEQ
--     gcompare (IS x) (IS y) = gcompare x y
--     gcompare IZ     (IS _) = GLT
--     gcompare (IS _) IZ     = GGT
-- :}
--
-- Using @Idx@ we can index @NP@ values:
--
-- >>> :{
-- let index :: NP f xs -> Idx xs x -> f x
--     index (x :* _)  IZ     = x
--     index (_ :* xs) (IS i) = index xs i
-- :}
--
-- And with some extra utilities
--
-- >>> mapNP :: (forall x. f x -> g x) -> NP f xs -> NP g xs; mapNP _ Nil = Nil; mapNP f (x :* xs) = f x :* mapNP f xs
-- >>> traverseNP :: Applicative m => (forall x. f x -> m (g x)) -> NP f xs -> m (NP g xs); traverseNP _ Nil = pure Nil; traverseNP f (x :* xs) = (:*) <$> f x <*> traverseNP f xs
-- >>> indices :: NP f xs -> NP (Idx xs) xs; indices Nil = Nil; indices (_ :* xs) = IZ :* mapNP IS (indices xs) -- first argument acts as list singleton
--
-- we can make a combinator for generating dynamic let-expression:
--
-- >>> :{
-- let letNP :: (Quote q, MonadFix q) => NP (Code q) xs -> (NP (Code q) xs -> Code q r) -> Code q r
--     letNP vals g = letrecH (\_ -> "x") (\_rec idx -> return (index vals idx)) (\rec -> do { vals' <- traverseNP rec (indices vals); return (g vals') })
-- :}
--
-- and use it to bind 'values' before using them in 'gen':
--
-- >>> TH.ppr <$> TH.unTypeCode (letNP values gen)
-- let {x_0 = GHC.Types.True; x_1 = 'x'}
--  in x_1 GHC.Types.: (x_1 GHC.Types.: GHC.Show.show x_0)
--
-- The result of evaluating either expression is the same:
--
-- >>> $$(gen values)
-- "xxTrue"
--
-- >>> $$(letNP values gen)
-- "xxTrue"
--
-- This example illustrates that 'letrecH' is more general than something
-- like @letNP@ and doesn't require extra data-structures
-- (Instead of having 'GCompare' constraint the function can ask for @tag x -> tag y -> Maybe (x :~: y)@ function)
--
letrecH
    :: forall q tag r. (GCompare tag, Quote q, MonadFix q)
    => (forall x. tag x -> String)                                                             -- ^ tag naming function
    -> (forall m y. Monad m => (forall x. tag x -> m (Code q x)) -> (tag y -> m (Code q y)))   -- ^ bindings generator (with recursive function)
    -> (forall m.   Monad m => (forall x. tag x -> m (Code q x)) -> m (Code q r))              -- ^ final expression generator
    -> Code q r                                                                                -- ^ generated let expression
letrecH nameOf bindf exprf = unsafeCodeCoerce $ TH.LetRec.letrecE
    (\(Some tag) -> nameOf tag)
    (\recf (Some tag) -> unTypeCode <$> bindf (\tag' -> unsafeCodeCoerce <$> recf (Some tag')) tag)
    (\recf            -> unTypeCode <$> exprf (\tag' -> unsafeCodeCoerce <$> recf (Some tag')))
