{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.TH.LetRec (
    letrecE,
) where

import Control.Monad.Fix              (MonadFix)
import Control.Monad.Trans.Class      (lift)
import Control.Monad.Trans.State.Lazy (StateT, get, modify, runStateT)
import Language.Haskell.TH.Lib        (letE, normalB, valD, varE, varP)
import Language.Haskell.TH.Syntax     (Exp, Name, Quote (newName))

import qualified Data.Map.Lazy as Map

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Language.Haskell.TH.Syntax as TH
-- >>> import Language.Haskell.TH.Lib    as TH
-- >>> import Language.Haskell.TH.Ppr    as TH

-- | Generate potentially recursive let expression.
--
-- The 'Monad' constraint in generators forces to sequence
-- binding generation calls, thus allowing to do lazy binding generation.
--
-- Example of generating a list of alternating 'True' and 'False' values:
--
-- >>> let trueFalse = letrecE (\tag -> "go" ++ show tag) (\rec tag -> rec (not tag) >>= \next -> return [| $(TH.lift tag) : $next |]) ($ True)
--
-- The generated let-bindings look like:
--
-- >>> TH.ppr <$> trueFalse
-- let {goFalse_0 = GHC.Types.False GHC.Types.: goTrue_1;
--      goTrue_1 = GHC.Types.True GHC.Types.: goFalse_0}
--  in goTrue_1
--
-- And when spliced it produces a list of alternative 'True' and 'False' values:
--
-- >>> take 10 $trueFalse
-- [True,False,True,False,True,False,True,False,True,False]
--
-- Another example where dynamic nature is visible is generating
-- fibonacci numbers:
--
-- >>> let fibRec rec tag = case tag of { 0 -> return [| 1 |]; 1 -> return [| 1 |]; _ -> do { minus1 <- rec (tag - 1); minus2 <- rec (tag - 2); return [| $minus1 + $minus2 |] }}
-- >>> let fib n = letrecE (\tag -> "fib" ++ show tag) fibRec ($ n)
--
-- The generated let-bindings look like:
-- >>> TH.ppr <$> fib 7
-- let {fib0_0 = 1;
--      fib1_1 = 1;
--      fib2_2 = fib1_1 GHC.Num.+ fib0_0;
--      fib3_3 = fib2_2 GHC.Num.+ fib1_1;
--      fib4_4 = fib3_3 GHC.Num.+ fib2_2;
--      fib5_5 = fib4_4 GHC.Num.+ fib3_3;
--      fib6_6 = fib5_5 GHC.Num.+ fib4_4;
--      fib7_7 = fib6_6 GHC.Num.+ fib5_5}
--  in fib7_7
--
-- And the result is expected:
--
-- >>> $(fib 7)
-- 21
--
letrecE
    :: forall q tag. (Ord tag, Quote q, MonadFix q)
    => (tag -> String)                                                   -- ^ tag naming function
    -> (forall m. Monad m => (tag -> m (q Exp)) -> (tag -> m (q Exp)))   -- ^ bindings generator (with recursive function)
    -> (forall m. Monad m => (tag -> m (q Exp)) -> m (q Exp))            -- ^ final expression generator
    -> q Exp                                                             -- ^ generated let expression.
letrecE nameOf recf exprf = do
    (expr0, bindings) <- runStateT (exprf loop) Map.empty
    letE
        [ valD (varP name) (normalB expr) []
        | (_tag, (name, expr)) <- Map.toList bindings
        ]
        expr0
  where
    loop :: tag -> StateT (Map.Map tag (Name, q Exp)) q (q Exp)
    loop tag = do
        m <- get
        case Map.lookup tag m of
            -- if name is already generated, return it.
            Just (name, _exp) -> return (varE name)

            -- otherwise generate new name, and insert it into the loop.
            Nothing -> mdo
                name <- lift (newName (nameOf tag))
                modify (Map.insert tag (name, expr))
                expr <- recf loop tag
                return (varE name)
