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
-- The generated let-bindings looks like:
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
