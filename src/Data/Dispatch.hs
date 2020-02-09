{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Dispatch where

import Control.Monad
import Language.Haskell.TH

import Data.WordCount

dispatch :: Name -> Q Exp
dispatch bs = reify ''Statistics >>= \case
  TyConI (DataD _ _ _ _ cons _) -> do
    let consNames = [ name | NormalC name _ <- cons ]
    let powerset = filterM (const [True, False]) consNames
    let matches = buildMatch bs <$> filter (not . null) powerset
    fallbackMatch <- (\body -> Match WildP (NormalB body) []) <$> [e| error "Unexpected input" |]
    pure $ LamCaseE $ matches <> [fallbackMatch]
  _ -> fail "unsupported type"

buildMatch :: Name -> [Name] -> Match
buildMatch bs consNames = Match (ListP $ (`ConP` []) <$> consNames) (NormalB $ VarE 'show `AppE` (wcCall `AppE` VarE bs)) []
  where
    wcCall = VarE 'wc `AppTypeE` foldr1 f (PromotedT <$> consNames)
    f accTy promotedTy = PromotedT '(:::) `AppT` accTy `AppT` promotedTy
