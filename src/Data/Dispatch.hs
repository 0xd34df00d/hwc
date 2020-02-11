{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Dispatch where

import Control.Monad
import Language.Haskell.TH

import Data.WordCount

dispatch :: Name -> Name -> Q Exp
dispatch fun bs = reify ''Statistics >>= \case
  TyConI (DataD _ _ _ _ cons _) -> do
    let consNames = [ name | NormalC name _ <- cons ]
    let powerset = filterM (const [True, False]) consNames
    let matches = buildMatch fun bs <$> filter (not . null) powerset
    fallbackMatch <- (\body -> Match WildP (NormalB body) []) <$> [e| error "Unexpected input" |]
    pure $ LamCaseE $ matches <> [fallbackMatch]
  _ -> fail "unsupported type"

buildMatch :: Name -> Name -> [Name] -> Match
buildMatch fun bs consNames = Match (ListP $ (`ConP` []) <$> consNames) (NormalB $ VarE 'prettyPrint `AppE` (wcCall `AppE` VarE bs)) []
  where
    wcCall = VarE fun `AppTypeE` foldr1 f (PromotedT <$> consNames)
    f accTy promotedTy = PromotedT '(:::) `AppT` accTy `AppT` promotedTy
