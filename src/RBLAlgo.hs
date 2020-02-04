module RBLAlgo where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (Exception)
import Control.Monad ((<=<), (>=>), join)
import Control.Monad.IO.Class (MonadIO)
import Data.IP

import RBL (Domain, Provider, Providers)

checkDomain' f lookUpDomain lookUpRBL providerHandle (handleErr, parseResult) providers =
  either handleErr handleResult `f` lookUpDomain
  where
    handleResult = parseResult `f` mapM lookUpRBL'
    lookUpRBL' ip = mapM (flip lookUpRBL'' ip) providers
    lookUpRBL'' provider = providerHandle' `f` lookUpRBL provider
    providerHandle' = uncurry either providerHandle

checkDomainM ::
     (Monad m, Exception e, Exception e')
  => (Domain -> m (Either e [IPv4]))
  -> (Provider Domain -> IPv4 -> m (Either e' (Provider [IP])))
  -> (e' -> m b, Provider [IP] -> m b)
  -> (e -> m c, [[b]] -> m c)
  -> Providers
  -> Domain
  -> m c
checkDomainM lookUpDomain lookUpRBL providerHandle (handleErr, parseResult) providers =
  lookUpDomain >=> either handleErr handleResult
  where
    handleResult = mapM lookUpRBL' >=> parseResult
    lookUpRBL' ip = mapM (flip lookUpRBL'' ip) providers
    lookUpRBL'' provider = lookUpRBL provider >=> providerHandle'
    providerHandle' = uncurry either providerHandle

checkDomain ::
     (Monad m, Exception e, Exception e')
  => (Domain -> m (Either e [IPv4]))
  -> (Provider Domain -> IPv4 -> m (Either e' (Provider [IP])))
  -> (e' -> b, Provider [IP] -> b)
  -> (e -> c, [[b]] -> c)
  -> Providers
  -> Domain
  -> m c
checkDomain lookUpDomain lookUpRBL providerHandle (handleErr, parseResult) providers =
  join . (either (return . handleErr) handleResult <.> lookUpDomain)
  where
    handleResult = parseResult <.> mapM lookUpRBL'
    lookUpRBL' ip = mapM (flip lookUpRBL'' ip) providers
    lookUpRBL'' provider = providerHandle' <.> lookUpRBL provider
    providerHandle' = uncurry either providerHandle

infixr 1 <.>

f <.> g = \x -> f <$> g x
