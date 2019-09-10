{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Utils
 ( takeIf
 , tryIO
 , orDie
 ) where

import Control.Exception
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as F
import GHC.IO.Exception
import System.Exit

instance MonadFail (Either String) where
  fail :: String -> Either String a
  fail = Left

takeIf :: MonadFail m => (a -> Bool) -> String -> a -> m a
takeIf f s a = if f a then return a else F.fail s

tryIO :: IO a -> String -> IO a
tryIO f s = catch f (\a@IOError {} -> die $ s ++ ": " ++ displayException a)

orDie :: Either String a -> String -> IO a
orDie (Left msg) s = die $ s ++ ": " ++ msg
orDie (Right x) _ = return x
