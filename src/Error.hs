{-# LANGUAGE FlexibleContexts #-}

module Error
  ( module Error
  , Identity (..)
  , lift
  ) where

import Util

import Control.Applicative
import Control.Monad.Trans.Class
import Data.Functor.Identity

newtype ErrorT m a = ErrorT
  { runErrorT :: m (Either String a)
  }

instance Functor m => Functor (ErrorT m) where
  fmap f = ErrorT . fmap (fmap f) . runErrorT

instance Applicative m => Applicative (ErrorT m) where
  pure = ErrorT . pure . Right
  mf <*> mx = ErrorT $ ((<*>) <$> runErrorT mf) <*> runErrorT mx

instance (Monad m) => Monad (ErrorT m) where
  return a = ErrorT $ return $ Right a
  m >>= f  = ErrorT $ either (return . Left) (runErrorT . f) =<< runErrorT m
  fail msg = ErrorT $ return $ Left msg

instance MonadTrans ErrorT where
  lift m = ErrorT $ do
    a <- m
    return (Right a)

type Error = ErrorT Identity

runError :: Error a -> Either String a
runError m = runIdentity $ runErrorT m

lowerHandleError :: (Monad m) => ErrorT m a -> (String -> m b) -> (a -> m b) -> m b
lowerHandleError m f s = either f s =<< runErrorT m

handleError :: Error a -> (String -> b) -> (a -> b) -> b
handleError m f s = runIdentity $ handleError m (Identity . f) (Identity . s)

catch :: (Monad m) => ErrorT m a -> (String -> ErrorT m a) -> ErrorT m a
catch m f = ErrorT $ do
  e <- runErrorT m
  case e of
    Left err -> runErrorT $ f err
    Right a  -> return $ Right a

lowerDropError :: (Monad m) => ErrorT m a -> m (Maybe a)
lowerDropError m = return . either (const Nothing) Just =<< runErrorT m

dropError :: Error a -> Maybe a
dropError = either (const Nothing) Just . runError

liftReportNothing :: (Monad m) => String -> m (Maybe a) -> ErrorT m a
liftReportNothing msg m = ErrorT $ return . maybe (Left msg) Right =<< m

reportNothing :: (Monad m) => String -> Maybe a -> ErrorT m a
reportNothing msg m = case m of
  Just a  -> return a
  Nothing -> fail msg

fails :: (Monad m) => String -> String -> m a
fails e1 e2 = fail $ unlines [e1,e2]

wrapFail :: (Monad m) => String -> ErrorT m a -> ErrorT m a
wrapFail msg m = catch m $ fail . addLine msg

errorLookup' :: (Monad m, Eq a, Show a) => [(a,b)] -> a -> ErrorT m b
errorLookup' al a = reportNothing err $ lookup a al
  where
  err = "Couldn't find key " ++ show a ++ "in assoc list"

errorLookup :: (Monad m, Show a, Show b, Eq a) => [(a,b)] -> a -> ErrorT m b
errorLookup al a = reportNothing err $ lookup a al
  where
  err = "Couldn't find key " ++ show a ++ " in assoc list " ++ show al

fromEither :: Either String a -> Error a
fromEither = ErrorT . Identity

