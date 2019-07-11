#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE FlexibleInstances #-}


-- Learned from the article at, https://www.fpcomplete.com/blog/2017/06/readert-design-pattern

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as State
import Control.Concurrent.STM
import Say
import Test.Hspec

{-
  The core of this article is how to set things up in an environment
  and access it throughout the application.

  Trouble arises when we move to HasLog and HasBalance approaches to
  separating concerns. It adds heft to the type signature. However, it
  also makes it easier to test atomic parts later.

  The ease of this approach is nice. I didn't know how to do this when
  working on some of my smaller examples. For instance, I wanted to
  make persisted models that would communicate with the database but I
  didn't want database code all throughout my code. This allows me to
  test with State locally or verify that logs are written too.
-}

data Env = Env
  { envLog :: !(String -> IO ())
  , envBalance :: !(TVar Int)
  }

class HasLog a where
  getLog :: a -> (String -> IO ())
instance HasLog (String -> IO ()) where
  getLog = id
instance HasLog Env where
  getLog = envLog


class HasBalance a where
  getBalance :: a -> TVar Int
instance HasBalance (TVar Int) where
  getBalance = id
instance HasBalance Env where
  getBalance = envBalance


{-
  MonadBalance includes the name monad so its clear that we are using
  it later. Maybe this is a design smell, but it makes it more clear
  that we'll be using that later.
-}
class Monad m => MonadBalance m where
  modifyBalance :: (Int -> Int) -> m ()

instance (HasBalance env, MonadIO m) => MonadBalance (ReaderT env m) where
  modifyBalance f = do
    env <- ask
    liftIO $ atomically $ modifyTVar' (getBalance env) f

instance Monad m => MonadBalance (State.StateT Int m) where
  modifyBalance f = State.modify f

{-
  With using the HasLog approach, we can test this function without it
  being coupled to logSomething.  By using MonadBalance we can
  get away from the restriction of MonadIO everywhere.

  StateT is a good approach to abstract databases locally to know the
  operations are correct.
-}

modify :: MonadBalance m
       => (Int -> Int)
       -> m ()

modify = modifyBalance


logSomething :: (MonadReader env m, HasLog env, MonadIO m)
             => String
             -> m ()
logSomething msg = do
  env <- ask
  liftIO $ getLog env msg


main :: IO ()
main = do

  hspec $ do
    describe "modify" $ do
      it "works on IO" $ do
        var <- newTVarIO (1 :: Int)
        runReaderT (modify (+ 2)) var
        res <- readTVarIO var
        res `shouldBe` 3

      it "works with StateT" $ do
        res <- State.execStateT (modify (+ 2)) (0 :: Int)
        res `shouldBe` 2


    describe "logSomething" $ do
      it "works without Env" $ do
        var <- newTVarIO ""
        let logFunc msg = atomically $ modifyTVar var (++ msg)
            msg1 = "Hello "
            msg2 = "World\n"
        runReaderT (logSomething msg1 >> logSomething msg2) logFunc
        res <- readTVarIO var
        res `shouldBe` (msg1 ++ msg2)

  ------------------
  ref <- newTVarIO 4
  let env = Env
        { envLog = sayString
        , envBalance = ref
        }
  runReaderT
    (concurrently
      (modify (+ 1))
      (logSomething "Increasing account balance"))
    env

  balance <- readTVarIO ref
  sayString $ "Final balance: " ++ show balance
