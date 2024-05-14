module Data.Challenge.ChallengeSpec (main, spec) where

import Crypto.PubKey.Ed25519.OpenSSH (readKeys)
import Data.Challenge
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "challenge" $ do
    before prep $ do
      it "is solved and verified correctly" $ \(pp, os, op, chl) -> do
        verify op pp (solve os op chl)
  where
    prep = do
      let edPath = "./data/ed25519"
      (sec, pub) <- readKeys edPath >>= maybe (die $ "Can't parse" <> edPath) return
      chl <- newChallenge sec pub "token" $ Just "callback"
      return (pub, sec, pub, chl)
