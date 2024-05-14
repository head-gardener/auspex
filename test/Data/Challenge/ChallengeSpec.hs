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
        verify pp op (solve os op chl)
  where
    prep = do
      let edPath = "./data/ed25519"
      let edPathOwner = "./data/ed25519-owner"
      (secO, pubO) <- readKeys edPathOwner >>= maybe (die "Can't parse") return
      (secP, pubP) <- readKeys edPath >>= maybe (die "Can't parse") return
      chl <- newChallenge secP pubP "token" $ Just "callback"
      return (pubP, secO, pubO, chl)
