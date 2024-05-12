module Data.Challenge.ChallengeSpec (main, spec) where

-- import Test.QuickCheck

import Crypto.PubKey.Ed25519.OpenSSH (readKeys)
import Data.Challenge
import Test.Hspec

challenge :: Challenge
challenge = Challenge Nothing "hello"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "challenge" $ do
    before (readKeys "./data/ed25519" >>= maybe (fail "Can't parse") return) $ do
      it "is solved and verified correctly" $ \(s, p) -> do
        isCorrect p (solve s p challenge)
      it "can be converted to a sig" $ \(s, p) -> do
        isJust (challengeToSig (solve s p challenge))

-- it "is idempotent" $
--   property $
--     \str -> str === (reverse str :: String)
