module Crypto.PubKey.Ed25519.OpenSSHSpec (main, spec) where

import Crypto.PubKey.Ed25519
import Crypto.PubKey.Ed25519.OpenSSH
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parser" $ do
    before (readKeys "./data/ed25519") $ do
      it "can parse keyfiles" $ \ks -> do
        isJust ks
      it "is sane" $ \ks -> do
        fmap (toPublic . fst) ks `shouldBe` fmap snd ks
