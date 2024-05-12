module Crypto.Error.Utils where

import Crypto.Error

cfToEither :: CryptoFailable a -> Either CryptoError a
cfToEither (CryptoPassed x) = Right x
cfToEither (CryptoFailed x) = Left x
