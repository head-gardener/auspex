{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application where

import Foundation
import Yesod.Auth
import Yesod.Core

import Routes

mkYesodDispatch "App" resourcesApp
