{-# LANGUAGE OverloadedStrings #-}
module Coala.Input.SingleFile
    ( SingleFile
    , singleFile
    ) where

import Coala ( Filename, emptyFilename, newFilename )
import Coala.Input ( Settings, emptySettings )

import Data.Text ( Text )
import Data.Text.Encoding ( encodeUtf8 )
import Data.ByteString.Lazy ( ByteString )
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson ( (.:) )
import Data.Aeson.Types ( parseMaybe )

import Data.Aeson ( decode, Object )

data SingleFile = SingleFile  { filename :: Filename -- ^ Source filename
                              , file :: [Text]     -- ^ Lines of source file
                              , settings :: Settings -- ^ Settings
                              } deriving ( Eq, Show )

singleFile :: (ByteString -> Maybe SingleFile)
singleFile content = do o <- decode content :: Maybe Object
                        flip parseMaybe o $ \obj -> do
                          fn <- obj .: "filename"
                          t <- obj .: "file"
                          s <- obj .: "settings"
                          return $ SingleFile (newFilename fn) t s
