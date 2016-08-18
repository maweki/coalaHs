{-# LANGUAGE OverloadedStrings #-}
module Coala
    ( Filename
    , emptyFilename
    , newFilename
    , coala
    , coalaIO
    , Result
    , Severity
    , Line
    , Column
    , Affect
    , CodeRef
    , encodeResults
    ) where

import Prelude hiding ( getContents, putStr )
import Data.ByteString.Lazy ( putStr, ByteString, getContents )
import Data.Text ( Text, empty )
import Data.Aeson ( ToJSON, toJSON, object, (.=), Value , encode )

newtype Filename = Filename String deriving ( Eq, Show )
emptyFilename = Filename ""
newFilename fn = Filename fn

coala :: String -> (ByteString -> a) -> (a -> [Result]) -> IO ()
coala bearname reader bear = do
  c <- getContents
  putStr $ encode $ encodeResults bearname $ bear $ reader c

coalaIO :: String -> (ByteString -> a) -> (a -> IO [Result]) -> IO ()
coalaIO bearname reader bear = do
  c <- getContents
  res <- bear $ reader c
  putStr $ encode $ encodeResults bearname res

newtype Severity = Severity Int deriving ( Show, Eq )
newtype Line = Line Int deriving ( Show, Eq )
newtype Column = Column Int deriving ( Show, Eq )

data Affect = Affect  { start :: CodeRef
                      , end :: CodeRef
                      } deriving ( Eq, Show )

data CodeRef = CodeRef  { file :: Filename
                        , line :: Line
                        , column :: Maybe Column
                        }  deriving (Eq, Show)

data Result = Result  { message :: String
                      , affected :: [Affect]
                      , severity :: Severity
                      } deriving (Eq, Show)


instance ToJSON Severity where
    toJSON (Severity sev) = toJSON sev

instance ToJSON Filename where
    toJSON (Filename fn) = toJSON fn

instance ToJSON Line where
    toJSON (Line l) = toJSON l

instance ToJSON Column where
    toJSON (Column c) = toJSON c

instance ToJSON CodeRef where
    toJSON ref = object [
        "file" .= file ref
      , "line" .= line ref
      , "column" .= column ref
      ]

instance ToJSON Affect where
  toJSON affect = let s = start affect
                      e = end affect
                  in object [
                      "file" .= if file s == file e
                                then file s
                                else Filename ""
                    , "start" .= s
                    , "end" .= s
                    ]

encodeResult :: String -> Result -> Value
encodeResult bearname r = object [
    "message" .= message r
  , "origin" .= bearname
  , "debug_msg" .= empty
  , "additional_info" .= empty
  , "severity" .= severity r
  , "affected_code" .= affected r
  ]

encodeResults :: String -> [Result] -> Value
encodeResults bearname rs = object [
    "results" .= map (encodeResult bearname) rs
  ]