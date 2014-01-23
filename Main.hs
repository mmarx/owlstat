{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (FilePath)

import Data.Monoid ((<>))
import Data.Text ( Text
                 , pack
                 , unpack
                 )
import System.Environment (getArgs)
import Filesystem.Path.CurrentOS ( fromText
                                 , toText
                                 ,  FilePath
                                 )

needles :: [(Text, [Text])]
needles = [ ("Datatypes", [])
          , ("universal role", ["owl:topObjectProperty"])
          , ("empty role", ["owl:bottomObjectProperty"])
          , ("negative role assertions", [])
          , ("negative data assertions", [])
          , ("self restrictions", ["ObjectHasSelf"])
          , ("HasKey", ["HasKey"])
          ]

treeStat :: FilePath -> IO ()
treeStat path = case toText path of
  Left p -> putStrLn . unpack $ "failed to decode path `" <> p <> "'"
  Right p -> putStrLn . unpack $ "got path `" <> p <> "'"

main :: IO ()
main = getArgs >>= mapM_ (treeStat . fromText . pack)
