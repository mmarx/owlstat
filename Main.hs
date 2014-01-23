{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (FilePath)

import Data.Conduit ( ($$)
                    , (=$)
                    , runResourceT
                    )
import qualified Data.Conduit.List as CL
import Data.Conduit.Filesystem ( sourceFile
                               , traverse
                               )
import Data.Monoid ((<>))
import Data.Text ( Text
                 , pack
                 , unpack
                 )
import qualified Data.Text.IO as TIO
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

statFile :: FilePath -> IO ()
statFile path = case toText path of
  Left p -> error . unpack $ "could not decode path `" <> p <> "'"
  Right p -> do
    TIO.putStrLn $ "got file `" <> p <> "'"
    return ()

treeStat :: FilePath -> IO ()
treeStat path = case toText path of
  Left p -> error . unpack $ "could not decode path `" <> p <> "'"
  Right p -> do
    TIO.putStrLn $ "traversing `" <> p <> "'"
    traverse True path $$ CL.mapM_ statFile

main :: IO ()
main = getArgs >>= mapM_ (treeStat . fromText . pack)
