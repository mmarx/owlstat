{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Main
Copyright   :  (c) 2014 Maximilian Marx
License     :  GPL-3
Maintainer  :  Maximilian Marx <mmarx@wh2.tu-dresden.de>
Stability   :  experimental
Portability :  unknown

Generate statistics on the use of exotic constructs in OWL2 ontologies.
-}


module Main (main) where

import Prelude hiding ( FilePath
                      , lines
                      )

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Conduit ( ($$)
                    , (=$)
                    , await
                    , runResourceT
                    , Sink
                    )
import Data.Conduit.Text ( decode
                         , utf8
                         , lines
                         )
import qualified Data.Conduit.List as CL
import Data.Conduit.Filesystem ( sourceFile
                               , traverse
                               )
import Data.HashMap.Strict ( HashMap
                           , elems
                           , fromList
                           , unionWith
                           )
import Data.Monoid ( Monoid (..)
                   , (<>)
                   )
import Data.Text ( Text
                 , pack
                 , split
                 , unpack
                 )
import qualified Data.Text.IO as TIO
import Filesystem (isFile)
import Filesystem.Path.CurrentOS ( extension
                                 , fromText
                                 , toText
                                 , FilePath
                                 )
import System.Environment (getArgs)

needles :: [(Text, [Text])]
needles = [ ("Datatypes", [ "DataIntersectionOf"
                          , "DataUnionOf"
                          , "DataComplementOf"
                          , "DatatypeRestriction"
                          , "DataSomeValuesFrom"
                          , "DataAllValuesFrom"
                          , "DataHasValue"
                          , "DataMinCardinality"
                          , "DataMaxCardinality"
                          , "DataExactCardinality"
                          , "SubDataPropertyOf"
                          , "EquivalentDataProperties"
                          , "DisjointDataProperties"
                          , "DataPropertyDomain"
                          , "DataPropertyRange"
                          , "DataPropertyAssertion"
                          , "DatatypeDefinition"
                          ])
          , ("universal role", ["owl:topObjectProperty"])
          , ("empty role", ["owl:bottomObjectProperty"])
          , ("negative role assertions", ["NegativeObjectPropertyAssertion"])
          , ("negative data assertions", ["NegativeDataPropertyAssertion"])
          , ("self restrictions", ["ObjectHasSelf"])
          , ("HasKey", ["HasKey"])
          ]

type Count = HashMap Text Int

sink :: MonadIO m => Sink Text m Count
sink = go mempty
  where go cnt = do
          mLine <- await
          case mLine of
            Just line -> go $! unionWith (+) cnt $! (countWords $! split isDelim line)
            Nothing -> return $! cnt
        isDelim ' ' = True
        isDelim '\r' = True
        isDelim '\n' = True
        isDelim '(' = True
        isDelim ')' = True
        isDelim _ = False

countWords :: [Text] -> Count
countWords lst = go lst mempty
  where go [] cnt = cnt
        go (w:ws) cnt = go ws $! unionWith (+) (countWord w) cnt

countWord :: Text -> Count
countWord w = fromList $ map (countNeedle w) needles
  where countNeedle word (tag, needles')
          | word `elem` needles' = (tag, 1)
          | otherwise = (tag, 0)

statFile :: FilePath -> IO [(Text, Count)]
statFile path = do
  name <- case toText path of
    Left p -> TIO.putStrLn ("could not decode path `" <> p <> "'") >> return p
    Right p -> TIO.putStrLn ("analyzing `" <> p <> "'") >> return p
  counts <- runResourceT $ sourceFile path
            $$ decode utf8
            =$ lines
            =$ sink
  print counts
  return [(name, counts)]

statTree :: FilePath -> IO ()
statTree path = case toText path of
  Left p -> error . unpack $ "could not decode path `" <> p <> "'"
  Right p -> do
    TIO.putStrLn $ "traversing `" <> p <> "'"
    isF <- isFile path
    if isF
      then void $ statFile path
      else (traverse True path $$ CL.foldMapM maybeStatFile) >>= printTree
  where maybeStatFile p = case extension p of
          Just "owl" -> statFile p
          _ -> return []

printTree :: [(Text, Count)] -> IO ()
printTree stats = do
  let stats' = filter (any (/=0) . elems . snd) stats
      cnt = length stats'
  putStrLn $ "Found " <> show cnt <> " interesting ontologies."
  mapM_ printEntry stats'
  where printEntry = print

main :: IO ()
main = getArgs >>= mapM_ (statTree . fromText . pack)
