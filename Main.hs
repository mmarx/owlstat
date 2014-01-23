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
                      , takeWhile
                      , words
                      )

import Control.Applicative ( (<$>)
                           , (<*>)
                           )
import Control.Monad.IO.Class ( MonadIO
                              , liftIO
                              )
import Data.Conduit ( ($$)
                    , (=$)
                    , await
                    , runResourceT
                    , Sink (..)
                    )
import Data.Conduit.Text ( decode
                         , utf8
                         , lines
                         )
import qualified Data.Conduit.List as CL
import Data.Conduit.Filesystem ( sourceFile
                               , traverse
                               )
import Data.Monoid ( Monoid (..)
                   , (<>)
                   , mconcat
                   )
import Data.Text ( Text
                 , pack
                 , takeWhile
                 , unpack
                 , words
                 )
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Filesystem.Path.CurrentOS ( extension
                                 , fromText
                                 , toText
                                 , FilePath
                                 )

needles :: [(Text, [Text])]
needles = [ ("Datatypes", [ "DataIntersectionOf"
                          , "DataUnionOf"
                          , "DataComplementOf"
                          , "DataOneOf"
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
                          , "FunctionalDataProperty"
                          , "DatatypeDefinition"
                          ])
          , ("universal role", ["owl:topObjectProperty"])
          , ("empty role", ["owl:bottomObjectProperty"])
          , ("negative role assertions", ["NegativeObjectPropertyAssertion"])
          , ("negative data assertions", ["NegativeDataPropertyAssertion"])
          , ("self restrictions", ["ObjectHasSelf"])
          , ("HasKey", ["HasKey"])
          ]

newtype Count = Count [(Text, Integer)]
              deriving (Read, Show, Eq)

instance Monoid Count where
  mempty = Count $ map (\(tag, _) -> (tag, 0)) needles
  mappend (Count l) (Count r) = Count $ zipWith sum l r
    where sum (tag, cnt) (_, cnt') = (tag, cnt + cnt')

sink :: MonadIO m => Sink Text m Count
sink = go mempty
  where go cnt = do
          mLine <- await
          case mLine of
            Just line -> go $! cnt <> (countWords $ words line)
            Nothing -> return $! cnt

countWords :: [Text] -> Count
countWords lst = go lst mempty
  where go [] cnt = cnt
        go (w:ws) cnt = go ws $! countWord (takeWhile (/='(') w) <> cnt

countWord :: Text -> Count
countWord w = Count $ map (countNeedle w) needles
  where countNeedle word (tag, needles)
          | word `elem` needles = (tag, 1)
          | otherwise = (tag, 0)

statFile :: FilePath -> IO ()
statFile path = do
  case toText path of
    Left p -> TIO.putStrLn $ "could not decode path `" <> p <> "'"
    Right p -> TIO.putStrLn $ "analyzing `" <> p <> "'"
  counts <- runResourceT $ sourceFile path
            $$ decode utf8
            =$ lines
            =$ sink
  print counts
  return ()

treeStat :: FilePath -> IO ()
treeStat path = case toText path of
  Left p -> error . unpack $ "could not decode path `" <> p <> "'"
  Right p -> do
    TIO.putStrLn $ "traversing `" <> p <> "'"
    traverse True path $$ CL.mapM_ maybeStatFile
  where maybeStatFile path = case extension path of
          Just "owl" -> statFile path
          _ -> return ()

main :: IO ()
main = getArgs >>= mapM_ (treeStat . fromText . pack)
