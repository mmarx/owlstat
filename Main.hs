{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Main
Copyright   :  (c) 2014, 2015 Maximilian Marx
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

import Control.Monad ( void
                     , when
                     )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource ( ResourceT
                                    , runResourceT
                                    )
import Data.Conduit ( ($$)
                    , (=$)
                    , await
                    , Sink
                    )
import Data.Conduit.Text ( decode
                         , utf8
                         , lines
                         )
import qualified Data.Conduit.List as CL
import Data.Conduit.Combinators ( sourceDirectoryDeep
                                , sourceFile
                                )
import Data.HashMap.Strict ( HashMap
                           , elems
                           , empty
                           , fromList
                           , lookupDefault
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
needles = expand [ ("Datatypes", [ "DataIntersectionOf"
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
                 , ("Datatypes-EL", [ "rdf:PlainLiteral"
                                    , "rdf:XMLLiteral"
                                    , "rdfs:Literal"
                                    , "owl:real"
                                    , "owl:rational"
                                    , "xsd:decimal"
                                    , "xsd:integer"
                                    , "xsd:nonNegativeInteger"
                                    , "xsd:string"
                                    , "xsd:normalizedString"
                                    , "xsd:token"
                                    , "xsd:Name"
                                    , "xsd:NCName"
                                    , "xsd:NMTOKEN"
                                    , "xsd:hexBinary"
                                    , "xsd:base64Binary"
                                    , "xsd:anyURI"
                                    , "xsd:dateTime"
                                    , "xsd:dateTimeStamp"
                                    ])
                 , ("Datatypes-non-EL", [ "xsd:double"
                                        , "xsd:float"
                                        , "xsd:nonPositiveInteger"
                                        , "xsd:positiveInteger"
                                        , "xsd:negativeInteger"
                                        , "xsd:long"
                                        , "xsd:int"
                                        , "xsd:short"
                                        , "xsd:byte"
                                        , "xsd:unsignedLong"
                                        , "xsd:unsignedInt"
                                        , "xsd:unsignedShort"
                                        , "xsd:unsignedByte"
                                        , "xsd:language"
                                        , "xsd:boolean"
                                        ])
                 , ("universal role", ["owl:topObjectProperty"])
                 , ("empty role", ["owl:bottomObjectProperty"])
                 , ("negative role assertions", ["NegativeObjectPropertyAssertion"])
                 , ("negative data assertions", ["NegativeDataPropertyAssertion"])
                 , ("self restrictions", ["ObjectHasSelf"])
                 , ("HasKey", ["HasKey"])
                 ]
  where expand lst = go lst []
        go :: [(Text, [Text])] -> [(Text, [Text])] -> [(Text, [Text])]
        go [] res = res
        go (tn@(tag, ns):tns) res
          | length ns > 1 = go tns $! tn:[(tag <> "/" <> n, [n]) | n <- ns] ++ res
          | otherwise = go tns $! tn:res

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
  when (any (/=0) . elems $ counts) $ print counts
  return [(name, counts)]

statTree :: FilePath -> IO ()
statTree path = case toText path of
  Left p -> error . unpack $ "could not decode path `" <> p <> "'"
  Right p -> do
    TIO.putStrLn $ "traversing `" <> p <> "'"
    isF <- isFile path
    if isF
      then void $ statFile path
      else runResourceT $
           (sourceDirectoryDeep True path $$ CL.foldMapM maybeStatFile)
           >>= printTree
  where maybeStatFile p = case extension p of
          Just "owl" -> lift $ statFile p
          _ -> return []

printTree :: [(Text, Count)] -> ResourceT IO ()
printTree stats = lift $ do
  let stats' = filter (any (/=0) . elems . snd) stats
      cnt = length stats'
  putStrLn $ "Found " <> show cnt <> " interesting ontologies."
  print $ statify stats' empty
  where statify [] m = m
        statify ((_, s):ss) m = statify ss $
                                unionWith (+) m $
                                fromList [ (k, val k s)
                                         | (k, _) <- needles
                                         ]
        val :: Text -> Count -> Int
        val k s = if 0 == lookupDefault 0 k s then 0 else 1

main :: IO ()
main = getArgs >>= mapM_ (statTree . fromText . pack)
