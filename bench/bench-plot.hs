{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TransformListComp #-}
module Main (main) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import           Data.Maybe (maybeToList)
import           Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import           Numeric

import           Control.Monad

import           GHC.Exts (groupWith, sortWith, the)
import           System.Environment (getArgs)

-- A program to take the json output of criterion for the bloomfilter-bench
-- program and convert it into several files in the gnuplot input format.
-- These are then used by the gnuplot charts.
main :: IO ()
main = do
  [benchoutputfile] <- getArgs
  benchoutput <- throwDecode =<< BS.readFile benchoutputfile
  case parse extractMeans benchoutput of
    Error msg -> putStrLn msg
    Success benchdata ->
      sequence_
        [ do let valsByImpl =
                   [ (the implName', vals)
                   | (implName', vals) <- zip implName val
                   , then sortWith by implName'
                   , then group by implName' using groupWith ]

             T.writeFile (T.unpack (the datafilename)) $
               T.unlines
                 [ T.unwords $
                     tshow implName'
                   : map tshow vals
                 | (implName', vals) <- valsByImpl ]

             let renderVals impl1 impl2 vals1 vals2 =
                     the benchName <> " "
                  <> the keyType <> " "
                  <> tshow impl2 <> " vs. "
                  <> tshow impl1 <> " "
                  <> T.intercalate " "
                       (map (\v -> T.pack $
                                     showGFloat (Just 1) (v :: Double) "x")
                            (zipWith (/) (map realToFrac vals1)
                                         (map realToFrac vals2)))
             -- Compare new impls vs original
             mapM_ T.putStrLn $
               [ renderVals impl1 impl2 vals1 vals2
               | (impl1, vals1) <- cycle (take 1 valsByImpl)
               | (impl2, vals2) <- drop 1 valsByImpl
               ]
             -- Compare new impls vs each other
             mapM_ T.putStrLn $
               [ renderVals impl1 impl2 vals1 vals2
               | (impl1, vals1) <- drop 1 valsByImpl
               | (impl2, vals2) <- drop 2 valsByImpl
               ]

        | (benchcasestr, val) <- benchdata
        , BenchCase{..} <- maybeToList (classifyBench benchcasestr)
        , let datafilename :: T.Text
              datafilename =
                benchName <> "-" <> keyType <> ".gnuplot.data"
        , then group by (benchName, keyType) using groupWith
        ]
  where
    tshow :: Show a => a -> T.Text
    tshow = T.pack . show

extractMeans :: Value -> Parser [(T.Text, Scientific)]
extractMeans v = do
  header <- parseJSON v
  bodyarray  <- parseJSON (header V.! 2)
  forM bodyarray $ \report -> do
    name     <- report   .: "reportName"
    analysis <- report   .: "reportAnalysis"
    meanobj  <- analysis .: "anMean"
    mean     <- meanobj  .: "estPoint"
    pure (name, mean)

data BenchCase =
     BenchCase {
       implName  :: ImplName,
       keyType   :: T.Text,
       benchName :: T.Text,
       size      :: T.Text,
       fpr       :: T.Text
     }
  deriving stock Show

classifyBench :: T.Text -> Maybe BenchCase
classifyBench t =
    case T.split splitChar t of
      [benchName,_package,implName,keyType,size,fpr]
        -> Just BenchCase {
                  implName = classifyName implName,
                  fpr  = T.strip fpr,
                  size = T.strip size,
                  ..
                }
      _ -> Nothing
  where
    splitChar c = c == ':' || c == '@' || c == '/' || c == ','

data ImplName = Original | Classic | Blocked
  deriving stock (Eq, Ord, Show)

classifyName :: T.Text -> ImplName
classifyName "Data.BloomFilter"         = Original
classifyName "Data.BloomFilter.Classic" = Classic
classifyName "Data.BloomFilter.Blocked" = Blocked
classifyName _                          = error "classifyName: unexpected name"

