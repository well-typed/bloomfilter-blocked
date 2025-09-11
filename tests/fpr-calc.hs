{-# LANGUAGE ParallelListComp #-}
module Main (main) where

import qualified Data.BloomFilter as B (BitsPerEntry, FPR, Hashable, Salt)
import qualified Data.BloomFilter.Blocked as B.Blocked
import qualified Data.BloomFilter.Classic as B.Classic

import           Control.Parallel.Strategies
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.List (unfoldr)
import           Math.Regression.Simple
import           System.Environment (getArgs)
import           System.Exit (exitSuccess)
import           System.IO
import           System.Random

import           Prelude hiding (elem)

-- | Write out data files used by gnuplot fpr.plot
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering --for progress reporting

    args <- getArgs
    case args of
      ["fpr-vs-bits"] -> main_generateFPRvsBits
      ["fpr-vs-fpr"]  -> main_generateFPRvsFPR
      ["regression"]  -> main_regression
      _   -> do
        putStrLn "Usage: bloomfilter-fpr-calc [fpr-vs-bits|fpr-vs-fpr|regression]"
        exitSuccess

main_regression :: IO ()
main_regression = do
    s <- readFile "plots/fpr-vs-bits.blocked.gnuplot.data"
    let parseLine l = case words l of
          [w_xs_blocked, _, w_ys_blocked_actual] ->
            ( read w_xs_blocked, read w_ys_blocked_actual )
          _ -> error "failed parse"
        xs_blocked, ys_blocked_actual :: [Double]
        (xs_blocked, ys_blocked_actual) = unzip $ fmap parseLine $ lines s

    let regressionData     :: [(Double, Double)]
        regressionData      = zip xs_blocked
                                  (map (negate . log) ys_blocked_actual)
        regressionBitsToFPR = quadraticFit (\(x,y)->(x,y)) regressionData
        regressionFPRToBits = quadraticFit (\(x,y)->(y,x)) regressionData
    putStrLn ""
    putStrLn "Blocked bloom filter quadratic regressions:"
    putStrLn "bits independent, FPR dependent:"
    print regressionBitsToFPR
    putStrLn ""
    putStrLn "FPR independent, bits dependent:"
    print regressionFPRToBits

main_generateFPRvsBits :: IO ()
main_generateFPRvsBits = do
    writeGnuplotDataFile "fpr-vs-bits.classic"
      [ unwords [show bitsperkey, show y1, show y2]
      | (bitsperkey, _) <- xs_classic
      | y1              <- ys_calc   classicBloomImpl xs_classic
      | y2              <- ys_actual classicBloomImpl xs_classic
      ]

    writeGnuplotDataFile "fpr-vs-bits.blocked"
      [ unwords [show bitsperkey, show y1, show y2]
      | (bitsperkey, _) <- xs_blocked
      | y1              <- ys_calc   blockedBloomImpl xs_blocked
      | y2              <- ys_actual blockedBloomImpl xs_blocked
      ]

    putStrLn "Now run:\ngnuplot plots/fpr-vs-bits.gnuplot"
    putStrLn "To generate plots/fpr-vs-bits.png"
  where
    -- x axis values
    xs_classic =
      [ (bitsperkey, g)
      | bitsperkey <- [2,2.3..20]
      , g          <- mkStdGen <$> [1..3]
      ]
      -- We use fewer points for classic, as it's slower and there's less need.

    xs_blocked =
      [ (bitsperkey, g)
      | bitsperkey <- [2,2.2..24]
      , g          <- mkStdGen <$> [1..9]
      ]

    ys_calc :: BloomImpl b p s Int -> [(Double, StdGen)] -> [Double]
    ys_calc BloomImpl{..} xs =
      [ fpr
      | (bitsperkey, _) <- xs
      , let policy = policyForBits bitsperkey
            fpr    = policyFPR policy
      ]

    ys_actual :: BloomImpl b p s Int -> [(Double, StdGen)] -> [Double]
    ys_actual impl@BloomImpl{..} xs =
      withStrategy (parList rseq) -- eval in parallel
      [ fpr
      | (bitsperkey, g) <- xs
      , let policy   = policyForBits bitsperkey
            fpr_est  = policyFPR policy
            nentries = round (1000 * recip fpr_est)
            fpr      = actualFalsePositiveRate impl policy nentries g
      ]

-- | Similar to main_generateFPRvsBits, but plot requested FPR vs actual FPR
main_generateFPRvsFPR :: IO ()
main_generateFPRvsFPR = do
    writeGnuplotDataFile "fpr-vs-fpr.classic"
      [ unwords [show fpr_req, show fpr_actual]
      | (fpr_req, _) <- xs
      | fpr_actual   <- ys_actual classicBloomImpl
      ]

    writeGnuplotDataFile "fpr-vs-fpr.blocked"
      [ unwords [show fpr_req, show fpr_actual]
      | (fpr_req, _) <- xs
      | fpr_actual   <- ys_actual blockedBloomImpl
      ]

    putStrLn "Now run:\ngnuplot plots/fpr-vs-fpr.gnuplot"
    putStrLn "To generate plots/fpr-vs-fpr.png"
  where
    -- fpr values in the range 1e-1 .. 1e-4
    xs :: [(Double, StdGen)]
    xs = [ (exp (-log_fpr), g)
         | log_fpr <- [2.3,2.4 .. 9.2]
         , g       <- mkStdGen <$> [1..3]
         ]

    ys_actual :: BloomImpl b p s Int -> [Double]
    ys_actual impl@BloomImpl{..} =
      withStrategy (parList rseq) -- eval in parallel
      [ fpr_actual
      | (fpr_req, g) <- xs
      , let policy     = policyForFPR fpr_req
            nentries   = round (1000 * recip fpr_req)
            fpr_actual = actualFalsePositiveRate impl policy nentries g
      ]

actualFalsePositiveRate :: BloomImpl bloom policy size Int
                        -> policy -> Int -> StdGen -> Double
actualFalsePositiveRate bloomimpl policy n g0 =
    fromIntegral (countFalsePositives bloomimpl policy n g0)
  / fromIntegral n

countFalsePositives :: forall bloom policy size.
                       BloomImpl bloom policy size Int
                    -> policy -> Int -> StdGen -> Int
countFalsePositives BloomImpl{..} policy n g0 =
    let (!g01, !g02) = splitGen g0

        -- create a random salt
        (!salt, !g03) = uniform g02

        -- create a bloom filter from n elements from g0
        size  = sizeForPolicy policy n

        xs_b :: bloom Int
        !xs_b = unfold size salt nextElement (g01, 0)

        -- and a set, so we can make sure we don't count true positives
        xs_s :: IntSet
        !xs_s = IntSet.fromList (unfoldr nextElement (g01, 0))

        -- now for a different random sequence (that will mostly not overlap)
        -- count the number of false positives
     in length
          [ ()
          | y <- unfoldr nextElement (g03, 0)
          , y `elem` xs_b                -- Bloom filter reports positive
          , not (y `IntSet.member` xs_s) -- but it is not a true positive
          ]
  where
    nextElement :: (StdGen, Int) -> Maybe (Int, (StdGen, Int))
    nextElement (!g, !i)
      | i >= n    = Nothing
      | otherwise = Just (x, (g', i+1))
        where
          (!x, !g') = uniform g

writeGnuplotDataFile :: FilePath -> [String] -> IO ()
writeGnuplotDataFile name datalines = do
    withFile filename WriteMode $ \h -> do
      hSetBuffering h LineBuffering --for incremental output
      putStrLn ("Writing " ++ filename ++ " ...")
      mapM_ (\l -> hPutStrLn h l >> putChar '.') datalines
    putStrLn "Done"
  where
    filename = "plots/" ++ name ++ ".gnuplot.data"


data BloomImpl bloom policy size a = BloomImpl {
       policyForBits :: B.BitsPerEntry -> policy,
       policyForFPR  :: B.FPR          -> policy,
       policyBits    :: policy -> B.BitsPerEntry,
       policyFPR     :: policy -> B.FPR,
       sizeForPolicy :: policy -> Int -> size,
       unfold        :: forall b. size -> B.Salt -> (b -> Maybe (a, b)) -> b -> bloom a,
       elem          :: a -> bloom a -> Bool
     }

classicBloomImpl :: B.Hashable a
                 => BloomImpl B.Classic.Bloom
                              B.Classic.BloomPolicy B.Classic.BloomSize a
classicBloomImpl =
    BloomImpl {
       policyForBits = B.Classic.policyForBits,
       policyForFPR  = B.Classic.policyForFPR,
       policyBits    = B.Classic.policyBits,
       policyFPR     = B.Classic.policyFPR,
       sizeForPolicy = B.Classic.sizeForPolicy,
       unfold        = B.Classic.unfold,
       elem          = B.Classic.elem
    }

blockedBloomImpl :: B.Hashable a
                 => BloomImpl B.Blocked.Bloom
                              B.Blocked.BloomPolicy B.Blocked.BloomSize a
blockedBloomImpl =
    BloomImpl {
       policyForBits = B.Blocked.policyForBits,
       policyForFPR  = B.Blocked.policyForFPR,
       policyBits    = B.Blocked.policyBits,
       policyFPR     = B.Blocked.policyFPR,
       sizeForPolicy = B.Blocked.sizeForPolicy,
       unfold        = B.Blocked.unfold,
       elem          = B.Blocked.elem
    }
