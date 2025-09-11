module Main (main) where

import           Control.DeepSeq (NFData)
import           Criterion.Main (bench, bgroup, defaultMain, env, whnf)
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable, typeRep)
import           Data.Word (Word64)
import           System.Random

import qualified Data.BloomFilter.Blocked as B.Blocked
import qualified Data.BloomFilter.Classic as B.Classic
import qualified Data.BloomFilter.Hash as B

main :: IO ()
main =
  defaultMain $
    [ bgroup "construct"
      [ env newStdGen $ \g0 ->
        bench (nameBloom ++ "/" ++ sizeName) $
          whnf (constructBloom nelems fpr) g0
      | SomeBenchBloomImpl BenchBloomImpl {..} <- benchBloomImpls
      , (sizeName, nelems, fpr) <- sizes
      ]
    ]
  where
    sizes =
      [ ("1M, fpr=1%",       1_000_000, 0.05)  -- 5.0%
      , ("10M, fpr=0.1%",   10_000_000, 0.005) -- 0.5%
      , ("100M, fpr=0.1%", 100_000_000, 0.001) -- 0.1%
      ]

--
-- Implementations interface
--

data SomeBenchBloomImpl where
     SomeBenchBloomImpl ::
       NFData (b k) => BenchBloomImpl b k -> SomeBenchBloomImpl

data BenchBloomImpl b k =
     BenchBloomImpl {
       nameBloom      :: String,
       constructBloom :: Int -> Double -> StdGen -> b k
     }

benchBloomImpls :: [SomeBenchBloomImpl]
benchBloomImpls =
  [ SomeBenchBloomImpl (benchBloomImpl_classic (Proxy :: Proxy Word64))
  , SomeBenchBloomImpl (benchBloomImpl_blocked (Proxy :: Proxy Word64))
  ]

benchBloomImpl_classic :: (B.Hashable a, Uniform a, Typeable a)
                       => Proxy a -> BenchBloomImpl B.Classic.Bloom a
benchBloomImpl_classic proxy =
    BenchBloomImpl {
      nameBloom      = "bloomfilter-blocked:Data.BloomFilter.Classic@"
                    ++ show (typeRep proxy),
      constructBloom = constructBloom_classic
    }

benchBloomImpl_blocked :: (B.Hashable a, Uniform a, Typeable a)
                       => Proxy a -> BenchBloomImpl B.Blocked.Bloom a
benchBloomImpl_blocked proxy =
    BenchBloomImpl {
      nameBloom      = "bloomfilter-blocked:Data.BloomFilter.Blocked@"
                    ++ show (typeRep proxy),
      constructBloom = constructBloom_blocked
    }

{-# SPECIALISE benchBloomImpl_classic  :: Proxy Word64   -> BenchBloomImpl B.Classic.Bloom Word64 #-}
{-# SPECIALISE benchBloomImpl_blocked  :: Proxy Word64   -> BenchBloomImpl B.Blocked.Bloom Word64 #-}

--
-- Construction
--

constructBloom_classic :: (B.Hashable a, Uniform a)
                       => Int -> Double -> StdGen -> B.Classic.Bloom a
constructBloom_classic !n !fpr !g0 =
  let (!salt, !g1) = uniform g0 in
    B.Classic.unfold (B.Classic.sizeForFPR fpr n) salt (nextElement n) (g1, 0)

constructBloom_blocked :: (B.Hashable a, Uniform a)
                       => Int -> Double -> StdGen -> B.Blocked.Bloom a
constructBloom_blocked !n !fpr !g0 =
  let (!salt, !g1) = uniform g0 in
    B.Blocked.unfold (B.Blocked.sizeForFPR fpr n) salt (nextElement n) (g1, 0)

{-# SPECIALISE constructBloom_classic  :: Int -> Double -> StdGen -> B.Classic.Bloom Word64 #-}
{-# SPECIALISE constructBloom_blocked  :: Int -> Double -> StdGen -> B.Blocked.Bloom Word64 #-}

--
-- Utils
--

{-# INLINE nextElement #-}
nextElement :: Uniform a => Int -> (StdGen, Int) -> Maybe (a, (StdGen, Int))
nextElement !n (!g, !i)
  | i >= n    = Nothing
  | otherwise = Just (x, (g', i+1))
    where
      (!x, !g') = uniform g

