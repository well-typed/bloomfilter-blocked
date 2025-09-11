module Main (main) where

import           Control.DeepSeq (NFData)
import           Criterion.Main (bench, bgroup, defaultMain, env, whnf)
import           Data.ByteString (ByteString)
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable, typeRep)
import           Data.Word (Word64)
import           GHC.Conc (pseq)
import           System.Random
import           System.Random.Stateful

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

    , bgroup "lookup-positive"
      [ env newStdGen $ \g0 ->
        env (pure $ constructBloom nelems fpr g0) $ \b ->
        bench (nameBloom ++ "/" ++ sizeName) $
          whnf (lookupBloom b 100_000) g0
      | SomeBenchBloomImpl BenchBloomImpl {..} <- benchBloomImpls
      , (sizeName, nelems, fpr) <- sizes
      ]

    , bgroup "lookup-negative"
      [ env newStdGen $ \g0 ->
        env newStdGen $ \g1 ->
        env (pure $ constructBloom nelems fpr g0) $ \b ->
        bench (nameBloom ++ "/" ++ sizeName) $
          whnf (lookupBloom b 100_000) g1
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
       constructBloom :: Int -> Double -> StdGen -> b k,
       lookupBloom    :: b k -> Int -> StdGen -> ()
     }

benchBloomImpls :: [SomeBenchBloomImpl]
benchBloomImpls =
  [ SomeBenchBloomImpl (benchBloomImpl_classic (Proxy :: Proxy Word64))
  , SomeBenchBloomImpl (benchBloomImpl_blocked (Proxy :: Proxy Word64))
  , SomeBenchBloomImpl (benchBloomImpl_classic (Proxy :: Proxy Bytes100))
  , SomeBenchBloomImpl (benchBloomImpl_blocked (Proxy :: Proxy Bytes100))
  ]

benchBloomImpl_classic :: (B.Hashable a, Uniform a, Typeable a)
                       => Proxy a -> BenchBloomImpl B.Classic.Bloom a
benchBloomImpl_classic proxy =
    BenchBloomImpl {
      nameBloom      = "bloomfilter-blocked:Data.BloomFilter.Classic@"
                    ++ show (typeRep proxy),
      constructBloom = constructBloom_classic,
      lookupBloom    = lookupBloom_classic
    }

benchBloomImpl_blocked :: (B.Hashable a, Uniform a, Typeable a)
                       => Proxy a -> BenchBloomImpl B.Blocked.Bloom a
benchBloomImpl_blocked proxy =
    BenchBloomImpl {
      nameBloom      = "bloomfilter-blocked:Data.BloomFilter.Blocked@"
                    ++ show (typeRep proxy),
      constructBloom = constructBloom_blocked,
      lookupBloom    = lookupBloom_blocked
    }

{-# SPECIALISE benchBloomImpl_classic  :: Proxy Word64   -> BenchBloomImpl B.Classic.Bloom Word64 #-}
{-# SPECIALISE benchBloomImpl_classic  :: Proxy Bytes100 -> BenchBloomImpl B.Classic.Bloom Bytes100 #-}
{-# SPECIALISE benchBloomImpl_blocked  :: Proxy Word64   -> BenchBloomImpl B.Blocked.Bloom Word64 #-}
{-# SPECIALISE benchBloomImpl_blocked  :: Proxy Bytes100 -> BenchBloomImpl B.Blocked.Bloom Bytes100 #-}

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
{-# SPECIALISE constructBloom_classic  :: Int -> Double -> StdGen -> B.Classic.Bloom Bytes100 #-}
{-# SPECIALISE constructBloom_blocked  :: Int -> Double -> StdGen -> B.Blocked.Bloom Word64 #-}
{-# SPECIALISE constructBloom_blocked  :: Int -> Double -> StdGen -> B.Blocked.Bloom Bytes100 #-}

--
-- Lookup
--

lookupBloom_classic :: (B.Hashable a, Uniform a)
                    => B.Classic.Bloom a -> Int -> StdGen -> ()
lookupBloom_classic !b !n !g0 = go g0 0
  where
    go !g !i =
      case nextElement n (g, i) of
        Nothing            -> ()
        Just (x, (g', i')) -> B.Classic.elem x b `pseq` go g' i'

lookupBloom_blocked :: (B.Hashable a, Uniform a)
                    => B.Blocked.Bloom a -> Int -> StdGen -> ()
lookupBloom_blocked !b !n !g0 = go g0 0
  where
    go !g !i =
      case nextElement n (g, i) of
        Nothing            -> ()
        Just (x, (g', i')) -> B.Blocked.elem x b `pseq` go g' i'


{-# SPECIALISE lookupBloom_classic  :: B.Classic.Bloom Word64   -> Int -> StdGen -> () #-}
{-# SPECIALISE lookupBloom_classic  :: B.Classic.Bloom Bytes100 -> Int -> StdGen -> () #-}
{-# SPECIALISE lookupBloom_blocked  :: B.Blocked.Bloom Word64   -> Int -> StdGen -> () #-}
{-# SPECIALISE lookupBloom_blocked  :: B.Blocked.Bloom Bytes100 -> Int -> StdGen -> () #-}

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

-- | 100 long strings of bytes, used as Bloom filter keys
newtype Bytes100 = Bytes100 ByteString
  deriving newtype B.Hashable

instance Uniform Bytes100 where
  uniformM g = Bytes100 <$> uniformByteStringM 100 g

