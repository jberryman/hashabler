{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric,StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Criterion.Main
import Data.Word
import Data.Typeable
import GHC.Generics
import Data.Data
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.DeepSeq

import Data.Hashabler
import qualified Data.Hashable as Their


instance NFData FNV32 where rnf = rnf . fnv32 
instance NFData (Hash32 a) where rnf = rnf . hashWord32
instance NFData (Hash64 a) where rnf = rnf . hashWord64




data MediumADT
  = UV T.Text T.Text DT (Maybe Dyn)
  | PV T.Text T.Text DT Dfl Bool Bool
  | EV T.Text DT
  deriving (Eq, Data, Typeable, Generic)

data DT = DTn | DTb | DTs | DTdt | DTda | DTtd | DTzo | DTl | DTph | DTu
          deriving (Eq, Data, Typeable, Generic)
data Dfl = Dfla Dyn | Dflb | Dflc | Dfld
  deriving (Eq, Show, Typeable, Data, Generic)

data Dyn = Tn Integer | Tb Bool | Ts T.Text | Tl [T.Text]
  deriving (Eq, Typeable, Show, Data, Generic)

instance Their.Hashable MediumADT
instance Their.Hashable Dyn
instance Their.Hashable Dfl
instance Their.Hashable DT


-- NOTE: both eta-fiddling and inline pragma seem crucial for performance.
instance Hashable MediumADT where
  {-# INLINE hash #-}
  hash h = \x-> case x of
    (UV a b c d) -> mixConstructor 0 (h `hash` a `hash` b `hash` c `hash` d)
    (PV a b c d e f) -> mixConstructor 1 (h `hash` a `hash` b `hash` c `hash` d `hash` e `hash` f)
    (EV a b) -> mixConstructor 2 (h `hash` a `hash` b)
instance Hashable Dyn where
  {-# INLINE hash #-}
  hash h = \x-> case x of
    (Tn a) -> mixConstructor 0 (h `hash` a)
    (Tb a) -> mixConstructor 1 (h `hash` a)
    (Ts a) -> mixConstructor 2 (h `hash` a)
    (Tl a) -> mixConstructor 3 (h `hash` a)
instance Hashable Dfl where
  {-# INLINE hash #-}
  hash h = \x-> case x of
    (Dfla d) -> mixConstructor 0 (h `hash` d)
    Dflb -> mixConstructor 1 h 
    Dflc -> mixConstructor 2 h 
    Dfld -> mixConstructor 3 h
instance Hashable DT where
  {-# INLINE hash #-}
  hash h = \x-> case x of
    DTn -> mixConstructor 0 h 
    DTb -> mixConstructor 1 h 
    DTs -> mixConstructor 2 h 
    DTdt -> mixConstructor 3 h 
    DTda -> mixConstructor 4 h 
    DTtd -> mixConstructor 5 h 
    DTzo -> mixConstructor 6 h 
    DTl -> mixConstructor 7 h 
    DTph -> mixConstructor 8 h 
    DTu -> mixConstructor 9 h

mediumADT :: MediumADT
mediumADT = PV "california" "stars" DTdt (Dfla (Ts "hello")) True False

data SmallADT = SmallADT T.Text (Maybe Int)
  deriving (Eq, Data, Typeable, Generic)
instance Their.Hashable SmallADT

instance Hashable SmallADT where
  {-# INLINE hash #-}
  hash h = \(SmallADT a b) -> h `hash` a `hash` b

smallADT :: SmallADT
smallADT = SmallADT "foobar" (Just 17)


{-
-- same but to compare generic derived instances:
data MediumADT_g
  = UV_g T.Text T.Text DT (Maybe Dyn)
  | PV_g T.Text T.Text DT Dfl Bool Bool
  | EV_g T.Text DT

data DT_g = DTn_g | DTb_g | DTs_g | DTdt_g | DTda_g | DTtd_g | DTzo_g | DTl_g | DTph_g | DTu_g
          deriving (Eq, Data, Typeable, Generic)
data Dfl_g = Dfla_g Dyn_g | Dflb_g | Dflc_g | Dfld_g
  deriving (Eq, Show, Typeable, Data, Generic)

data Dyn_g = Tn_g Integer | Tb_g Bool | Ts_g T.Text | Tl_g [T.Text]
  deriving (Eq, Typeable, Show, Data, Generic)

mediumADT_g :: MediumADT_g
mediumADT_g = PV_g "california" "stars" DTdt_g (Dfla_g (Ts_g "hello")) True False


data SmallADT_g = SmallADT_g T.Text (Maybe Int)

smallADT_g :: SmallADT_g
smallADT_g = SmallADT_g "foobar" (Just 17)
-}


main :: IO ()
main = do
#  ifdef ASSERTIONS_ON
    error "Sorry, please reconfigure without -finstrumented so that we turn off assertions in library code."
#  endif

      -- custom ADT (small and large, nested; one that looks like crappy JSON) made up of above
      --   also test for hashable generic and TH deriving.
    let !mb = (2::Int)^(20 :: Int)  -- 1 Mb
        blmeg = BL.take (fromIntegral mb) . BL.fromChunks . repeat
        tlmeg = TL.take (fromIntegral mb) . TL.fromChunks . repeat
    defaultMain
      [ bgroup "ADTs" [
          bgroup "small" [
            bench "hashFNV64" $  whnf (hashWord64 . hashFNV64) smallADT
          , bench "siphash64_1_3" $  whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) smallADT
          , bench "hashable" $ whnf Their.hash smallADT
          ],
          bgroup "medium" [
            bench "hashFNV64" $  whnf (hashWord64 . hashFNV64) mediumADT
          , bench "siphash64_1_3" $  whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) mediumADT
          , bench "hashable" $ whnf Their.hash mediumADT
          ]
        ]
      , bgroup "List" [
            bgroup "small" [
                env (return $ take 7 [(1::Int)..]) $ \ ~l -> bgroup "Int" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 7 $ cycle [True, False]) $ \ ~l -> bgroup "Bool" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 7 $ cycle $ [("don't"::T.Text), "be", "ashamed", "or", "disgusted", "with", "yourselves"]) $ \ ~l -> bgroup "Text" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 7 $ cycle [(1::Word8)..]) $ \ ~l -> bgroup "Word8" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 7 $ cycle [(1::Word8)..]) $ \ ~l -> bgroup "Word32" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              ]
          , bgroup "medium" [
                env (return $ take 30 [(1::Int)..]) $ \ ~l -> bgroup "Int" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 30 $ cycle [True, False]) $ \ ~l -> bgroup "Bool" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 30 $ cycle [("don't"::T.Text), "be", "ashamed", "or", "disgusted", "with", "yourselves"]) $ \ ~l -> bgroup "Text" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 30 $ cycle [(1::Word8)..]) $ \ ~l -> bgroup "Word8" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 30 $ cycle [(1::Word8)..]) $ \ ~l -> bgroup "Word32" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              ]
          , bgroup "large" [
                env (return $ take 10000 [(1::Int)..]) $ \ ~l -> bgroup "Int" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 10000 $ cycle [True, False]) $ \ ~l -> bgroup "Bool" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 10000 $ cycle [("don't"::T.Text), "be", "ashamed", "or", "disgusted", "with", "yourselves"]) $ \ ~l -> bgroup "Text" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 10000 $ cycle [(1::Word8)..]) $ \ ~l -> bgroup "Word8" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              , env (return $ take 10000 $ cycle [(1::Word32)..]) $ \ ~l -> bgroup "Word32" 
                 [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) l
                 , bench "hashable" $ whnf Their.hash l
                 ] 
              ]
          ]
      , bgroup "ByteString"
        -- we add some extra benchmarks for special cases that apply to Text etc as well:
        [ bgroup "strict"
          [ env (return $ B8.pack ['\0'..'\4']   ) $ \ ~bs5 ->
            bgroup "5" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs5
             , bench "hashable" $ whnf Their.hash bs5
             ]
          , env (return $ B8.pack ['\0'..'\7']     ) $ \ ~bs8 ->
            bgroup "8" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs8
             , bench "hashable" $ whnf Their.hash bs8
             ]
          , env (return $ B8.pack ['\0'..'\10'] ) $ \ ~bs11 ->
            bgroup "11" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs11
             , bench "hashable" $ whnf Their.hash bs11
             ]
          -- difficult case: possibly Word + 7 bytes:
          , env (return $ B8.pack ['\0'..'\14'] ) $ \ ~bs15 ->
            bgroup "15" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs15
             , bench "hashable" $ whnf Their.hash bs15
             ]
          -- benchmark potential unaligned reads of Words:
          , env (return $ B8.take 32 $ bs128Noinline ) $ \ ~bs32 ->
            bgroup "32 (aligned)" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs32
             , bench "hashable" $ whnf Their.hash bs32
             ]
          , env (return $ B8.take 32 $ B8.drop 1 bs128Noinline ) $ \ ~bs32 ->
            bgroup "32 (off 1)" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs32
             , bench "hashable" $ whnf Their.hash bs32
             ]
          , env (return $ B8.take 32 $ B8.drop 3 bs128Noinline ) $ \ ~bs32 ->
            bgroup "32 (off 3)" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs32
             , bench "hashable" $ whnf Their.hash bs32
             ]

          , env (return $ B8.pack ['\0'..'\39'] ) $ \ ~bs40 ->
            bgroup "40" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs40
             , bench "hashable" $ whnf Their.hash bs40
             ]
          , env (return $ B8.pack ['\0'..'\127'] ) $ \ ~bs128 ->
            bgroup "128" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs128
             , bench "hashable" $ whnf Their.hash bs128
             ]
          , env (return $ B8.pack ['\0'..'\511'] ) $ \ ~bs512 ->
            bgroup "512" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs512
             , bench "hashable" $ whnf Their.hash bs512
             ]
          , env (return $ B8.pack ['\0'..'\999999']        ) $ \ ~bs1Mb ->
            bgroup "2^20" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs1Mb
             , bench "hashable" $ whnf Their.hash bs1Mb
             ]
          ]
        , bgroup "lazy"
            [ env (return $ BL.fromChunks [B8.pack ['\0'..'\4']   ]     ) $ \ ~bl5 ->
              bgroup "5" 
               [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bl5
               , bench "hashable" $ whnf Their.hash bl5
               ]
            , env (return $ BL.fromChunks [B8.pack ['\0'..'\7']     ]) $ \ ~bl8 ->
              bgroup "8" 
               [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bl8
               , bench "hashable" $ whnf Their.hash bl8
               ]
            , env (return $ BL.fromChunks [B8.pack ['\0'..'\10'] ]   ) $ \ ~bl11 ->
              bgroup "11" 
               [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bl11
               , bench "hashable" $ whnf Their.hash bl11
               ]
            , env (return $ BL.fromChunks [B8.pack ['\0'..'\39'] ]) $ \ ~bl40 ->
              bgroup "40" 
               [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bl40
               , bench "hashable" $ whnf Their.hash bl40
               ]
            , env (return $ BL.fromChunks [B8.pack ['\0'..'\127'] ] ) $ \ ~bl128 ->
              bgroup "128" 
               [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bl128
               , bench "hashable" $ whnf Their.hash bl128
               ]
            , env (return $ BL.fromChunks [B8.pack ['\0'..'\511'] ]) $ \ ~bl512 ->
              bgroup "512" 
               [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bl512
               , bench "hashable" $ whnf Their.hash bl512
               ]
            , env (return $ blmeg $ B8.pack ['\0'..'\39'] ) $ \ ~bl1Mb_40 ->
              bgroup "2^20_40" 
               [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bl1Mb_40
               , bench "hashable" $ whnf Their.hash bl1Mb_40
               ]
            , env (return $ blmeg $ B8.pack ['\0'..'\127'] ) $ \ ~bl1Mb_128 ->
              bgroup "2^20_128" 
               [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bl1Mb_128
               , bench "hashable" $ whnf Their.hash bl1Mb_128
               ]
            , env (return $ blmeg $ B8.take 65536 $ B8.pack ['\0'..'\999999']        ) $ \ ~bl1Mb_64k ->
              bgroup "2^20_64k" 
               [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bl1Mb_64k
               , bench "hashable" $ whnf Their.hash bl1Mb_64k
               ]
            ]
        ]
      , bgroup "String"
        [ env (return $ ['\0'..'\4'])   $ \ ~s5 ->
          bgroup "5" 
           [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) s5
           , bench "hashable" $ whnf Their.hash s5
           ]
        , env (return $ ['\0'..'\7']     ) $ \ ~s8 ->
          bgroup "8" 
           [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) s8
           , bench "hashable" $ whnf Their.hash s8
           ]
        , env (return $ ['\0'..'\10']) $ \ ~s11 ->
          bgroup "11" 
           [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) s11
           , bench "hashable" $ whnf Their.hash s11
           ]
        , env (return $ ['\0'..'\39'] ) $ \ ~s40 ->
          bgroup "40" 
           [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) s40
           , bench "hashable" $ whnf Their.hash s40
           ]
        , env (return $ ['\0'..'\127'] ) $ \ ~s128 ->
          bgroup "128" 
           [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) s128
           , bench "hashable" $ whnf Their.hash s128
           ]
        , env (return $ ['\0'..'\511']) $ \ ~s512 ->
          bgroup "512" 
           [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) s512
           , bench "hashable" $ whnf Their.hash s512
           ]
        , env (return $ ['\0'..'\999999']) $ \ ~s1Mb ->
          bgroup "2^20" 
           [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) s1Mb
           , bench "hashable" $ whnf Their.hash s1Mb
           ]
        ]
      , bgroup "Text"
        [ bgroup "strict"
          [ env (return $ T.pack ['\0'..'\4']   ) $ \ ~t5 ->
            bgroup "5" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t5
             , bench "hashable" $ whnf Their.hash t5
             ]
          , env (return $ T.pack ['\0'..'\7']     ) $ \ ~t8 ->
            bgroup "8" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t8
             , bench "hashable" $ whnf Their.hash t8
             ]
          , env (return $ T.pack ['\0'..'\10']) $ \ ~t11 ->
            bgroup "11" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t11
             , bench "hashable" $ whnf Their.hash t11
             ]
          , env (return $ T.pack ['\0'..'\39'] ) $ \ ~t40 ->
            bgroup "40" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t40
             , bench "hashable" $ whnf Their.hash t40
             ]
          , env (return $ T.pack ['\0'..'\127'] ) $ \ ~t128 ->
            bgroup "128" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t128
             , bench "hashable" $ whnf Their.hash t128
             ]
          , env (return $ T.pack ['\0'..'\511']) $ \ ~t512 ->
            bgroup "512" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t512
             , bench "hashable" $ whnf Their.hash t512
             ]
          , env (return $ T.pack ['\0'..'\999999']) $ \ ~t1Mb ->
            bgroup "2^20" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t1Mb
             , bench "hashable" $ whnf Their.hash t1Mb
             ]
          ]
        , bgroup "lazy"
          [ env (return $ TL.fromStrict $ T.pack ['\0'..'\4']  ) $ \ ~tl5 ->
            bgroup "5" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) tl5
             , bench "hashable" $ whnf Their.hash tl5
             ]
          , env (return $ TL.fromStrict $ T.pack ['\0'..'\7']    ) $ \ ~tl8 ->
            bgroup "8" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) tl8
             , bench "hashable" $ whnf Their.hash tl8
             ]
          , env (return $ TL.fromStrict $ T.pack ['\0'..'\10']) $ \ ~tl11 ->
            bgroup "11" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) tl11
             , bench "hashable" $ whnf Their.hash tl11
             ]
          , env (return $ TL.fromStrict $ T.pack ['\0'..'\39']) $ \ ~tl40 ->
            bgroup "40" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) tl40
             , bench "hashable" $ whnf Their.hash tl40
             ]
          , env (return $ TL.fromStrict $ T.pack ['\0'..'\127']) $ \ ~tl128 ->
            bgroup "128" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) tl128
             , bench "hashable" $ whnf Their.hash tl128
             ]
          , env (return $ TL.fromChunks (replicate 4 $ T.pack ['\0'..'\127'])) $ \ ~tl512 ->
            bgroup "512" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) tl512
             , bench "hashable" $ whnf Their.hash tl512
             ]
          , env (return $ tlmeg $ T.pack ['\0'..'\39']) $ \ ~tl1Mb_40 ->
            bgroup "2^20_40" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) tl1Mb_40
             , bench "hashable" $ whnf Their.hash tl1Mb_40
             ]
          , env (return $ tlmeg $ T.pack ['\0'..'\127']) $ \ ~tl1Mb_128 ->
            bgroup "2^20_128" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) tl1Mb_128
             , bench "hashable" $ whnf Their.hash tl1Mb_128
             ]
          , env (return $ tlmeg (T.take 65536 $ T.pack ['\0'..'\999999'])) $ \ ~tl1Mb_64k ->
            bgroup "2^20_64k" 
             [ bench "siphash64_1_3" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) tl1Mb_64k
             , bench "hashable" $ whnf Their.hash tl1Mb_64k
             ]
          ]
        ]
      ]

-- try to make sure no funny business happens and we can check different offsets
bs128Noinline :: B8.ByteString
{-# NOINLINE bs128Noinline #-}
bs128Noinline = B8.pack ['\0'..'\127']
