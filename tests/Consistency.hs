{-# LANGUAGE RankNTypes , CPP , MagicHash , DeriveDataTypeable, GeneralizedNewtypeDeriving , StandaloneDeriving #-}
module Consistency where

import Control.Applicative
import Data.Maybe
import System.Directory
import Control.Monad
import Data.Hashable.FNV1a
import System.Random


#if MIN_VERSION_base(4,8,0)
#  if MIN_VERSION_integer_gmp(1,0,0)
-- implemented via BigNat, etc here:
import Numeric.Natural
#  endif
#endif
-- #ifdef VERSION_integer_gmp
-- # if MIN_VERSION_integer_gmp(1,0,0)
-- import GHC.Integer.GMP.Internals (BigNat(BN#))
-- # endif
-- #endif
import Data.Word
import Data.Int

-- For ByteString & Text instances:
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Short.Internal as BSh
#endif
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Primitive as P
import Control.Exception(assert, onException)

import Data.Typeable
import Data.Ratio
import System.IO.Unsafe(unsafeDupablePerformIO)



-- Simply for checking that our Hash and Hashable instances are the same across
-- architectures, and across code changes.
--
-- This is all very crappy, sorry.


generatedVectorsDir :: FilePath
generatedVectorsDir = "tests/Vectors/generated/"

outpFilePath :: String -> String -> FilePath
outpFilePath nm hashNm = generatedVectorsDir++nm++".out."++hashNm

inFilePath :: String -> FilePath
inFilePath nm = generatedVectorsDir++nm++".in"



randsIO :: Random a=> IO [a]
randsIO = replicateM 1000 randomIO

-- get `n` lists of random values of Random length between 0 and 1000
randLengths :: Random a=> Int -> IO [[a]]
randLengths n = replicateM n (randomRIO (0,1000)) >>= mapM (`replicateM` randomIO)

-- prepend big and small values:
withEdges :: (Enum a, Bounded a)=> [a] -> [a]
withEdges l = (take 128 [maxBound, pred maxBound ..])++(take 128 [minBound ..])++l

data CustomForTypeable a b c = 
        Foo a
      | Bar a b Bool
      | Baz Bool (Either Ordering a) a b c ()
       deriving Typeable

-- Maybe ill-considered to even store serialized random samples, but I'm not
-- sure that Random is going to give consistent results across platforms, given
-- same seed, and it seems nice to have some concrete random test vectors that
-- can be inspected, altered to check failure, etc.
data HashableTestInputs a b
        -- Random generator of Hashable a, the results of which will be stored
        -- and must be Read/Show:
        = StoredRandom (IO [a])
        -- For small types we can test exhaustively, or where Read/Show is
        -- impossible:
        | JustThese [b]

justThese :: [b] -> HashableTestInputs () b
justThese = JustThese

storedRandom :: IO [a] -> HashableTestInputs a ()
storedRandom = StoredRandom

-- cheezy instance that matches how we generate below:
instance Show P.ByteArray where
    show ba = 
        let s = P.sizeofByteArray ba 
            s8 = fromIntegral s :: Word8
         in assert (s<= (fromIntegral (maxBound :: Word8))) $ 
             "P.pack "++(show [1 .. s8])

-- Enumerate our Show/Read-able Hashable instances for generating/checking:
forHashableInstances :: (forall h h'. (Read h, Show h, Hashable h, Show h', Hashable h')
                          => String -> HashableTestInputs h h' -> IO a) 
                        -> IO [a]
forHashableInstances ioUnhandled = sequence [
    -- the second argument are for generating values, and also for type hints
    -- when checking.
    io "Bool" (justThese [True,False])
  , io "Char" $ storedRandom (withEdges <$> randsIO :: IO [Char])
  , io "Double" $ storedRandom (randsIO :: IO [Double])
  , io "Float" $ storedRandom (randsIO :: IO [Float])
  , io "Int8" (justThese [minBound::Int8 .. maxBound] )
  , io "Int16" $ storedRandom (withEdges <$> randsIO :: IO [Int16])
  , io "Int32" $ storedRandom (withEdges <$> randsIO :: IO [Int32])
  , io "Int64" $ storedRandom (withEdges <$> randsIO :: IO [Int64])
  , io "Word16" $ storedRandom (withEdges <$> randsIO :: IO [Word16])
  , io "Word32" $ storedRandom (withEdges <$> randsIO :: IO [Word32])
  , io "Word64" $ storedRandom (withEdges <$> randsIO :: IO [Word64])
    -- Platform-dependent; only test consistency when within 32-bits:
  , io "Int" $ storedRandom (map fromIntegral <$> (withEdges <$> randsIO :: IO [Int32]) :: IO [Int])
  , io "Word" $ storedRandom (map fromIntegral <$> (withEdges <$> randsIO :: IO [Word32]) :: IO [Word])

    -- Make sure we get different widths > and < sizeOf Word
  , io "Integer" $ storedRandom ((do
        smallInts <- randsIO :: IO [Int]
        return $ ([0,1]++) $ concatMap (\smI-> [smI, smI*2^(30::Int), smI*2^(60::Int), smI*2^(130::Int)] ) $ 
                  [2,3,4]++map fromIntegral smallInts
        ) :: IO [Integer])
  , io "Ordering" (justThese [LT,EQ,GT])
  , io "Word8" (justThese [minBound::Word8 .. maxBound] )
  , io "Unit" (justThese [()])
  , io "B.ByteString" $ storedRandom (map B.pack . ([]:) <$> randLengths 999 :: IO [B.ByteString])
  , io "BL.ByteString" $ justThese $ [ BL.empty, BL.fromChunks $ 
        let wds = iterate (+7) (0::Word8)
         in map (\len-> B.pack (take len wds)) [1,2,3,5,8,13,100,10000] ]
  , io "T.Text" $ storedRandom $ (map T.pack . ("":) <$> randLengths 999 :: IO [T.Text])
  , io "TL.Text" $ justThese $ [ TL.empty, TL.fromChunks $ 
        let cs = iterate succ minBound
         in map (\len-> T.pack (take len cs)) [1,2,3,5,8,13,100,10000] ]
  -- , io "TL.Text" $ storedRandom $ ((forM [1..100] $ \numChunks-> do
  --       charChunks <- randLengths numChunks
  --       return $ TL.fromChunks $ 
  --           map T.pack charChunks
  --       ) :: IO [TL.Text])
  
  -- Or we could write an orphan Read/Show instances, and make more random:
  , io "P.ByteArray" $ justThese $ unsafeDupablePerformIO $
           forM [0..255] $ \s-> do
             aMut <- P.newByteArray s 
             forM_ [0..(s-1)] $ \ix-> P.writeByteArray aMut ix (fromIntegral s :: Word8)
             P.unsafeFreezeByteArray aMut
#if MIN_VERSION_bytestring(0,10,4)
  , io "ShortByteString" $ storedRandom $ (map BSh.pack . ([]:) <$> randLengths 999 :: IO [BSh.ShortByteString])
#endif
#if MIN_VERSION_base(4,8,0)
# ifdef VERSION_integer_gmp
#  if MIN_VERSION_integer_gmp(1,0,0)
  -- TEST THIS VIA Natural FOR NOW; TODO:
  -- , BigNat
  , io "Natural" $ storedRandom $ ((do 
        smallInts <- replicateM 100 (randomRIO (2,maxBound):: IO Int) 
        return $ ([0,1]++) $ concatMap (\smI-> [smI, smI*2^(30::Integer), smI*2^(60::Integer), smI*2^(130::Integer)] ) $
                  [2,3,4]++map fromIntegral smallInts
        ) :: IO [Natural])
#  endif
# endif
#endif

  , io "List_Word8" $ justThese $ 
        let wds = iterate (+7) (0::Word8)
         in map (`take` wds) [1,2,3,5,8,13,100,10000] 
  , io "Ratio_Word8" $ storedRandom $ ((replicateM 1000 $ (%) <$> randomIO <*> (randomRIO (1,maxBound))) :: IO [Ratio Word8])

  -- For now, just on >= 7.2:
#if __GLASGOW_HASKELL__ >= 702
  , io "TypeRep" (justThese [ typeOf True
                         , typeOf (1::Int)
                         , typeOf (Just LT)
                         , typeOf (Left [Just 1] :: Either [Maybe Word8] Bool)
                         , typeOf (Foo True :: CustomForTypeable Bool () (Either (IO Bool) Integer))
                         , typeOf (print ())
                         -- TODO sufficient?
                         ])
#endif
  , io "Maybe_Word8" (justThese (Nothing : map Just [minBound::Word8 .. maxBound])  )
  , io "Either_Word8_Bool" (justThese (Right True : Right False : map Left [minBound::Word8 .. maxBound]) )
  , io "Tuple2_Word8" $ storedRandom $ (replicateM 1000 $ (,) <$> randomIO <*> randomIO :: IO [(Word8,Word8)])
  , io "Tuple3_Word8" $ storedRandom $ (replicateM 1000 $ (,,) <$> randomIO <*> randomIO <*> randomIO :: IO [(Word8,Word8,Word8)])
  , io "Tuple4_Word8" $ storedRandom $ (replicateM 1000 $ (,,,) <$> randomIO <*> randomIO <*> randomIO <*> randomIO :: IO [(Word8,Word8,Word8,Word8)])
  , io "Tuple5_Word8" $ storedRandom $ (replicateM 1000 $ (,,,,) <$> randomIO <*> randomIO <*> randomIO<*> randomIO  <*> randomIO :: IO [(Word8,Word8,Word8,Word8,Word8)])
  , io "Tuple6_Word8" $ storedRandom $ (replicateM 1000 $ (,,,,,) <$> randomIO <*> randomIO <*> randomIO <*> randomIO <*> randomIO <*> randomIO :: IO [(Word8,Word8,Word8,Word8,Word8,Word8)])
  , io "Tuple7_Word8" $ storedRandom $ (replicateM 1000 $ (,,,,,,) <$> randomIO <*> randomIO <*> randomIO <*> randomIO <*> randomIO <*> randomIO <*> randomIO :: IO [(Word8,Word8,Word8,Word8,Word8,Word8,Word8)])
  , io "Tuple8_Word8" $ storedRandom $ (replicateM 1000 $ (,,,,,,,) <$> randomIO <*> randomIO <*> randomIO <*> randomIO <*> randomIO <*> randomIO <*> randomIO <*> randomIO :: IO [(Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)])
  ] where io nm f = ioUnhandled nm f `onException` (putStrLn $ " !!! in forHashableInstances "++nm)

-- forHashFunctions :: (forall a1 h. (Hash h, Hashable a1) =>
--                           String -> (a1 -> h) -> IO a) -> IO [a] -- TODO fix
forHashFunctions io = sequence [
      io "FNV32" hashFNV32
    ]


-- Nothing means regenerate all vectors, else just those in the list of names:
regenerateVectors :: Maybe [String] -> IO ()
regenerateVectors vs = void $ 
  forHashableInstances $ \nm inputs-> do
    let regenerateOuts :: (Hashable h, Show h)=> [h] -> IO ()
        regenerateOuts ins = void $
          forHashFunctions $ \hashNm hashFunc -> do
            writeFile (outpFilePath nm hashNm) $ show $
              map hashFunc ins
    when (maybe True (nm `elem`) vs) $
        case inputs of
             JustThese ins    -> regenerateOuts ins >> putStr "."
             StoredRandom gen -> do
                putStr "o"
                ins <- gen
                let inFile = inFilePath nm
                writeFile inFile $ show ins
                regenerateOuts ins

-- TODO IS INSTANCE FOR LAZY BYTESTRINGS SHITTY AND SLOW W/ SPAC LEAK?

-- Return a (hopefully empty) list of failed cases:
--                  input type, hash, input, correct output, actual output
checkGeneratedVectors :: IO [(String,String,String,String,String)] 
checkGeneratedVectors = fmap concat $
  forHashableInstances $ \nm inputs-> do
    putStr "."
    let checkAgainstStored :: (Hashable h, Show h)=> [h] -> IO [(String,String,String,String,String)]
        checkAgainstStored ins = fmap concat $ 
            forHashFunctions $ \hashNm hashFunc -> do
              let outFile = outpFilePath nm hashNm 
              outFileExists <- doesFileExist outFile
              if not outFileExists
                then do putStrLn $ "WARNING: Skipping, as no output file found: "
                                  ++outFile
                        return []
                else do
                  outs <- tryRead nm <$> readFile outFile

                  unless (length ins == length outs) $
                    error $ "Inputs and outputs to check don't align, in "
                             ++nm++" / "++hashNm
          
                  return $ catMaybes $
                    let check i o
                          | o == hashFunc i = Nothing
                          | otherwise = Just ( nm, hashNm, show i
                                             , show o, show $ hashFunc i)
                     in zipWith check ins outs
    case inputs of
       JustThese ins    -> checkAgainstStored ins
       StoredRandom gen -> do
           let inFile = inFilePath nm
           inFileExists <- doesFileExist inFile
           if not inFileExists
             then do putStrLn $ "WARNING: no input file found for type: "++nm
                     return []
             else do ins <- (tryRead nm <$> readFile inFile) `asTypeOf` gen
                     when (null ins) $
                       error "We don't seem to have any test vectors here!!"
                     checkAgainstStored ins

tryRead :: Read a => String -> String -> a
tryRead nm = maybeErr . fmap fst . listToMaybe . reads where
    maybeErr = fromMaybe (error $ "Error parsing stored vector for: "++nm) 
