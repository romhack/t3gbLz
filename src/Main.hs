module Main where
import           Data.Binary.Get             (Get, getWord8, runGetOrFail)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as Bs
import           Data.ByteString.Lazy.Search (breakOn, strictify)
import           Data.Word                   (Word8)
import           Data.Bits                   (shiftL, shiftR, testBit, (.&.))
import           Data.Int                    (Int64)
import           Data.List                   (foldl', isSuffixOf)
import           Data.List.Split             (chunksOf)
import           Numeric                     (showHex)
import           System.Environment          (getArgs)
import           Text.Printf                 (printf)

data LzEntry = Liter Word8 | Refer {len :: Int, dist :: Int}

instance Show LzEntry where
  show (Liter b) = " Liter 0x"++ showHex b ""
  show (Refer l d)  =  " Refer {len = 0x" ++ showHex l "" ++ " dist = 0x" ++ showHex d "" ++ "}"

windowSize :: Int
windowSize = 0x3FF --ring buffer is 0x400 bytes long
matchLenMax :: Int
matchLenMax = 0x22 --length of match can be encoded by 5 bits + 3 minimal bytes


main :: IO()
main = getArgs >>= parse
  where
    parse ["-v"] = putStrLn "t3gbLz v0.1\nLZSS compression tool \
      \for 'TMNT III - Radical Rescue' game."
    parse ["-d", inFileName, offs, size, outFileName] =
      decompress inFileName (read offs) (read size) outFileName
    parse ["-c", inFileName, outFileName] = compress inFileName outFileName
    parse _ = putStrLn "Usage:\n\
      \  t3gbLz -d <inFile> <offset> <outSize> <outFile>  Decompress block from given ROM file.\n\
      \  t3gbLz -c <inFile> <outFile> Compress given plain block.\n\
      \Options:\n\
      \  -h     Show this screen.\n\
      \  -v     Show version."


--------------------------DECOMPRESS--------------------------------------------
decompress :: String -> Int64 -> Int -> String ->  IO ()
decompress inFileName offs unpackSize outFileName = do
  input <- Bs.readFile inFileName
  let
    binaryTail = Bs.drop offs input
    getBlock = runGetOrFail (getLzEntries unpackSize) binaryTail
  case getBlock of
    Left (_, _, errStr) -> error errStr
    Right (_, position, entries) -> do
      let output = decodeLz entries
      --mapM_ print entries --debug
      Bs.writeFile  outFileName $ Bs.pack output
      putStrLn $ printf "Compressed block size was 0x%X" position


getLzEntries :: Int -> Get [LzEntry]
getLzEntries unpackSize = do
  let
    go :: Int -> [Bool] -> Get [LzEntry]
    go count [] = do --read flag byte as Bools from low to high order
      flagByte <- getWord8
      go count $ map (testBit flagByte) [0..7]

    go count flags
      | count >= unpackSize = return [] --dst size reached, unpack end
      | head flags = --flag is set - raw literal
        do byte <- getWord8
           rest <- go (count + 1) (tail flags)
           return (Liter byte : rest)
      | otherwise = --flag not set - lz reference
        do distLo <- getWord8
           mixedByte <- getWord8
           let l = fromIntegral (mixedByte .&. 0x1F) + 3 --minimal effective lenghth is 3
               d =  (fromIntegral (mixedByte .&. 0x60) `shiftL` 3) + fromIntegral distLo
               offsetD = if d >= 0x3DE then d - 0x3DE else d + matchLenMax
               --unpack starts at 3de and wraps at 400
           rest <- go (count + l) (tail flags)
           return (Refer l offsetD : rest)
  go 0 []

decodeLz :: [LzEntry] -> [Word8] --decode given LzEntries
decodeLz  =  go []
  where
    go buffer [] = buffer
    go buffer (Liter b:es) = go (buffer ++ [b]) es
    go buffer (Refer l d:es)
      | d >= length buffer = go (buffer ++ zeroChunk) (Refer restLen 0 : es)--if we read beyond cur pos, we read zeroes from inited memory and then ring back to start of buffer
      | otherwise = go (buffer ++ refChunk) es
      where
        zeroChunk = take l (replicate cutLen 0)
        cutLen = windowSize + 1 - d --the rest of inited memory till the end of buffer
        restLen = l - cutLen
        refChunk = take l . cycle' $ drop d buffer --usual lz copy
--infinite cycle list. If length>distance, we will read data, already decoded in this reference
        cycle' xs = if null xs then xs else cycle xs --cycle, that works with empty lists

--------------------------COMPRESS----------------------------------------------

compress :: String -> String -> IO ()
compress inFileName outFileName = do
  input <- Bs.readFile inFileName
  let
    entries = encodeLz $ Bs.unpack input
    output = serializeLzEntries entries
  Bs.writeFile outFileName $ Bs.pack output

--Match is found in 2 passes: first search for match in passed buffer window (InBufferMatch),
--then we check again if full needle is circular list with a period of found match length (OutBufferMatch)
--so we could encode with len > dist
--Then we skip 1 byte and see if we could get better result, compare that and
--emit optimal LzEntry.
encodeLz :: [Word8] -> [LzEntry]
encodeLz inp = encodeLz' inp [] --start with zero buffer
  where
  encodeLz' [] _ = [] -- end of stream
  encodeLz' input@(i:is) buffer
    | l < 3 || l < l2 = Liter i : encodeLz' is (buffer ++ [i])--skip one literal if refer too short or ther's longer refer ahead
    | otherwise =  Refer l d : encodeLz' (drop l input) (buffer ++ take l input)
    where (l, d)   = findLzMatch input buffer
          (l2, _) = findLzMatch is (buffer ++ [i]) --search for a longer match at the next input byte (LZ non greedy string parse)
          findLzMatch needle haystack = max (findInBufferMatch (Bs.pack needle) (Bs.pack haystack)) (findOutBufferMatch needle haystack)

--get latest index of  length common chunk. Returns length - distance tuple. (0, 0) means not found.
--ByteStrings are used for speed of 'indices' string search algorithm
findInBufferMatch :: Bs.ByteString -> Bs.ByteString -> (Int, Int)
findInBufferMatch needle' haystack' = go n haystack'
  where
    n = strictify $ Bs.take (fromIntegral matchLenMax) needle'
    go :: B.ByteString -> Bs.ByteString -> (Int, Int)
    go needle haystack
      | B.null needle = (0, 0) --match is not found at all
      | Bs.null (snd index)  = go (B.init needle) haystack --full needle not found, cut and search again
      | d < fromIntegral (Bs.length haystack') && B.length needle + d > fromIntegral(Bs.length haystack') = go (B.init needle) haystack --that's outbuffer case
      | otherwise   = (fromIntegral (B.length needle), d)
      where
        index = breakOn needle haystack--indices needle haystack
        d = fromIntegral $ Bs.length $ fst index --first found match in buffer will be returned

--define if needle is a circular list with period of len, then we can encode with length > distance
--find length of haystack suffix, and then check max match between cycle list
--and needle
findOutBufferMatch :: (Eq a) => [a] -> [a] -> (Int, Int)
findOutBufferMatch needle' haystack'
  | length needle > fromIntegral windowSize || null needleMatch = (0, 0) --needle was not found at the end of haystack
  | otherwise = (outBufferLen, d)
  where
    needle = take  matchLenMax needle'
    haystack = take windowSize haystack'
    needleMatch = getSuffix needle
    getSuffix n = if n `isSuffixOf` haystack then n else getSuffix $ init n
    --take while needle is equal to infinite circular array and output length
    outBufferLen = length . takeWhile id  $ zipWith (==) needle $ cycle needleMatch
    d = length haystack - length needleMatch

serializeLzEntries :: [LzEntry] -> [Word8]
serializeLzEntries entries = concatMap serializeFlagBlock $ chunksOf 8 entries
  where
    serializeFlagBlock :: [LzEntry] -> [Word8] --one flag byte and it's data
    serializeFlagBlock es = bitsToNum flags : serializedEntries --prepend flags
      where
        bitsToNum :: (Num a) => [Bool] -> a
        bitsToNum = foldl' (\byte b -> byte*2 + if b then 1 else 0) 0
        (flags, serializedEntries) = foldl' processFlagByte ([], []) es
        processFlagByte (fs, eBytes) (Liter i) = (True:fs, eBytes ++ [i])
        processFlagByte (fs, eBytes) (Refer l d) = (False:fs, eBytes ++ [distLo, mixedByte])
          where
            lenEncoded = (l - 3) .&. 0x1F
            distEncoded = (d + 0x3DE) .&. windowSize
            distLo = fromIntegral $ distEncoded .&. 0xFF
            mixedByte = fromIntegral $ ((distEncoded `shiftR` 3) .&. 0x60) + lenEncoded
