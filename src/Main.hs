module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.Word
import Control.Monad (replicateM)
import Text.Show.Pretty
import System.Environment ( getArgs ) --part of base
import System.Console.GetOpt --part of base
import Control.Applicative

getInt8 = fmap (\x -> fromIntegral x ::Int) getWord8
getInt16 = fmap (\x -> fromIntegral x ::Int) getWord16le
getInt32 = fmap (\x -> fromIntegral x ::Int) getWord32le
getFloat32 =  fmap wordToFloat getWord32be

getPascalString = do
    n <- getInt8
    C.unpack <$> getByteString n

getFloats = do
    n <- getInt32
    floats <- replicateM n getFloat32
    return (n, floats)

{--
16 bit value identifying the format tag (identifies way data is stored, 1 here means no compression (PCM), if otherwise it's some other type of format)
16 bit value describing # of channels (1 means mono, 2 means stereo)
32 bit value describing sample rate or number of samples per second (like 44100, 22050, or 11025)
32 bit value describing average # of bytes per second (found by: samplerate*channels*(bitspersample/8)) you probably won't need or use this value
16 bit value describing block alignment (found by: (bitspersample/8)*channels) you probably won't need or use this value either
16 bit value describing bits per sample (8bit or 16bit sound)
--}

--Just up to sample rate
getHeaderInfo = do
  riff <- getByteString 4
  size1 <- getInt32
  wave <- getByteString 4
  fmt <- getByteString 4
  fmtSize <- getInt32
  a <- getInt16
  numChans <- getInt16
  srate <- getInt32
  return (riff,size1,wave,fmt,fmtSize,a,numChans,srate)

getSR :: Get Int
getSR = do
  skip (4+4+4+4+4+2+2)
  getInt32 

postInfo g contents = case runGetOrFail g contents of
  Right (restOfFile,_,header) -> putStrLn.ppShow $ header
  Left (_,_,msg) -> putStrLn msg

postSR = postInfo getSR

postHeader file = do
    contents <- B.readFile file
    postInfo getHeaderInfo contents

checkForWaveFile contents = case runGetOrFail getHeaderInfo contents of
  Right (_,_,(_,_,wave,fmt,_,_,_,srate)) -> if (C.unpack wave == "WAVE") && (C.unpack fmt == "fmt ") then Just srate else Nothing
  _ -> Nothing

conversion sr oldf newf = do   
  contents <- B.readFile oldf
  --debug weird headers
  --let (_,_,wave,fmt,_,_,_,_) = runGet getHeaderInfo contents
  --putStrLn $ "wave: " ++ C.unpack wave ++ " fmt: " ++ show (C.unpack fmt) ++ (show (C.unpack wave == "WAVE")) ++ (show (C.unpack fmt == "fmt "))
  case checkForWaveFile contents of
   Just oldsr -> do
     putStrLn $ "Changing sample rate in header of wave file from " ++show oldsr ++"Hz to "++show sr++"Hz."
     let srPos = 4+4+4+4+4+2+2 --skip header until samplerate
         newSr = runPut $ putWord32le (fromIntegral sr::Word32) --construct new samplerate
         newContents = B.take srPos contents `B.append` newSr `B.append` B.drop (srPos+4) contents --assemble new file
     putStrLn "working..."
     B.writeFile newf newContents
     putStrLn "done"
   Nothing -> putStrLn $ "error: file "++oldf++" is not a normal wave file."
  
main :: IO ()
main = do
  args <- getArgs
  case args of
   _:oldf:newf:_ | oldf == newf -> putStrLn "usage: srhack sr file newfile\nfile must be different from newfile"
   newsr:oldf:newf:_  -> do
     let sr = read newsr::Int
     conversion sr oldf newf
   _ -> putStrLn $ unlines [
     "srhack: change samplerate in header of wave file\n",
     "usage: srhack sr file newfile",
     "  sr: new sample rate",
     "  file: file path",
     "  newfile file path of converted file\n",
     "only works with standard WAVE files"
     ]
