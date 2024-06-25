{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module FilePack where

import Control.Applicative
import Control.Monad (when)
import Data.Bits (shift, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word
import System.Posix.Types (CMode (..), FileMode)
import Text.Read (readEither)

data FileContents
  = StringFileContents String
  | TextFileContents Text
  | ByteStringFileContents ByteString
  deriving (Eq, Read, Show)

data FileData a = FileData
  { fileName :: FilePath,
    fileSize :: Word32,
    filePermissions :: FileMode,
    fileData :: a
  }
  deriving (Eq, Read, Show)

class Encode a where
  encode :: a -> ByteString
  encode = BS.drop 4 . encodeWithSize

  encodeWithSize :: a -> ByteString
  encodeWithSize a =
    let s = encode a
        l = fromIntegral $ BS.length s
     in word32ToByteString l <> s
  {-# MINIMAL encode | encodeWithSize #-}

class Decode a where
  decode :: ByteString -> Either String a

instance Encode ByteString where
  encode = id

instance Decode ByteString where
  decode = Right . id

instance Encode Text where
  encode = encodeUtf8

instance Decode Text where
  decode = Right . decodeUtf8

instance Encode String where
  encode = BC.pack

instance Decode String where
  decode = Right . BC.unpack

word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes word =
  let a = fromIntegral $ 255 .&. word
      b = fromIntegral $ 255 .&. (shift word (-8))
      c = fromIntegral $ 255 .&. (shift word (-16))
      d = fromIntegral $ 255 .&. (shift word (-24))
   in (a, b, c, d)

word32ToByteString :: Word32 -> ByteString
word32ToByteString word =
  let (a, b, c, d) = word32ToBytes word
   in BS.pack [a, b, c, d]

consWord32 :: Word32 -> ByteString -> ByteString
consWord32 word bytestring =
  let packedWord = word32ToByteString word
   in packedWord <> bytestring

instance Encode Word32 where
  encode = word32ToByteString
  encodeWithSize w =
    let (a, b, c, d) = word32ToBytes w
     in BS.pack [4, 0, 0, 0, a, b, c, d]

word32FromBytes :: (Word8, Word8, Word8, Word8) -> Word32
word32FromBytes (a, b, c, d) =
  let a' = fromIntegral a
      b' = shift (fromIntegral b) 8
      c' = shift (fromIntegral c) 16
      d' = shift (fromIntegral d) 24
   in a' .|. b' .|. c' .|. d'

bytestringToWord32 :: ByteString -> Either String Word32
bytestringToWord32 bytestring =
  case BS.unpack bytestring of
    [a, b, c, d] -> Right $ word32FromBytes (a, b, c, d)
    _otherwise ->
      let l = show $ BS.length bytestring
       in Left ("Expecting 4 bytes but got" <> l)

instance Decode Word32 where
  decode = bytestringToWord32

instance Encode FileMode where
  encode (CMode fMode) = encode fMode

instance Decode FileMode where
  decode = fmap CMode . decode

instance (Encode a) => Encode (FileData a) where
  encode FileData {..} =
    encode $
      encodeWithSize fileName
        <> encodeWithSize fileSize
        <> encodeWithSize filePermissions
        <> encodeWithSize fileData

instance (Encode a, Encode b) => Encode (a, b) where
  encode (a, b) =
    encode $ encodeWithSize a <> encodeWithSize b

instance {-# OVERLAPPABLE #-} (Encode a) => Encode [a] where
  encode = encode . foldMap encodeWithSize

data Packable
  = forall a.
  (Encode a) =>
  Packable {getPackable :: FileData a}

instance Encode Packable where
  encode (Packable p) = encode p

newtype FilePack = FilePack [Packable]

instance Encode FilePack where
  encode (FilePack p) = encode p

naiveDecodeWord32 :: ByteString -> Either String (Word32, ByteString)
naiveDecodeWord32 inputString = do
  when (BS.length inputString < 4) $
    Left "Error, not enough data to get the size of the next field"
  let (encodedSizePrefix, rest) = BS.splitAt 4 inputString
  sizePrefix <- fromIntegral <$> bytestringToWord32 encodedSizePrefix
  when (sizePrefix /= 4) $
    Left "the field size of a word should be 4"
  when (BS.length rest < fromIntegral sizePrefix) $
    Left "Not enough data for the next field size"
  let (encodedWord, rest') = BS.splitAt sizePrefix rest
  decodedWord <- decode encodedWord
  pure (decodedWord, rest')

extractBytes :: Int -> ByteString -> Either String (ByteString, ByteString)
extractBytes n byteString = do
  when (BS.length byteString < n) $
    Left $
      "Error, extract bytes needs at least " <> show n <> " bytes"
  pure $ BS.splitAt n byteString

nextSegmentSize :: ByteString -> Either String (Word32, ByteString)
nextSegmentSize byteString = do
  (nextSegmentStr, rest) <- extractBytes 4 byteString
  parsedSegmentSize <- bytestringToWord32 nextSegmentStr
  pure (parsedSegmentSize, rest)

nextSegment :: ByteString -> Either String (ByteString, ByteString)
nextSegment byteString = do
  (segmentSize, rest) <- nextSegmentSize byteString
  extractBytes (fromIntegral segmentSize) rest

newtype FilePackParser a = FilePackParser
  { runParser :: ByteString -> Either String (a, ByteString)
  }

functorHelper :: (a -> b) -> (ByteString -> Either String (a, ByteString)) -> ByteString -> Either String (b, ByteString)
functorHelper changeParsedOutput parseFunction input = do
  (parsedValue, remainder) <- parseFunction input
  pure (changeParsedOutput parsedValue, remainder)

instance Functor FilePackParser where
  fmap f parser = FilePackParser $ \input -> do
    (parsedValue, result) <- runParser parser input
    pure (f parsedValue, result)

instance Applicative FilePackParser where
  pure a = FilePackParser $ \s -> pure (a, s)
  f <*> s = FilePackParser $ \input -> do
    (f', initialRemainder) <- runParser f input
    (a, finalRemainder) <- runParser s initialRemainder
    pure (f' a, finalRemainder)

extractValue :: (Decode a) => FilePackParser a
extractValue = FilePackParser $ \input -> do
  when (BS.length input < 4) $
    Left "Input has less than 4 bytes, we can't get a segment size"

  let (rawSegmentSize, rest) = BS.splitAt 4 input
  segmentSize <- fromIntegral <$> bytestringToWord32 rawSegmentSize

  when (BS.length rest < segmentSize) $
    Left "not enough iinput to parse the next value"

  let (rawSegmentValue, rest') = BS.splitAt segmentSize rest

  case decode rawSegmentValue of
    Left err -> Left err
    Right a -> Right (a, rest')

execParser :: FilePackParser a -> ByteString -> Either String a
execParser parser inputString =
  fst <$> runParser parser inputString

parseCount :: FilePackParser Int
parseCount = FilePackParser $ \input ->
  let countLetters letter =
        BS.length . BC.filter (== letter)
   in if BS.length input < 10
        then Left "Error: not enough input"
        else
          let (toParse, rest) = BS.splitAt 10 input
           in Right (countLetters 'a' toParse, rest)

instance (Decode a, Decode b) => Decode (a, b) where
  decode = execParser $ (,) <$> extractValue <*> extractValue

instance (Decode a) => Decode (FileData a) where
  decode = execParser $ FileData <$> extractValue <*> extractValue <*> extractValue <*> extractValue

instance Alternative FilePackParser where
  empty = FilePackParser $ const (Left "empty perser")
  parserA <|> parserB = FilePackParser $ \s ->
    case runParser parserA s of
      Right val -> Right val
      Left errA -> runParser parserB s

extractOptional :: FilePackParser a -> FilePackParser (Maybe a)
extractOptional p = Just <$> p <|> pure Nothing

parseSome :: FilePackParser a -> FilePackParser [a]
parseSome p = (:) <$> p <*> parseMany p

parseMany :: FilePackParser a -> FilePackParser [a]
parseMany p = parseSome p <|> pure []

instance Monad FilePackParser where
  return = pure
  valParser >>= mkParser = FilePackParser $ \input -> do
    (val, rest) <- runParser valParser input
    runParser (mkParser val) rest

data FilePackImage
  = FilePackPBM Word32 Word32 [Word32]
  | FilePackPGM Word32 Word32 Word32 [Word32]
  deriving (Eq, Show)

instance Encode FilePackImage where
  encode (FilePackPBM width height values) =
    encode $
      encodeWithSize @String "pbm"
        <> encodeWithSize width
        <> encodeWithSize height
        <> encode values
  encode (FilePackPGM width height maxValue values) =
    encode $
      encodeWithSize @String "pgm"
        <> encodeWithSize width
        <> encodeWithSize height
        <> encodeWithSize maxValue
        <> encode values

instance Decode FilePackImage where
  decode = execParser $ do
    tag <- extractValue @String
    case tag of
      "pbm" ->
        FilePackPBM
          <$> extractValue
          <*> extractValue
          <*> many extractValue
      "pgm" ->
        FilePackPGM
          <$> extractValue
          <*> extractValue
          <*> extractValue
          <*> many extractValue
      otherTag ->
        fail $ "unknown image type tag: " <> otherTag

instance MonadFail FilePackParser where
  fail errMsg = FilePackParser (const $ Left errMsg)
