RecursiveCompression.lhs - A Haskell
implementation of a 'compression' algorithm used to estimate 
Kolmogorov Complexity.

This algorithm *actually* encodes strings, it is not just an estimation, encoded strings can be uniquely decoded

Decoding such a string is fast and is not implemented here as any scripting language is good enough for that.

> module RecursiveCompression(encode) where

> import StringUtils
> import qualified Data.Word8 as W
> import qualified Data.ByteString.Char8 as C
> import qualified Data.ByteString as B
> import qualified Data.ByteString.Search as S
> import Data.List.Utils
> import Data.List


> zeroStr :: B.ByteString
> zeroStr = B.pack [W._0]

> oneStr :: B.ByteString
> oneStr = B.pack [W._1]

> bStr :: B.ByteString
> bStr = B.pack [W._B]

Simple encoding of a binary string
NOTE : It cannot be used on its own, one would need to encode the
length as well. Here, only the number of zeros or ones is encoded and
the rank of the string.

> encodeBinary :: B.ByteString -> B.ByteString
> encodeBinary s = symCount `B.append` selfDelimited (toBin $ rankBinary s)
>		where
>			symCount = if nZeros < nOnes then B.cons W._0 (selfDelimited (toBin nZeros)) else B.cons W._1 (selfDelimited (toBin nOnes))
>			nZeros = toInteger (B.count W._0 s)
>			nOnes = (toInteger $ B.length s ) - nZeros

EncodeNAry : Encode strings with more than two symbols.
 
> encodeNAry :: Bool -> B.ByteString -> B.ByteString -> B.ByteString
> encodeNAry b p s = 
>		let
>			encP = selfDelimited p
>			e = encodeNAry' p s
>		in
>			case b of 
> 			True -> B.cons W._1 encP `B.append` e
> 			False -> B.cons W._0 encP `B.append` e

> encodeNAry' :: B.ByteString -> B.ByteString -> B.ByteString
> encodeNAry' p s = e
> 	where 
>			s' = toStrict $ S.replace bStr oneStr (toStrict $ S.replace  oneStr zeroStr (toStrict $ S.replace p bStr s))
>			e = encodeBinary s'

encodeT : Encode a string, starting by looking at patterns of length 
t 

> encodeT :: Integer -> Integer -> Integer -> Integer -> B.ByteString -> B.ByteString
> encodeT rDepth mrDepth t mt s = 
>		let 
>			patterns = nub (substrings (fromIntegral t) s)
>			level0 = 
>				let
>					encodeWithPattern p = if t == 1
>						then -- Simply encode the string
>							B.cons W._1 (selfDelimited p) `B.append` encodeBinary s
>						else -- The pattern is a proper substring
>							let
>								e' = encodeNAry True p s
>								s' = toStrict $ S.replace p B.empty s
>							in
>								if B.length s' == 0 then e' else e' `B.append` encodeBinary s'
>					encodings = map encodeWithPattern patterns
>				in
>					minimumBy ordBS encodings
>			higherLevels = 
>				let
>					encodeWithPattern p =
>						let
>							e' = encodeNAry False p s
>							s' = toStrict $ S.replace p B.empty s
>							r = min t (toInteger (B.length s'))
>							encodeDeeper l = e' `B.append` encodeT (rDepth+1) mrDepth l mt s'
>						in
>							if B.length s' == 0 then Nothing
>								else -- Else try encode the left over
>									Just $ minimumBy ordBS (map encodeDeeper [r,r-1..1])
>					encodings = map encodeWithPattern patterns
>				in
>					if t == 1 then Nothing
>						else fmap (minimumBy ordBS) (sequence encodings)
>		in
>			case rDepth > mrDepth of
>				False -> case higherLevels of 
>					Nothing -> level0
>					Just hLs -> if B.length level0 < B.length hLs then level0 else hLs
>				_ -> level0

Encode : Encode_t over all possible values of t

> encode :: Integer -> Integer -> B.ByteString -> B.ByteString
> encode rDepth mt s = minimumBy ordBS encodings
>	where
>		n = toInteger $ B.length s
>		mt' = if mt < 0 then n else mt
>		encodings = map (\t -> (selfDelimited(toBin (n-t)) `B.append` encodeT 0 rDepth t mt' s)) [mt',mt'-1..1] 
