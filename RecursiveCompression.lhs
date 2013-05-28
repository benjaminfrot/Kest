*RecursiveCompression.lhs* - A Haskell
implementation of a 'compression' algorithm used to estimate 
Kolmogorov Complexity.

This algorithm *actually* encodes strings, it is not just an estimation, encoded strings can be uniquely decoded

Decoding such a string is fast and is not implemented here as any scripting language is good enough for that.

> module RecursiveCompression(encode,pEncode) where

> import StringUtils
> import qualified Data.Word8 as W
> import qualified Data.ByteString.Char8 as C
> import qualified Data.ByteString as B
> import qualified Data.ByteString.Search as S
> import Data.List.Utils
> import Data.List
> import Control.Parallel.Strategies as PS


> zeroStr :: B.ByteString
> zeroStr = B.pack [W._0]

> oneStr :: B.ByteString
> oneStr = B.pack [W._1]

> bStr :: B.ByteString
> bStr = B.pack [W._B]

`encodeBinary` encodes a binary string in a pretty straightforward way :
	- It counts the number of zeros and ones in the string.
	- If there are less zeros than ones, then the first bit of encoded string is set to 0, otherwise it is set to 1
	- The number of zeros or ones (depending whether #zeros < #ones) is then appended to the encoding.
	- Finally, the rank of this binary string among the set of all binary strings of this length for that many 1s added 
(see StringUtils.lhs for the implementation, function `rankBinary`).

NOTE : The length of the string is not encoded, this is done outside this function, and only if necessary.

> encodeBinary :: B.ByteString -> B.ByteString
> encodeBinary s = symCount `B.append` selfDelimited (toBin $ rankBinary s)
>		where
>			symCount = if nZeros < nOnes then B.cons W._0 (selfDelimited (toBin nZeros)) else B.cons W._1 (selfDelimited (toBin nOnes))
>			nZeros = toInteger (B.count W._0 s)
>			nOnes = (toInteger $ B.length s ) - nZeros


`encodeNAry` is a function that takes a "pattern", *p* say, and a string *s*. All the occurences of *p* in *s* are replaced
by ones. The other bits of the string are replaced by zeros. This gives a binary string which can be encoded using `encodeBinary`.
The Boolean *b* given as parameter is here only tell whether the algorithm reached the bottom of the recursion stack or not.
 
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

`encodeT` is the most important function for algorithm. Here is an idea of how it works:
	- Take an integer *t* and a string *s* (of length *n*, say). 
	- Find all the substrings of length *t* of *s* (there are *n-t* such substrings).
	- For any possible substring *ss*, Use `encodeNAry` to encode the positions of the occurences of *ss* in *s*.
	- Cut *ss* out of *s* so that we end up with a shorter string *s'*. 
	- There are two solutions : 
		- a) encode *s'* as simple binary string using `encodeBinary`
		- b) repeat the same operation on *s'*, trying all possible values of *t*.
	- Return the best encoding, *i.e.* the shortest one.

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
>								else -- Else try to encode the left over
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

`encode` : call `encodeT` trying all possible values of *t*, keep the best.  

> encode :: Integer -> Integer -> B.ByteString -> B.ByteString
> encode rDepth mt s = minimumBy ordBS encodings
>	where
>		n = toInteger $ B.length s
>		mt' = if mt < 0 then n else mt
>		encodings = map (\t -> (selfDelimited(toBin (n-t)) `B.append` encodeT 0 rDepth t mt' s)) [mt',mt'-1..1] 


`pEncode` : same as `encode` but in parallel. Different values of *t* are tested at the same time.

> pEncode :: Integer -> Integer -> B.ByteString -> B.ByteString
> pEncode rDepth mt s = minimumBy ordBS encodings
>	where
>		n = toInteger $ B.length s
>		mt' = if mt < 0 then n else mt
>		bs = map (\t -> (selfDelimited(toBin (n-t)) `B.append` encodeT 0 rDepth t mt' s)) [mt',mt'-1..1] 
>		encodings = bs `PS.using` PS.parList PS.rdeepseq
