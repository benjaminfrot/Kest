*RecursiveCompression.lhs* - A Haskell implementation of a 'compression' algorithm used to estimate 

Kolmogorov Complexity.

This algorithm *actually* encodes strings, it is not just an estimation, encoded strings can be uniquely decoded

Decoding such a string is fast and is not implemented here as any scripting language is good enough for that.

> module RecursiveCompression(encode,pEncode,Parameters(Parameters,leftRDepth,maxLRDepth,rightRDepth,maxRRDepth,mt,topNPatterns)) where

> import StringUtils
> import qualified Data.Word8 as W
> import qualified Data.ByteString.Char8 as C
> import qualified Data.ByteString as B
> import qualified Data.ByteString.Search as S
> import Data.List.Utils
> import Data.List
> import Data.Function
> import Control.Parallel.Strategies as PS

> data Parameters = Parameters {
>		leftRDepth :: Integer -- Current depth of 'left' recursion 
>		,maxLRDepth :: Integer -- Maximum depth of left recursion
>		,rightRDepth :: Integer -- Current depth of right recursion
>		,maxRRDepth :: Integer -- Maximum depth of right recursion
>		,mt :: Integer -- Maximum size of pattern
>		,topNPatterns :: Integer -- Keep only the N most frequent patterns and N least frequent
>	}

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
- Finally, the rank of this binary string among the set of all binary strings of this length for that many 1s added (see StringUtils.lhs for the implementation, function `rankBinary`).

NOTE : The length of the string is not encoded, this is done outside this function, and only if necessary.

> encodeBinary :: B.ByteString -> B.ByteString
> encodeBinary s = symCount `B.append` selfDelimited (toBin $ rankBinary s)
>		where
>			symCount = if nZeros < nOnes then B.cons W._0 (selfDelimited (toBin nZeros)) else B.cons W._1 (selfDelimited (toBin nOnes))
>			nZeros = toInteger (B.count W._0 s)
>			nOnes = (toInteger $ B.length s ) - nZeros

> preEncode :: B.ByteString -> B.ByteString
> preEncode s = symCount
>		where
>			symCount = if nZeros < nOnes then B.cons W._0 (selfDelimited (toBin nZeros)) else B.cons W._1 (selfDelimited (toBin nOnes))
>			nZeros = toInteger (B.count W._0 s)
>			nOnes = (toInteger $ B.length s ) - nZeros


`encodeNAry` is a function that takes a "pattern", *p* say, and a string *s*. All the occurences of *p* in *s* are replaced
by ones. The other bits of the string are replaced by zeros. This gives a binary string which can be encoded using `encodeBinary`.
The Boolean *b* given as parameter is here only tell whether the algorithm reached the bottom of the recursion stack or not.
 
> encodeNAry :: Parameters -> Bool -> B.ByteString -> B.ByteString -> B.ByteString
> encodeNAry params b p s =
>		let
>			encP = selfDelimited p
>			e = encodeNAry' params p s
>		in
>			case b of 
> 			True -> B.cons W._1 encP `B.append` e
> 			False -> B.cons W._0 encP `B.append` e

> encodeNAry' :: Parameters -> B.ByteString -> B.ByteString -> B.ByteString
> encodeNAry' params p s = e
> 	where 
>			s' = toStrict $ S.replace bStr oneStr (toStrict $ S.replace  oneStr zeroStr (toStrict $ S.replace p bStr s))
>			e = encode (params {leftRDepth = (leftRDepth params) + 1, mt = (toInteger $ B.length s')}) s'

`encodeT` is the most important function for algorithm. Here is an idea of how it works:
- Take an integer *t* and a string *s* (of length *n*, say). 
- Find all the substrings of length *t* of *s* (there are *n-t* such substrings).
- For any possible substring *ss*, Use `encodeNAry` to encode the positions of the occurences of *ss* in *s*.
- Cut *ss* out of *s* so that we end up with a shorter string *s'*. 
- There are two solutions : 
	a) encode *s'* as simple binary string using `encodeBinary`
	;  b) repeat the same operation on *s'*, trying all possible values of *t*.
- Return the best encoding, *i.e.* the shortest one.

> encodeT :: Parameters -> Integer -> B.ByteString -> B.ByteString
> encodeT ps t s = 
>		let 
>			allPatterns = substrings (fromIntegral t) s
>			uniquePatterns = nub allPatterns
>			patterns = if toInteger (length uniquePatterns) <= (topNPatterns ps)
>				then uniquePatterns
>				else 
>					if (topNPatterns ps) <= 0 then uniquePatterns
>						else
>						let
>							elemCounts = [(element, count) | element <- uniquePatterns, let count = length (filter (==element) allPatterns)]
>							sortedCounts = sortBy (compare `on` snd) elemCounts
>						in -- Now that the patterns have been sorted by frequency we need to pick the n most frequent and the n least frequent.
>							 -- What if n = 2 and there are 5 patterns of length 1. ... we want to select two patterns at random : TODO!
>							nub (take (fromIntegral (topNPatterns ps)) (((map fst).reverse) sortedCounts) ++
>								take (fromIntegral (topNPatterns ps)) (map fst sortedCounts))
>			level0 = 
>				let
>					encodeWithPattern p = if t == 1
>						then -- Simply encode the string
>							B.cons W._1 (selfDelimited p) `B.append` encodeBinary s
>						else -- The pattern is a proper substring
>							let
>								e' = encodeNAry ps True p s
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
>							e' = encodeNAry ps False p s
>							s' = toStrict $ S.replace p B.empty s
>							r = min t (toInteger (B.length s'))
>							encodeDeeper l = e' `B.append` encodeT (ps {rightRDepth = (rightRDepth ps) +1}) l s'
>						in
>							if B.length s' == 0 then Nothing
>								else -- Else try to encode the left over
>									Just $ minimumBy ordBS (map encodeDeeper [r,r-1..1])
>					encodings = map encodeWithPattern patterns
>				in
>					if t == 1 then Nothing
>						else fmap (minimumBy ordBS) (sequence encodings)
>		in
>			case (rightRDepth ps) >= (maxRRDepth ps) of
>				False -> case higherLevels of 
>					Nothing -> level0
>					Just hLs -> if B.length level0 < B.length hLs then level0 else hLs
>				_ -> level0

`encode` : call `encodeT` trying all possible values of *t*, keep the best.  

> encode :: Parameters -> B.ByteString -> B.ByteString
> encode ps s = minimumBy ordBS encodings
>	where
>		n = toInteger $ B.length s
>		mt' = if (mt ps) < 0 then n else (mt ps)
>		simple = [B.cons W._0 $ encodeBinary s]
>		encodings = if (leftRDepth ps) > (maxLRDepth ps)
>			then
>				simple
>			else 
>				if (leftRDepth ps) == 0 
>					then
>						map (\t -> (selfDelimited(toBin (n-t)) `B.append` encodeT (ps {rightRDepth = 0, mt = mt'}) t s)) [mt',mt'-1..1] 
>					else
>						simple ++ map (\t -> (W._1 `B.cons` (preEncode s) `B.append` encodeT (ps {rightRDepth = 0, mt = mt' }) t s)) [mt',mt'-1..1]

`pEncode` : same as `encode` but in parallel. Different values of *t* are tested at the same time.

> pEncode :: Parameters -> B.ByteString -> B.ByteString
> pEncode ps s = minimumBy ordBS encodings
>	where
>		n = toInteger $ B.length s
>		mt' = if (mt ps) < 0 then n else (mt ps)
>		simple = [B.cons W._0 $ encodeBinary s]
>		bs = if (leftRDepth ps) > (maxLRDepth ps)
>			then
>				simple
>			else 
>				if (leftRDepth ps) == 0 
>					then
>						map (\t -> (selfDelimited(toBin (n-t)) `B.append` encodeT (ps {rightRDepth = 0, mt = mt'}) t s)) [mt',mt'-1..1] 
>					else
>						simple ++ map (\t -> (W._1 `B.cons` (preEncode s) `B.append` encodeT (ps {rightRDepth = 0, mt = mt'}) t s)) [mt',mt'-1..1]
>		encodings = bs `PS.using` PS.parList PS.rdeepseq
