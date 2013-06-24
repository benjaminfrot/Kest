StringUtils.lhs - A set of functions performing basic operations (mostly
on ByteStrings)

> module StringUtils((#),toBin,selfDelimited,rankBinary,substrings,toStrict,ordBS,toBinFixedLength,toBinaryString,enumeratePositions) where
> import qualified Math.Combinatorics.Exact.Binomial as S
> import qualified Numeric as N
> import qualified Data.Word8 as W
> import qualified Data.ByteString.Char8 as C
> import qualified Data.ByteString.Lazy as BL
> import qualified Data.ByteString.Internal as BI
> import qualified Data.ByteString.Search as SS
> import qualified Data.ByteString as B
> import qualified Data.Set as S
> import qualified Data.Map as M

Infix notation for n choose k

> (#) :: Integer -> Integer -> Integer
> (#) n k = n `S.choose` k

Order strings by length

> ordBS a b = if B.length a > B.length b then GT else if a == b then EQ else LT

Convert an integer to a binary string of length n. Pad with zeros if necessary

> binarySymbols :: Int -> Char
> binarySymbols 0 = '0'
> binarySymbols 1 = '1'

> toBinFixedLength :: Integer -> Integer -> B.ByteString
> toBinFixedLength l n = (B.replicate (fromIntegral (l - (toInteger (B.length e)))) W._0) `B.append` e
>		where e = C.pack (N.showIntAtBase 2 binarySymbols n "")

> toBin :: Integer -> B.ByteString
> toBin n = C.pack (N.showIntAtBase 2 binarySymbols n "")

Convert a lazy ByteString to a strict one

> toStrict :: BL.ByteString -> B.ByteString
> toStrict = B.concat . BL.toChunks

selfDelimited : encode a string of length n using n + 2*log2(n) bits.

> selfDelimited :: B.ByteString -> B.ByteString
> selfDelimited s = (B.snoc (B.replicate (B.length e - 1) W._0) W._1) `B.append` e `B.append` s
>		where e = toBin (toInteger (B.length s - 1))

Binary String Ranking : See http://en.wikipedia.org/wiki/Combinatorial_number_system

> rankBinary :: B.ByteString -> Integer
> rankBinary s = rankBinary' 1 0 1 s

> rankBinary' :: Integer -> Integer -> Integer -> B.ByteString -> Integer
> rankBinary' r n l (B.uncons -> Nothing) = n
> rankBinary' r n l (B.uncons -> Just((==W._1)->True,xs)) = if l > r then rankBinary' (r+1) (n + ((l-1) # r)) (l+1) xs else rankBinary' (r+1) n (l+1) xs
> rankBinary' r n l (B.uncons -> Just(_,xs)) = rankBinary' r n (l+1) xs

substrings : Returns all substrings of length n of s

> substrings :: Int -> B.ByteString -> [B.ByteString]
> substrings n s = [((B.take n).(B.drop i)) s | i <- [0..(B.length s - n)]] -- Could be MUCH faster! FIXME

enumeratePositions

> enumeratePositions :: B.ByteString -> B.ByteString -> Int -> [(B.ByteString,B.ByteString)]
> enumeratePositions s p t = 
>		let
>			toks = SS.split p s
>			n_occurences = (length toks)-1
>			createPair = 
>				let
>					(+-+) (a,b) (c,d) = (a `B.append` c, b `B.append` d)
>					blend (y:[]) (B.uncons -> Nothing) = (B.replicate (B.length y) W._0, y)
>					blend (y:ys) (B.uncons -> Just((==W._1) -> True,zs)) = ((B.replicate (B.length y) W._0) `B.append` (C.pack "1"), y) +-+ (blend ys zs)
>					blend (y:ys) (B.uncons -> Just((==W._0) -> True,zs)) = ((B.replicate ((B.length y) + (B.length p)) W._0) , y `B.append` p) +-+ (blend ys zs)
>				in
>					blend toks
>		in
>			if n_occurences > t
>				then map createPair (map (\x -> toBinFixedLength (toInteger n_occurences) (toInteger x)) [2^n_occurences -2,2^n_occurences - 1])
>				else map createPair (map (\x -> toBinFixedLength (toInteger n_occurences) (toInteger x)) [1..(2^n_occurences-1)])

++++ Support for non binary strings ++++
The algorithm works only on binary strings. So we want some simple tools to turn a string over an arbitrary
alphabet into a binary one.
Not very advanced for now : count the number of different symbols. If the cardinal of the alphabet is k,
then encode each symbol on log2(k) bits.

> toBinaryString :: M.Map W.Word8 B.ByteString -> B.ByteString -> B.ByteString
> toBinaryString m s =  B.foldl (if M.empty == m then convertToBinary else useMap) B.empty s
>		where
>			alphabet = S.fromList (BI.unpackBytes s)
>			cardinality = S.size alphabet
>			l = ceiling $ logBase 2 (fromIntegral cardinality)
>			symbolMap = M.fromList (zip (S.toList alphabet) [1..cardinality])
>			convertToBinary bs c = B.append bs (toBinFixedLength l (toInteger $ M.findIndex c symbolMap))
>			useMap bs c = B.append bs (M.findWithDefault (C.pack "") c m)
