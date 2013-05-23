StringUtils.lhs - A set of functions performing basic operations (mostly
on ByteStrings)

> module StringUtils((#),toBin,selfDelimited,rankBinary,substrings,toStrict,longStr,ordBS,toBinFixedLength) where
> import qualified Math.Combinatorics.Exact.Binomial as S
> import qualified Numeric as N
> import qualified Data.Word8 as W
> import qualified Data.ByteString.Char8 as C
> import qualified Data.ByteString.Lazy as BL
> import qualified Data.ByteString as B

Infix notation for n choose k

> (#) :: Integer -> Integer -> Integer
> (#) n k = n `S.choose` k

Dirty : Strings cannont be longer than 2000

> longStr :: B.ByteString
> longStr = B.replicate 2000 W._0

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
> substrings n s = [ ((B.take n).(B.drop i)) s | i <- [0..(B.length s - n)]] -- Could be MUCH faster! FIXME
