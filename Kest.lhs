Kest.lhs - Entry point of the program. Parse command line options

> import RecursiveCompression
> import qualified Data.ByteString.Char8 as C
> import qualified Data.ByteString as B
> import StringUtils
> import Data.List
> import System.Random
> import qualified Data.Map as Map
> import System.Console.CmdArgs

Histogram : A simple function to build the histogram
of a list (see http://langref.org/fantom+erlang+haskell/maps/algorithms/histogram)

> histogram :: Ord a => [a] -> Map.Map a Integer
> histogram xs = Map.fromList [ (head l, toInteger (length l)) | l <- group (sort xs) ]

Generate a list of n randoms numbers in [a,b]

> randomList :: Integer -> Integer -> Integer -> IO [Integer]
> randomList a b n = (sequence (map (\x -> randomRIO (a,b)) [1..n]))

Distribution : Distribution of complexities for strings of a given length

> distribution :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO (Map.Map Integer Integer)
> distribution strLength lDepth rDepth mT sSize nP = 
>		let
>			l = randomList 0 (2^strLength -1) (2^sSize)
>			strings = fmap (map (toBinFixedLength strLength)) l
>			complexities = fmap (map (toInteger. B.length . (pEncode 0 lDepth rDepth mT nP))) strings
>		in
>			fmap histogram complexities

> printDistribution :: Integer -> Map.Map Integer Integer -> String
> printDistribution n m = (" 0.0 , " ++ show (2^n) ++ "\n") ++ concat ( map (\x -> (show (fst x)) ++ " , " ++ (show (snd x)) ++ "\n") (Map.assocs m))

Get a list of strings; return a list of encoded strings

> encodeList :: Integer -> Integer -> Integer -> Integer -> [B.ByteString] -> [String]
> encodeList lDepth rDepth mT nP =  map (C.unpack.(pEncode 0 lDepth rDepth mT nP).toBinaryString) 

++++++++++ Command line parsing / Help ++++++++++++++++++

> summaryStr = summary "Kest V.1.1 2013  -- benjamin.frot@dtc.ox.ac.uk"
> data Kest = 
>		Dist {stringLength :: Integer, maxLength :: Integer, sampleSize :: Integer, rrecursionDepth :: Integer,lrecursionDepth :: Integer, nPatterns :: Integer}
>	 | File {maxLength :: Integer, rrecursionDepth :: Integer, lrecursionDepth :: Integer, filename :: FilePath, justK :: Bool, nPatterns :: Integer}
>	 | SingleStr {inputStr :: String, maxLength :: Integer, rrecursionDepth :: Integer, lrecursionDepth :: Integer, justK :: Bool, nPatterns :: Integer}
>	deriving (Eq,Show,Data,Typeable)

> dist = Dist 
>	{
>	stringLength = 10 &= strLHelp
>	,sampleSize = 10 &= help "Number of strings to sample : 2^sampleSize. Default is 10, so 1024 are sampled."
>	, maxLength = -1 &= mLHelp
>	, rrecursionDepth = 1 &= rDHelp
>	, lrecursionDepth = 0 &= lrDHelp
>	, nPatterns = -1 &= nPatternsHelp 
> } &= help "Compute the distribution of complexities for strings of a given length by sampling the space at uniformly at random."

> file = File
>	{
>	maxLength = -1 &= mLHelp
>	, rrecursionDepth = 1 &= rDHelp
>	, lrecursionDepth = 0 &= lrDHelp
>	, filename = "./to_encode" &= fnHelp
>	, nPatterns = -1 &= nPatternsHelp 
> , justK = False &= justKHelp
>	} &= help "Read a list of strings a file (one/line) and outputs the encoded strings in the same order."

> single = SingleStr
>	{
>	inputStr = "000000000" &= iShelp
>	, maxLength = -1 &= mLHelp
>	, rrecursionDepth = 1 &= rDHelp
>	, lrecursionDepth = 0 &= lrDHelp
>	, nPatterns = -1 &= nPatternsHelp 
> , justK = False &= justKHelp
>	} &= help "Encode a single string given as parameter."

> strLHelp = help "Length of the strings to sample. Default : 10"
> mLHelp = help "Maximum length of the patterns to search for inside the strings. -1 sets it to the length of the input string. Default : -1"
> rDHelp = help "Once a patten has been found, how many other patterns should the algorithm try to detect. 0 means no extra patterns. Default 1."
> lrDHelp = help "The string encoding the positions can encoded recursively using this algorithm or can be encoded naively. This is controls the recursion depth. Default 0."
> fnHelp = help "Filename containing the list of strings to encode. Make sure there are no empty lines. Default ./to_encode"
> iShelp = help "String to be encoded."
> justKHelp = help "Whether only the length of the encoding should be output instead of the whole string. Default False."
> nPatternsHelp = help "Number of patterns to take into account. Setting it to k will take the top k most frequent patterns and the top k least frequent patterns. If -1 then all of them are used. Default -1."
> mode = cmdArgs $ modes [dist&=auto,file,single] &= summaryStr &= help "Approximate Kolmogorov complexity by encoding binary strings. Patterns of various length are detected and the input is recursively encoded. It can be *very* time consuming, the recursion depth should be chosen small. Parallel execution is supported : add +RTS -N to use all available cores, +RTS -Nn to use n cores, e.g. ./Kest -f myStrings +RTS -N2"

Main function : parse arguments 

> main = do 
>		arguments <- mode
>		case arguments of
>			Dist {stringLength = n, maxLength = m, sampleSize = s, rrecursionDepth = d,lrecursionDepth = l, nPatterns = nP} -> do
>				complexities <- distribution n l d m s nP
>				putStrLn $ printDistribution s complexities
>			File {filename = fn, maxLength = m, rrecursionDepth = d, lrecursionDepth = l,justK = jK, nPatterns = nP} -> do
>				strs <- fmap C.lines (B.readFile fn)
>				putStrLn $ concat $ map ((\x -> x ++ "\n").(if jK then show.length else id)) (encodeList l d m nP strs)
>			SingleStr {inputStr = iS, maxLength = m, rrecursionDepth = d, lrecursionDepth = l,justK = jK, nPatterns = nP} -> do
>				putStrLn $ (if jK then show.B.length else C.unpack) $ pEncode 0 l d m nP ((toBinaryString.C.pack) iS) 
