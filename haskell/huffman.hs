import Data.Maybe
import Data.Char

data Tree c = Leaf c Int | Branch ( Tree c ) ( Tree c ) Int deriving ( Show, Eq, Ord, Read )

data Bit = Z | I deriving ( Eq, Ord )

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> ( show x ) ++ showList xs out

-- Part 1 ( Decompress a String using a given huffman tree and a bit list )

-- Convert Char into a Bit
readBit :: Char -> Bit
readBit '0' = Z
readBit '1' = I

-- Convert Int into a Bit
readBit' :: Int -> Bit
readBit' 0 = Z
readBit' 1 = I

-- Convert list of char bits into Bit list
readBits :: [Char] -> [Bit]
readBits xs = map readBit xs

-- Convert list of int bits into Bit list
readBits' :: [Int] -> [Bit]
readBits' xs = map readBit' xs

-- Get the String out of the Tree and the Bit list
decode :: Eq c => ( Tree c, [Bit] ) -> [c]
decode (tree, bits) = decodeAux tree tree bits

-- Helper for decode
decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux fullTree ( Leaf c _ ) [] = [c]
decodeAux fullTree ( Leaf c _ ) bs = c : ( decodeAux fullTree fullTree bs ) 
decodeAux fullTree ( Branch left right _ ) (Z:bs) = decodeAux fullTree left bs
decodeAux fullTree ( Branch left right _ ) (I:bs) = decodeAux fullTree right bs

-- Given [ lengthOfTree ++ Tree ++ BitList ] as a string, decompresses the String
decompress :: String -> String
decompress str = decode ( t, bits ) where
    (n',str') = span isDigit str
    n         = read n'
    t'        = take n str'
    t         = read t'
    bits      = readBits $ drop n str'

{- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed. 

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding. -}

charlength :: Int
charlength = 8

-- gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * ( length s )

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' ('*':s)   = s
decompress' s = decompress s

-- Generate the frequency table
-- An element of the type Freq is a symbol together with its frequency.
type Freq c = ( c, Int )

leaf :: Freq c -> Tree c
leaf ( c, i ) = Leaf c i

freq :: Tree c -> Int
freq ( Leaf _ i ) = i
freq ( Branch _ _ i ) = i

-- Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate = foldr update []

-- Removes the existing entry for c (if it exists), updates it, and then reinserts it if no entry exists
update :: Eq c => c -> [Freq c] -> [Freq c]
update c keys = newFreq : rest where
    ( old, rest ) = ( is c ) `outOf` keys
    key = fromMaybe ( c, 0 ) old
    newFreq = mapSnd ( +1 ) key

is :: Eq c => c -> Freq c -> Bool
is c ( d, _ ) = c == d

outOf :: ( a -> Bool ) -> [a] -> ( Maybe a, [a] )
outOf p []     = ( Nothing, [] )
outOf p (x:xs) = if ( p x ) then ( Just x, xs ) else ( mapSnd ( x: ) $ outOf p xs )

mapSnd :: ( a -> b ) -> ( c, a ) -> ( c, b )
mapSnd f ( c, a ) = ( c, f a )

-- End of Part 1.



-- Part 2 ( Compress a String into a huffman tree and a bitlist )

-- Collects a list of trees into an optimal prefix tree
makeTree :: [Tree c] -> Tree c
makeTree ( t1:t2:[] ) = merge t1 t2
makeTree ( t1:t2:ts ) = makeTree $ putNew ( merge t1 t2 ) ts

-- Puts new Tree into the right possition ( Ordered by frequency )
putNew :: Tree c -> [Tree c] -> [Tree c]
putNew t [] = [ t ]
putNew t ( x:xs ) | ( freq x ) < ( freq t ) = x : ( putNew t xs )
                  | otherwise = t : ( x:xs ) 

-- Wraps 2 trees in a Branch:
merge :: Tree c -> Tree c -> Tree c
merge t1 t2 = Branch t1 t2 ( ( freq t1 ) + ( freq t2 ) )

-- Generates a tree from list of Freqs ( using makeTree above )
generateTree :: [Freq c] -> Tree c
generateTree xs = makeTree [ leaf x | x <- xs ] 

-- Encoding table.
-- A key is a key-value pair ( an entry in a map / table ).
type Key c = ( c, [Bit] )

-- The whole coding table
type CodingTable c = [Key c]

-- Given a tree, generates a coding table
makeTable :: Eq c => Tree c -> CodingTable c
makeTable t = makeTable' t []

-- Goes throught the tree gathering Bits
makeTable':: Tree c -> [Bit] -> CodingTable c
makeTable' ( Leaf c i ) bits = [ ( c, bits ) ]
makeTable' ( Branch l r i ) bits = ( makeTable' l ( bits ++ [Z] ) ) ++ ( makeTable' r ( bits ++ [I] ) )

-- Takes a string of symbols to a bit list, based on a given coding table
encodeUsingTable :: Eq c => CodingTable c -> [c] -> [Bit]
encodeUsingTable xs [] = [] 
encodeUsingTable xs (y:ys) = ( findBits xs y ) ++ ( encodeUsingTable xs ys )

-- Gets the Bits of 1 given character
findBits :: Eq c => CodingTable c -> c -> [Bit]
findBits ( ( x, bits ):xs ) c | x == c = bits
                              | otherwise = findBits xs c


-- Encodes directly from the tree ( more efficient ).
encodeUsing :: Eq c => Tree c -> [c] -> [Bit]
encodeUsing t [] = []
encodeUsing t ( x:xs ) = ( bfsearch t x [] ) ++ ( encodeUsing t xs )

-- Goes through the tree checking if the leaf has the speciffic key ( Gathering bits on the way )
bfsearch :: Eq c => Tree c -> c -> [Bit] -> [Bit]
bfsearch ( Branch l r i ) x bits = ( bfsearch l x ( bits ++ [Z] ) ) ++ ( bfsearch r x ( bits  ++ [I] ) )
bfsearch ( Leaf c i ) x bits | c == x = bits
                             | otherwise = []


-- From a string of symbols, generate the coding tree and the encoding
encode :: Eq c => [c] -> ( Tree c, [Bit] )
encode xs = ( t, encodeUsingTable ( makeTable t ) xs ) where t = generateTree ( tabulate xs )

-- Encoding trees
-- Compressing a string. This should be the inverse of decompress.
-- That is, this should output a string of the form
--
-- n ++ t ++ c
--
-- Where,
--    * n is a read from an integer
--    * t is read from a tree, and contains exactly n characters.
--    * c is string of bits.
compress :: String -> String
compress xs = n ++ t ++ c where
    ( t', b ) = encode xs
    t = show t'
    n = show $ length t
    c = show b


-- Smarter compression: if the encoded string is larger than the input string,
-- instead output the input string with a '*' in front.
compress' :: String -> String
compress' xs | ( memSize xs ) < ( ( memSize $ n ++ t ) + c ) = ['*'] ++ xs
             | otherwise = compress xs where
    ( t', bits ) = encode xs
    t = show t'
    n = show $ length t
    c = length bits
