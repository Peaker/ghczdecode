-- Taken from GHC: compiler/utils/Encoding.hs

import Data.Char
import System.Environment (getArgs)

-- -----------------------------------------------------------------------------
-- The Z-encoding

{-
This is the main name-encoding and decoding function.  It encodes any
string into a string that is acceptable as a C name.  This is done
right before we emit a symbol name into the compiled C or asm code.
Z-encoding of strings is cached in the FastString interface, so we
never encode the same string more than once.

The basic encoding scheme is this.

* Tuples (,,,) are coded as Z3T

* Alphabetic characters (upper and lower) and digits
        all translate to themselves;
        except 'Z', which translates to 'ZZ'
        and    'z', which translates to 'zz'
  We need both so that we can preserve the variable/tycon distinction

* Most other printable characters translate to 'zx' or 'Zx' for some
        alphabetic character x

* The others translate as 'znnnU' where 'nnn' is the decimal number
        of the character

        Before          After
        --------------------------
        Trak            Trak
        foo_wib         foozuwib
        >               zg
        >1              zg1
        foo#            foozh
        foo##           foozhzh
        foo##1          foozhzh1
        fooZ            fooZZ
        :+              ZCzp
        ()              Z0T     0-tuple
        (,,,,)          Z5T     5-tuple
        (# #)           Z1H     unboxed 1-tuple (note the space)
        (#,,,,#)        Z5H     unboxed 5-tuple
                (NB: There is no Z1T nor Z0H.)
-}

type UserString = String        -- As the user typed it
type EncodedString = String     -- Encoded form

zDecodeString :: EncodedString -> UserString
zDecodeString [] = []
zDecodeString ('Z' : d : rest)
  | isDigit d = decode_tuple   d rest
  | otherwise = decode_upper   d : zDecodeString rest
zDecodeString ('z' : d : rest)
  | isDigit d = decode_num_esc d rest
  | otherwise = decode_lower   d : zDecodeString rest
zDecodeString (c   : rest) = c : zDecodeString rest

decode_upper, decode_lower :: Char -> Char

decode_upper 'L' = '('
decode_upper 'R' = ')'
decode_upper 'M' = '['
decode_upper 'N' = ']'
decode_upper 'C' = ':'
decode_upper 'Z' = 'Z'
decode_upper ch  = {-pprTrace "decode_upper" (char ch)-} ch

decode_lower 'z' = 'z'
decode_lower 'a' = '&'
decode_lower 'b' = '|'
decode_lower 'c' = '^'
decode_lower 'd' = '$'
decode_lower 'e' = '='
decode_lower 'g' = '>'
decode_lower 'h' = '#'
decode_lower 'i' = '.'
decode_lower 'l' = '<'
decode_lower 'm' = '-'
decode_lower 'n' = '!'
decode_lower 'p' = '+'
decode_lower 'q' = '\''
decode_lower 'r' = '\\'
decode_lower 's' = '/'
decode_lower 't' = '*'
decode_lower 'u' = '_'
decode_lower 'v' = '%'
decode_lower ch  = {-pprTrace "decode_lower" (char ch)-} ch

-- Characters not having a specific code are coded as z224U (in hex)
decode_num_esc :: Char -> EncodedString -> UserString
decode_num_esc d
  = go (digitToInt d)
  where
    go n (c : rest) | isHexDigit c = go (16*n + digitToInt c) rest
    go n ('U' : rest)           = chr n : zDecodeString rest
    go n other = error ("decode_num_esc: " ++ show n ++  ' ':other)

decode_tuple :: Char -> EncodedString -> UserString
decode_tuple d
  = go (digitToInt d)
  where
        -- NB. recurse back to zDecodeString after decoding the tuple, because
        -- the tuple might be embedded in a longer name.
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go 0 ('T':rest)     = "()" ++ zDecodeString rest
    go n ('T':rest)     = '(' : replicate (n-1) ',' ++ ")" ++ zDecodeString rest
    go 1 ('H':rest)     = "(# #)" ++ zDecodeString rest
    go n ('H':rest)     = '(' : '#' : replicate (n-1) ',' ++ "#)" ++ zDecodeString rest
    go n other = error ("decode_tuple: " ++ show n ++ ' ':other)

{-
Tuples are encoded as
        Z3T or Z3H
for 3-tuples or unboxed 3-tuples respectively.  No other encoding starts
        Z<digit>

* "(# #)" is the tycon for an unboxed 1-tuple (not 0-tuple)
  There are no unboxed 0-tuples.

* "()" is the tycon for a boxed 0-tuple.
  There are no boxed 1-tuples.
-}

main :: IO ()
main = do
  args <- getArgs
  putStrLn . unwords $ map zDecodeString args
