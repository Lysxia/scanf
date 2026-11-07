{-# LANGUAGE TemplateHaskell #-}

module Text.Scanf.TH where

import Data.Char (isSpace)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Text.Scanf.Internal

import Data.Set (Set, empty, insert, singleton)

import Numeric (showHex)

formatString :: String -> Q Exp
formatString ('\\' : s1) = formatString' $ escape s1
formatString s1          = formatString'          s1

formatString' :: String -> Q Exp
formatString' ""         = [|Empty|]
formatString' ('%' : s1) =
  case s1 of
    'c' : s' -> [|char    $(formatString s')|]
    'd' : s' -> [|int     $(formatString s')|]
    'f' : s' -> [|double  $(formatString s')|]
    'l' : s' -> [|integer $(formatString s')|]
    's' : s' -> [|string  $(formatString s')|]
    '[' : s' -> mkScanset s'
    '%' : s' -> [|constant "%" $(formatString s')|]
    _ -> error "Invalid format string"
  where
    mkScanset :: String -> Q Exp
    mkScanset s' = [|scanset set inc $(formatString s'')|]
      where
        set :: Set Char
        inc :: Bool
        s'' :: String

        ((set, s''), inc) = parseScanset s'

        parseScanset :: String -> ((Set Char, String), Bool)
        parseScanset []                = error "Unterminated scanset"
        parseScanset ['^', ']']        = error "Unterminated scanset"
        parseScanset ('^' : ']' : t'') = (parseScanset' (singleton ']') t'', False)
        parseScanset ('^' :       t'') = (parseScanset' empty           t'', False)
        parseScanset (']' :       t'') = (parseScanset' (singleton ']') t'', True)
        parseScanset t''               = (parseScanset' empty           t'', True)

        parseScanset' :: Set Char -> String -> (Set Char, String)
        parseScanset' s ('\\' : t'') = parseScanset'' s $ escape t''
        parseScanset' s t''          = parseScanset'' s   t''

        parseScanset'' :: Set Char -> String -> (Set Char, String)
        parseScanset'' _ []                        = error "Unterminated scanset"
        parseScanset'' s (']' :               t'') = (s, t'')
        parseScanset'' s (c1  : '-'  : ']'  : t'') = (insert '-' $ insert c1 s , t'')
        parseScanset'' s (c1  : '-'  : c2   : t'') =
          case c2 of
            '\\' -> parseScanset' (foldr insert s [c1..c2'])  t'''
            _    -> parseScanset' (foldr insert s [c1..c2])   t''
          where
            (c2',t''') = escape' t''
        parseScanset'' s (c   :               t'') = parseScanset' (insert c s)               t''

formatString' s@(c : _) | isSpace c =
  let (s0, s') = span isSpace s
  in [|whitespace $(lift s0) $(formatString s')|]
formatString' s =
  let (s0, s') = break (\c -> isSpace c || c == '%' || c == '\\') s
  in [|constant $(lift s0) $(formatString s')|]

-- Escapes the first character(s) in the input string.
escape :: String -> String
escape s = c : s'
  where (c,s') = escape' s

escape' :: String -> (Char,String)
escape' ""      = error "Can't escape an empty string"
escape' (c : s) =
  case c of
    '0'  -> (toEnum 0x00 , s)
    'a'  -> (toEnum 0x07 , s)
    'b'  -> (toEnum 0x08 , s)
    'e'  -> (toEnum 0x1b , s)
    'f'  -> (toEnum 0x0c , s)
    'n'  -> ('\n'        , s)
    'r'  -> (toEnum 0x0d , s)
    't'  -> ('\t'        , s)
    'v'  -> (toEnum 0x0b , s)
    'x'  -> escapeHex      s
    '\'' -> ('\''        , s)
    '"'  -> ('\"'        , s)
    '?'  -> (toEnum 0x3f , s)
    '\\' -> ('\\'        , s)
    _    -> error $ "Unknown escape sequence " ++ "(0x" ++ showHex (fromEnum c) "): '" ++ [c] ++ "'"
  where
    -- Replaces two hex digits with a character of that value.
    escapeHex :: String -> (Char,String)
    escapeHex (c1 : c0 : t) = ((toEnum $ 16 * fromHexDigit c1 + fromHexDigit c0) , t)
      where
        fromHexDigit :: Char -> Int
        fromHexDigit hc | '0' <= hc && hc <= '9' = fromEnum hc - fromEnum '0'
                        | 'A' <= hc && hc <= 'F' = 10 + fromEnum hc - fromEnum 'A'
                        | 'a' <= hc && hc <= 'f' = 10 + fromEnum hc - fromEnum 'a'
                        | otherwise = error $ "Invalid hex digit in \\x sequence: " ++ [hc]
    escapeHex _ = error "Missing 2-digit hexadecimal number following \\x."

-- | Parse a typed 'Format' string.
--
-- The following conversion strings are supported:
--
-- - @%d@: signed integer ('Int')
-- - @%l@: signed integer ('Integer', unbounded)
-- - @%f@: floating point ('Double')
-- - @%s@: string of non-space characters ('String')
-- - @%c@: single character ('Char')
-- - @%%@: parse/print a literal percent character
--
-- N.B.: in 'scanf', spaces in the format string match any number of whitespace
-- character until the next nonspace character.
--
-- @
-- ['fmt'|%d lazy %s and %d strict %s|]
--   :: 'Format' ('Int' ':+' 'String' ':+' 'Int' ':+' 'String' ':+' ())
-- @
fmt :: QuasiQuoter
fmt = QuasiQuoter
  { quoteExp = formatString
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
