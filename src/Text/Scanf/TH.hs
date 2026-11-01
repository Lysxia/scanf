{-# LANGUAGE TemplateHaskell #-}

module Text.Scanf.TH where

import Data.Char (isSpace)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Text.Scanf.Internal

import Data.Set (Set, empty, insert, singleton)

formatString :: String -> Q Exp
formatString ""          = [|Empty|]
formatString ('\\' : s1) = [|constant [ec] $(formatString s1')|]
  where (ec, s1') = escape s1
formatString ('%'  : s1) =
  case s1 of
    'c' : s' -> [|char $(formatString s')|]
    'd' : s' -> [|int $(formatString s')|]
    'f' : s' -> [|double $(formatString s')|]
    'l' : s' -> [|integer $(formatString s')|]
    's' : s' -> [|string $(formatString s')|]
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
        parseScanset' _ []                                = error "Unterminated scanset"
        parseScanset' _ ['\\']                            = error "Unterminated scanset"
        parseScanset' s (']'  :               t'') = (s, t'')
        parseScanset' s ('\\' :               t'') = parseScanset' (insert ec s)              t'''
          where (ec, t''') = escape t''
        parseScanset' s (c1   : '-'  : '\\' : t'') = parseScanset' (foldr insert s [c1..ec])  t'''
          where (ec, t''') = escape t''
        parseScanset' s (c1   : '-'  : ']'  : t'') = parseScanset' (insert '-' $ insert c1 s) t''
        parseScanset' s (c1   : '-'  : c2   : t'') = parseScanset' (foldr insert s [c1..c2])  t''
        parseScanset' s (c    :               t'') = parseScanset' (insert c s)               t''

formatString s@(c : _) | isSpace c =
  let (s0, s') = span isSpace s
  in [|whitespace $(lift s0) $(formatString s')|]
formatString s =
  let (s0, s') = break (\c -> isSpace c || c == '%') s
  in [|constant $(lift s0) $(formatString s')|]


-- Escapes the character(s) in the input string, returning the resulting
-- character and string.
escape :: String -> (Char,String)
escape []      = error "Can't escape an empty string"
escape (c : s) =
  case c of
    '0'  -> (toEnum 0x00, s)
    'a'  -> (toEnum 0x07, s)
    'b'  -> (toEnum 0x08, s)
    'e'  -> (toEnum 0x1b, s)
    'f'  -> (toEnum 0x0c, s)
    'n'  -> (toEnum 0x0a, s)
    'r'  -> (toEnum 0x0d, s)
    't'  -> (toEnum 0x09, s)
    'v'  -> (toEnum 0x0b, s)
    'x'  -> escapeHex     s
    '\'' -> (toEnum 0x27, s)
    '"'  -> (toEnum 0x22, s)
    '?'  -> (toEnum 0x3f, s)
    '\\' -> (toEnum 0x5c, s)
    _    -> error "Unknown escape sequence"
  where
    -- Replaces two hex digits with a character of that value.
    escapeHex :: String -> (Char,String)
    escapeHex (c1 : c0 : t)
      | d1 >= 16 || d0 >= 16 = error "Unknown escape sequence"
      | otherwise            = (toEnum $ 16*d1 + d0 , t)
      where
        d1, d0 :: Int
        d1 = fromEnum c1 - fromEnum '0'
        d0 = fromEnum c0 - fromEnum '0'
    escapeHex _              = error "Unknown escape sequence"

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
