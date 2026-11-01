{-# LANGUAGE TemplateHaskell #-}

module Text.Scanf.TH where

import Data.Char (isSpace)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Text.Scanf.Internal

import Data.Set (Set, empty, insert, singleton)

formatString :: String -> Q Exp
formatString "" = [|Empty|]
formatString ('%' : s1) =
  case s1 of
    'c' : s' -> [|char $(formatString s')|]
    'd' : s' -> [|int $(formatString s')|]
    'f' : s' -> [|double $(formatString s')|]
    'l' : s' -> [|integer $(formatString s')|]
    's' : s' -> [|string $(formatString s')|]
    '[' : s' -> mkScanset s'
    '%' : s' -> [|constant "%" $(formatString s')|]
    '0' : s' -> [|constant "\0" $(formatString s')|]
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
        parseScanset ('^' : t'')       = (parseScanset' empty           t'', False)
        parseScanset (']' : t'')       = (parseScanset' (singleton ']') t'', True)
        parseScanset t''               = (parseScanset' empty           t'', True)


        parseScanset' :: Set Char -> String -> (Set Char, String)
        parseScanset' _ []          = error "Unterminated scanset"
        parseScanset' s (']' : t'') = (s, t'')
        parseScanset' _ ['\\']      = error "Unterminated scanset"

        parseScanset' s ('\\' : c : t'') = parseScanset' s $ ch : t''
          where
            ch = case c of
              'a'  -> toEnum 0x07
              'b'  -> toEnum 0x08
              'e'  -> toEnum 0x1b
              'f'  -> toEnum 0x0c
              'n'  -> toEnum 0x0a
              'r'  -> toEnum 0x0d
              't'  -> toEnum 0x09
              'v'  -> toEnum 0x0b
              '\\' -> toEnum 0x5c
              '\'' -> toEnum 0x27
              '"'  -> toEnum 0x22
              '?'  -> toEnum 0x3f
              '0'  -> toEnum 0x00
              _    -> error "Unknown escape sequence"

        parseScanset' s (c1 : '-' : ']' : t'') = parseScanset' (insert '-' $ insert c1 s) t''
        parseScanset' s (c1 : '-' : c2  : t'') = parseScanset' (foldr insert s [c1..c2])  t''
        parseScanset' s (c  :             t'') = parseScanset' (insert c s)               t''

formatString s@(c : _) | isSpace c =
  let (s0, s') = span isSpace s
  in [|whitespace $(lift s0) $(formatString s')|]
formatString s =
  let (s0, s') = break (\c -> isSpace c || c == '%') s
  in [|constant $(lift s0) $(formatString s')|]

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
