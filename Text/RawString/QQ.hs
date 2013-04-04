-- | Raw string literals, implemented using Template Haskell's quasiquotation
-- feature.
module Text.RawString.QQ
       where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

{-|

A quasiquoter for raw string literals - that is, string literals that don't
recognise the standard escape sequences (such as @\'\\n\'@). Basically, they
make your code more readable by freeing you from the responsibility to escape
backslashes. They are useful when working with regular expressions, DOS/Windows
paths and markup languages (such as XML).

Don't forget the @LANGUAGE QuasiQuotes@ pragma if you're using this
module in your code.

Usage:

@
    ghci> :set -XQuasiQuotes
    ghci> import Text.RawString.QQ
    ghci> let s = [r|\\w+\@[a-zA-Z_]+?\\.[a-zA-Z]{2,3}|]
    ghci> s
    \"\\\\w+\@[a-zA-Z_]+?\\\\.[a-zA-Z]{2,3}\"
    ghci> [r|C:\\Windows\\SYSTEM|] ++ [r|\\user32.dll|]
    \"C:\\\\Windows\\\\SYSTEM\\\\user32.dll\"
@

Multiline raw string literals are also supported:

@
    multiline :: String
    multiline = [r|\<HTML\>
    \<HEAD\>
    \<TITLE\>Auto-generated html formated source\</TITLE\>
    \<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=windows-1252\"\>
    \</HEAD\>
    \<BODY LINK=\"#0000ff\" VLINK=\"#800080\" BGCOLOR=\"#ffffff\"\>
    \<P\> \</P\>
    \<PRE\>|]
@

Caveat: since the @\"|]\"@ character sequence is used to terminate the
quasiquotation, you can't use it inside the raw string literal. Use 'rQ' if you
want to embed that character sequence inside the raw string.

For more on raw strings, see e.g.
<http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2006/n2053.html>

For more on quasiquotation, see
<http://www.haskell.org/haskellwiki/Quasiquotation>

-}
r :: QuasiQuoter
r = QuasiQuoter {
    -- Extracted from dead-simple-json.
    quoteExp  = return . LitE . StringL,

    quotePat  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a declaration)"
}

{-| A variant of 'r' that interprets the @\"|~]\"@ sequence as @\"|]\"@.

Usage:

@
    ghci> [rQ||~]|~]|]
    \"|]|]\"
@
-}
rQ :: QuasiQuoter
rQ = QuasiQuoter {
    quoteExp  = return . LitE . StringL . escape,

    quotePat  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a declaration)"
}
  where
    escape :: String -> String
    escape []               = []
    escape ('|':'~':']':xs) = '|':']':escape xs
    escape (x:xs)           = x : escape xs
