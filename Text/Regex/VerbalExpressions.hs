{-| A library to make it easier to work with regular expressions. Based on the (original)
    Javascript VerbalExpression library by jehna.

    Here's some examples, first a http validator:

    > let expr =    endOfLine
    >                . anythingBut " "
    >                . possibly "www"
    >                . find "://"
    >                . possibly "s"
    >                . find "http"
    >                . startOfLine
    >                . searchGlobal
    >                $ verEx

    You can use VerEx's test to find if it matches.

    > test "http://www.google.com" expr
    > True

    The actual expression is the following in regexp:

    > ^(?:http)(?:s)?(?:://)(?:www.)?(?:[^ ]*)$

    Replacing a string.

    > let replaceMe = "Replace bird with a duck"
    > let expr2 = find "bird" $ verEx;
    > foo = replace replaceMe "duck" expr2

    The above can be shortened.

    > bar = replace "We have a red house" "blue" . find "red" $ verEx

    Basic usage of Verbal Expressions is through a singleton, called
    verEx, that compiles it to a regexp.

    > let expr = (all of your terms) $ verEx -}
module Text.Regex.VerbalExpressions
  ( verEx
  , add
  , startOfLine
  , startOfLine'
  , endOfLine
  , endOfLine'
  , find
  , possibly
  , anything
  , anythingBut
  , something
  , somethingBut
  , replace
  , lineBreak
  , br
  , tab
  , word
  , anyOf
  , range
  , withAnyCase
  , withAnyCase'
  , searchOneLine
  , searchOneLine'
  , searchGlobal
  , searchGlobal'
  , multiple
  , alt
  , test
  ) where

import Text.Regex.PCRE (getAllTextMatches, (=~))
import Data.Bits((.|.), (.&.), xor )
import Data.List(intercalate, isPrefixOf)

type Flag = Int

ignorecase :: Flag
ignorecase = 1

multiline :: Flag
multiline  = 2

global :: Flag
global     = 4

data VerStruct = VerStruct { prefix  :: String
                           , pattern :: String
                           , suffix  :: String
                           , source  :: String
                           , flags   :: Flag
                           }
instance Show VerStruct where
   show = pattern

verEx :: VerStruct
verEx = VerStruct "" "" "" "" 0 

withAnyCase :: VerStruct -> VerStruct
withAnyCase = withAnyCase' True

withAnyCase' :: Bool -> VerStruct -> VerStruct
withAnyCase' True  v   = v { flags = flags v  .|.   ignorecase }
withAnyCase' False v   = v { flags = flags v  `xor` ignorecase }

searchOneLine :: VerStruct -> VerStruct
searchOneLine = searchOneLine' True

searchOneLine' :: Bool -> VerStruct -> VerStruct
searchOneLine' True  v = v { flags = flags v  `xor` multiline  }
searchOneLine' False v = v { flags = flags v  .|.   multiline  }

searchGlobal :: VerStruct -> VerStruct
searchGlobal = searchGlobal' True

searchGlobal' :: Bool -> VerStruct -> VerStruct
searchGlobal' True  v  = v { flags = flags v  .|.   global     }
searchGlobal' False v  = v { flags = flags v  `xor` global     }

add :: String -> VerStruct -> VerStruct
add val v = v { pattern = foldl (++) "" [prefix v, source v, val, suffix v]
              , source  = foldl (++) "" [source v, val] }

find :: String -> VerStruct -> VerStruct
find val = add ("(?:"   ++ val ++ ")")

possibly :: String -> VerStruct -> VerStruct
possibly val = add ("(?:"   ++ val ++ ")?")

anything :: VerStruct -> VerStruct
anything = add "(?:.*)"

anythingBut :: String -> VerStruct -> VerStruct
anythingBut val = add ("(?:[^" ++ val ++ "]*)")

something :: VerStruct -> VerStruct
something = add "(?:.+)"

somethingBut :: String -> VerStruct -> VerStruct
somethingBut val = add ("(?:[^" ++ val ++ "]+)")

startOfLine :: VerStruct -> VerStruct
startOfLine = startOfLine' True

startOfLine' :: Bool -> VerStruct -> VerStruct
startOfLine' True  v = add "" v { prefix = "^" }
startOfLine' False v = add "" v { prefix = ""  }

endOfLine :: VerStruct -> VerStruct
endOfLine = endOfLine' True

endOfLine' :: Bool -> VerStruct -> VerStruct
endOfLine' True  v   = add "" v { suffix = "$" }
endOfLine' False v   = add "" v { suffix = ""  }

lineBreak :: VerStruct -> VerStruct
lineBreak = add "(?:(?:\\n)|(?:\\r\\n))"

br :: VerStruct -> VerStruct
br = lineBreak

tab :: VerStruct -> VerStruct
tab = add "(\\t)"

word :: VerStruct -> VerStruct
word = add "(\\w+)"

anyOf :: String -> VerStruct -> VerStruct
anyOf val = add ("[" ++ val ++ "]")

range :: [String] -> VerStruct -> VerStruct
range args = add ("[" ++ buildrange args ++ "]")
  where
    buildrange xs | length xs >= 2 = head xs ++ "+" ++ head (tail xs) ++ buildrange (tail $ tail xs)
                  | otherwise      = ""

multiple :: String -> VerStruct -> VerStruct
multiple val v  | head val == '*' = add val          v
                | head val == '+' = add val          v
                | otherwise       = add ('+' : val)  v

alt :: String -> VerStruct -> VerStruct
alt val v = find val (add ")|(" v { prefix = checkPrefix, suffix = checkSuffix })
  where
    checkPrefix
      | elem '(' (prefix v) = prefix v ++ "("
      | otherwise           = prefix v

    checkSuffix
      | elem ')' (suffix v) = ")" ++ suffix v
      | otherwise           = suffix v

replace :: String -> String -> VerStruct -> String
replace s val v = replacewords (getStringMatches s v) val s

test :: String -> VerStruct -> Bool
test val v  | flags v .&. multiline > 0 = foundMatch val
            | otherwise                 = foundMatch $ foldl (++) "" (split "\n" val)
  where
    foundMatch :: String -> Bool
    foundMatch value  | flags v .&. global > 0 = resultOf $ globalSearch value
                      | otherwise              = resultOf $ lineSearch   value

    searcher :: String -> [String]
    searcher value = getStringMatches value v

    resultOf :: [a] -> Bool
    resultOf = not . null

    globalSearch :: String -> [String]
    globalSearch = searcher

    lineSearch :: String -> [String]
    lineSearch = concatMap searcher . lines

replacewords :: [String] -> String -> String -> String
replacewords [] _ sen            = sen
replacewords (x:xs) replacer sen = replacewords xs replacer (replacefirst x sen)
  where
    replacefirst :: String -> String -> String
    replacefirst w s =  head (split w s) 
                        ++ replacer
                        ++ join w (tail $ split w s)

getStringMatches :: String -> VerStruct -> [String]
getStringMatches val v = getAllTextMatches $ val =~ pattern v :: [String]


--these are from Data.List.Utils
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
  let (firstline, remainder) = breakList (isPrefixOf delim) str
    in 
      firstline : case remainder  of
                                  [] -> []
                                  x  -> if x == delim
                                        then [[]]
                                        else split delim 
                                          (drop (length delim) x)
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

join :: [a] -> [[a]] -> [a]
join = intercalate
