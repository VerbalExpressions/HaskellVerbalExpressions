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
import Data.List(intersperse, isPrefixOf)

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
withAnyCase v          = withAnyCase' True v

withAnyCase' :: Bool -> VerStruct -> VerStruct
withAnyCase' True  v   = v { flags = flags v  .|.   ignorecase }
withAnyCase' False v   = v { flags = flags v  `xor` ignorecase }

searchOneLine :: VerStruct -> VerStruct
searchOneLine v        = searchOneLine' True v

searchOneLine' :: Bool -> VerStruct -> VerStruct
searchOneLine' True  v = v { flags = flags v  `xor` multiline  }
searchOneLine' False v = v { flags = flags v  .|.   multiline  }

searchGlobal :: VerStruct -> VerStruct
searchGlobal v         = searchGlobal' True v

searchGlobal' :: Bool -> VerStruct -> VerStruct
searchGlobal' True  v  = v { flags = flags v  .|.   global     }
searchGlobal' False v  = v { flags = flags v  `xor` global     }

add :: String -> VerStruct -> VerStruct
add val v = v { pattern = foldl (++) "" [prefix v, source v, val, suffix v]
              , source  = foldl (++) "" [source v, val] }

find :: String -> VerStruct -> VerStruct
find val v         = add ("(?:"   ++ val ++ ")")   v

possibly :: String -> VerStruct -> VerStruct
possibly val v     = add ("(?:"   ++ val ++ ")?")  v

anything :: VerStruct -> VerStruct
anything v         = add "(?:.*)"                  v

anythingBut :: String -> VerStruct -> VerStruct
anythingBut val v  = add ("(?:[^" ++ val ++ "]*)") v

something :: VerStruct -> VerStruct
something v        = add "(?:.+)"                  v

somethingBut :: String -> VerStruct -> VerStruct
somethingBut val v = add ("(?:[^" ++ val ++ "]+)") v

startOfLine :: VerStruct -> VerStruct
startOfLine v        = startOfLine True v

startOfLine' :: Bool -> VerStruct -> VerStruct
startOfLine' True  v = add "" v { prefix = "^" }
startOfLine' False v = add "" v { prefix = ""  }

endOfLine :: VerStruct -> VerStruct
endOfLine v          = endOfLine True v

endOfLine' :: Bool -> VerStruct -> VerStruct
endOfLine' True  v   = add "" v { suffix = "$" }
endOfLine' False v   = add "" v { suffix = ""  }

lineBreak :: VerStruct -> VerStruct
lineBreak v  = add "(?:(?:\\n)|(?:\\r\\n))"        v

br :: VerStruct -> VerStruct
br v         = lineBreak                           v

tab :: VerStruct -> VerStruct
tab v        = add "(\\t)"                         v

word :: VerStruct -> VerStruct
word v       = add "(\\w+)"                        v

anyOf :: String -> VerStruct -> VerStruct
anyOf val v  = add ("[" ++ val ++ "]")             v

range :: [String] -> VerStruct -> VerStruct
range args v = add ("[" ++ buildrange args ++ "]") v
  where
    buildrange xs | length xs >= 2 = head xs ++ "-" ++ (head $ tail xs) ++ (buildrange $ tail $ tail xs)
                  | otherwise      = ""

multiple :: String -> VerStruct -> VerStruct
multiple val v  | head val == '*' = add val          v
                | head val == '+' = add val          v
                | otherwise       = add ("+" ++ val) v

alt :: String -> VerStruct -> VerStruct
alt val v = find val (add ")|(" v { prefix = checkPrefix, suffix = checkSuffix })
  where
    checkPrefix
      | elem '(' (prefix v) == True = prefix v ++ "("
      | otherwise                   = prefix v

    checkSuffix
      | elem ')' (suffix v) == True = ")" ++ suffix v
      | otherwise                   = suffix v

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
    resultOf l = length l /= 0

    globalSearch :: String -> [String]
    globalSearch value = searcher value

    lineSearch :: String -> [String]
    lineSearch value = foldl (++) [] (map searcher (lines value))

replacewords :: [String] -> String -> String -> String
replacewords [] _ sen            = sen
replacewords (x:xs) replacer sen = replacewords xs replacer (replacefirst x sen)
  where
    replacefirst :: String -> String -> String
    replacefirst w s =  (head $ split w s) 
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
                                        then [] : []
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
join delim l = concat (intersperse delim l)