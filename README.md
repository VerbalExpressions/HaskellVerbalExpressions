HaskellVerbalExpressions
====================


## Haskell Regular Expressions made easy
VerbalExpressions is a Haskell library that helps to construct difficult regular expressions.

This haskell lib is based off of the (original) Javascript [VerbalExpressions](https://github.com/jehna/VerbalExpressions) library by [jehna](https://github.com/jehna/).

## Other Implementations
You can see an up to date list of all ports on [VerbalExpressions.github.io](http://VerbalExpressions.github.io).
- [Javascript](https://github.com/jehna/VerbalExpressions)
- [Ruby](https://github.com/VerbalExpressions/RubyVerbalExpressions)
- [C#](https://github.com/VerbalExpressions/CSharpVerbalExpressions)
- [Python](https://github.com/VerbalExpressions/PythonVerbalExpressions)
- [Java](https://github.com/VerbalExpressions/JavaVerbalExpressions)
- [PHP](https://github.com/VerbalExpressions/PHPVerbalExpressions)
- [C++](https://github.com/VerbalExpressions/CppVerbalExpressions)

## How to get started

cd to HaskellVerbalExpressions

`cabal install verbalexpressions.cabal`

`ghc Text/Regex/VerbalExpressions/verbalexpressions.hs examples.hs`

## Examples

Here's a couple of simple examples to give an idea of how VerbalExpressions works:

### Testing if we have a valid URL

```haskell
-- Create an example of how to test for correctly formed URLs
let expr = 	  endOfLine
				. anythingBut " "
				. possibly "www"
				. find "://"
				. possibly "s"
				. find "http"
				. startOfLine
				. searchGlobal
				$ verEx

-- Use VerEx's test() function to find if it matches
print $ test "http://www.google.com" expr

-- Ouputs the actual expression used: ^(?:http)(?:s)?(?:://)(?:www.)?(?:[^ ]*)$
print $ expr
```

### Replacing strings

```haskell
-- Create a test string
let replaceMe = "Replace bird with a duck"
-- Create an expression that seeks for word "bird"
let expr2 = find "bird" $ verEx;

-- Execute the expression
print $ replace replaceMe "duck" expr2
```

### Shorthand for string replace:

```haskell
print $ replace "We have a red house" "blue" . find "red" $ verEx
```




Here you can find the API documentation for Verbal Expressions

## Basic usage
Basic usage of Verbal Expressions is through a singleton, called `verEx`, that creates a new expression for you:

```haskell
let expr = (all of your terms) $ verEx
```

##API 

### Terms
* . anything
* . anythingBut - String
* . something
* . somethingBut - String
* . endOfLine'
* . find - String
* . possibly - String
* . startOfLine

### Special characters and groups
* .any - String
* .anyOf - String
* .br
* .lineBreak
* .range - [String]
* .tab
* .word

### Modifiers
* .withAnyCase
* .searchOneLine
* .searchGlobal

### Functions
* .replace - String(source) String(value)
* .test

### Other
* .add - String
* .multiple - String
* .alt
