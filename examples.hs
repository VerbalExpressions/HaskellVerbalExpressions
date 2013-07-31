import Text.Regex.VerbalExpressions

main :: IO()
main = do
	--create an example of how to test for correctly formed URLs
	let expr = 	  endOfLine'
				. anythingBut " "
				. possibly "www"
				. find "://"
				. possibly "s"
				. find "http"
				. startOfLine'
				. searchGlobal'
				$ verEx

	-- Use VerEx's test function to find if it matches
	print $ test "http://www.google.com" expr

	--Ouputs the actual expression used: ^(?:http)(?:s)?(?:://)(?:www.)?(?:[^ ]*)$
	print $ expr

	-- Create a test string
	let replaceMe = "Replace bird with a duck"
	-- Create an expression that seeks for word "bird"
	let expr2 = find "bird" $ verEx;
	
	-- Execute the expression
	print $ replace replaceMe "duck" expr2

	-- Shorthand string replace
	print $ replace "We have a red house" "blue" . find "red" $ verEx