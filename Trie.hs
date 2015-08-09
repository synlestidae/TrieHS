module Trie(emptyTrie, inTrie, addToTrie, removeFromTrie) where
import Data.List
import Debug.Trace 

data Trie = Node {
	member :: Bool,
	children :: [(Char, Trie)]
}

instance Show Trie where
	show e = joinStr $ map reverse (trieStr "" (children e))
		where joinStr = (concat . (intersperse ", "))

trieStr :: String -> [(Char, Trie)] -> [String]
trieStr parent [] = []
trieStr parent ((c, trie):rest) 
	| member trie = (c:parent) : result
	| otherwise =  result
	where result = trieStr (c:parent) (children trie)++(trieStr parent rest)

inTrie :: Trie -> [Char] -> Bool
inTrie node ([]) = member node
inTrie Node {children = children'} (c:cs) = 
	any (\(char, child) -> c == char && inTrie child cs) children'

emptyTrie :: Trie
emptyTrie = Node {member = False, children = []}

addToTrie :: [Char] -> Trie -> Trie
addToTrie [] node = node{member = True}
addToTrie (c:cs) trie =  (case existingNode (children trie) c of
	(otherTrie, members') -> trie{ children = (c, newElem):members' }
		where
			newElem = (addToTrie (cs) otherTrie)
			)

removeFromTrie :: [Char] -> Trie -> Trie
removeFromTrie [] node = node{member = False}
removeFromTrie str node =
	node{ children = map (removeStr str)  children'}
	where children' = children node 

removeStr :: [Char] -> (Char, Trie) -> (Char, Trie)
removeStr [] (c, trie) = (c, trie{member = False})
removeStr (c1:cs) (c2, trie)
	| c1 == c2 = (c1, removeFromTrie cs trie)
	| otherwise = (c2, trie)

existingNode = existingNode'
existingNode' :: [(Char, Trie)] -> Char -> (Trie, [(Char, Trie)])
existingNode' [] _ = (emptyTrie, [])
existingNode' ((c, trie):rest) c' 
	| c == c' = (trie, rest)
	| otherwise = case furtherResult of
		(something, others) -> (something, (c, trie):others)
		where furtherResult = existingNode rest c'