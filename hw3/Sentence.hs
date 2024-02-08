-- Homework 3 template
module Sentence where

-- Grammar for the animal sentence language:
--
--   <sentence> ->  <noun> <verb> [<noun>]  
--               	|  <sentence> `and` <sentence>
--				 	
--   <noun> 	-> <adj> <noun> | <noun> `and` <noun>
--					| `cats` | `dogs` | `bears` | `goats`

--   <verb>		->  `chase` | `cuddle` | `hug` | `scare`
--   <adj>		->	`sad` | `small` | `big` | `happy`

data Sentence = NVN Noun Verb Noun -- noun verb noun sentence
  | NV Noun Verb  -- noun verb sentence
  | And Sentence Sentence -- sentence and sentence
  | End -- end of sentence, in other words could be seen as epsilon in grammar
  deriving (Eq,Show)

-- represents adjective in sentence
data Adj = Sad | Small | Big | Happy -- possible values of adjective
  deriving (Eq,Show)

data Noun = NP Adj Noun  -- Noun phrase
  | NAnd Noun Noun  -- noun and noun
  | Bears | Cats | Dogs | Goats -- list of nouns
  deriving (Eq,Show)

data Verb = Chase | Cuddle | Hug | Scare -- list of verbs
  deriving (Eq,Show)


ex1 :: Sentence
ex1 = NVN Cats Hug Dogs

ex2 :: Sentence
ex2 = NVN (NP Small Cats) Hug Dogs

ex3 :: Sentence
ex3 = NVN (NAnd Dogs Cats) Chase Goats

ex4 :: Sentence
ex4 = NVN (NAnd (NP Happy Dogs) Cats) Chase Goats


-- | Build a sentence from a noun verb noun.
-- | buildS2 Cats Hug Cats
-- | NVN Cats Hug Cats
buildS2 :: Noun -> Verb ->Noun-> Sentence
buildS2 noun verb noun2 = NVN noun verb noun2

-- | Build a sentence from a noun verb 
-- | buildS1 Cats Hug 
-- | NV Cats Hug 
buildS1 :: Noun -> Verb ->Sentence
buildS1 noun verb = NV noun verb

-- | Build a noun phrase from an adjective and noun
-- | buildNP Happy Dogs
-- | NP Happy Dogs
buildNP :: Adj -> Noun -> Noun
buildNP noun1 noun2 = NP noun1 noun2

-- | Build a noun conjunction from two nouns
-- | buildNAnd Dogs Cats
-- | NAnd Dogs Cats
buildNAnd :: Noun -> Noun -> Noun
buildNAnd noun1 noun2 = NAnd noun1 noun2

-- | Build a sentence that is a conjunction of a list of other sentences.
-- | conjunction [ex1, ex2]
-- | And (NVN Cats Hug Dogs) (NVN (NP Small Cats) Hug Dogs)
-- | The End is used if no sentences are given
-- constructs all sentences in list together using and strings
conjunction :: [Sentence] -> Sentence
conjunction []     = End
conjunction [x]    = x
conjunction (x:xs) = And x (conjunction xs)
  
-- | Pretty print a sentence.
pretty :: Sentence -> String
pretty (NVN s v o) = prettyNoun s ++ " " ++ prettyVerb v ++ " " ++ prettyNoun o
pretty (NV s v)    = prettyNoun s ++ " " ++ prettyVerb v
pretty (And l End) = pretty l
pretty (And End r) = pretty r
pretty (And l r)   = pretty l ++ " and " ++ pretty r
pretty (End) = "."

-- | Pretty print a noun.
prettyNoun :: Noun -> String
prettyNoun Cats  = "cats"
prettyNoun Dogs  = "dogs" 
prettyNoun Bears = "bears" 
prettyNoun Goats = "goats" 

prettyNoun (NP a n) = prettyAdj a ++ " " ++ prettyNoun n
prettyNoun (NAnd m n) = prettyNoun m ++ " and " ++prettyNoun n

-- | Pretty print a verb.
prettyVerb :: Verb -> String
prettyVerb Chase  = "chase"
prettyVerb Cuddle = "cuddle"
prettyVerb Hug    = "hug"
prettyVerb Scare  = "scare"

-- | Pretty print an adjective.
prettyAdj :: Adj -> String
prettyAdj Sad   = "sad"
prettyAdj Small = "small"
prettyAdj Big   = "big"
prettyAdj Happy = "happy"

-- | Does the sentence contain only chase and scare?
-- | isMean ex2 => False
-- | isMean ex3 => True
-- set values for all possible matches
isMean :: Sentence -> Bool
isMean (NVN _ Chase _)  = True
isMean (NVN _ Scare _)  = True
isMean (NVN _ _ _)      = False
isMean (NV _ Chase)     = True
isMean (NV _ Scare)     = True
isMean (NV _ _)         = False
isMean (And s1 s2)      = isMean s1 && isMean s2 
isMean (End)            = False

-- |Count the number of words in a sentence
-- | wordCount ex4
--    6
-- Handle all different sentence scenarios and then properly count the amount of words
wordCount :: Sentence -> Int
wordCount (NVN noun1 _ noun2) = 1 + countNoun noun1 + countNoun noun2 
wordCount (NV noun1 _)    = 1 + countNoun noun1
wordCount (And l r)   = wordCount l + wordCount r + 1
wordCount (End)       = 0

-- In order to properly count the word count we also need to 
-- count the nouns since in the grammar a noun can be more than one word.
countNoun :: Noun -> Int 
countNoun (NP _ noun) = countNoun noun + 1 
countNoun (NAnd noun1 noun2) = countNoun noun1 + countNoun noun2 + 1
countNoun _ = 1
