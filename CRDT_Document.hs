module CRDT_Document
( CRDT_Document
, IDE_Operation (..)
, CRDT_Operation
, applyOp
, create
, render
, genOpCRDT
, compressSt
, compressOp
, parseSt
, parseOp
) where

import Data.Ratio
import Text.ParserCombinators.Parsec

-- -----------+
-- Data types |
-- -----------+

-- | CRDT_Node is an internal representation of character in Logoot algorithm of WOOT (WithOut Operational Transformation) approach
data CRDT_Node = Node { coord      :: Rational -- Coordinate of character
                      , precedence :: Int      -- Among two characters with equal coordinates the one with higher precedence will be the rightmost
                      , isVisible  :: Bool     -- Defines if the character will be rendered
                      , atom       :: Char     -- The char itself
                      } deriving Show

-- | CRDT_Document is a wrapper around normal document allowing several users to operate on it simultaneously without information loss
type CRDT_Document = [CRDT_Node]

-- | IDE_Operation is an operation user is trying to perform on text
data IDE_Operation = IDE_NoOp | IDE_Insert Int Char | IDE_Delete Int deriving (Show, Read)

-- | CRDT_Operation is an operation to be performed on CRDT_Document
data CRDT_Operation = CRDT_NoOp | CRDT_Insert Rational Int Char | CRDT_Delete Rational Int deriving Show

-- ----------------------------+
-- Operations on CRDT_Document |
-- ----------------------------+

-- | Finds node in CRDT_Document corresponding to Nth visible character
findNthVisible :: CRDT_Document -> Int -> Either String CRDT_Node
findNthVisible     [] _ = Left "findNthVisible: Not enough visible characters"
findNthVisible (x:xs) n
  | not (isVisible x) = findNthVisible xs n
  | n == 0            = Right x
  | otherwise         = findNthVisible xs (n - 1)

-- | Finds a place to insert node for it to be Nth visible
findNthMargin :: CRDT_Document -> Int -> Either String Rational
findNthMargin     [] _ = Left "findNthMargin: Incorrect CRDT_Document. Invariant broken"
findNthMargin (z:xs) n = helper (coord z) n xs
  where helper crd n     [] = Left "findNthMargin: Not enough visible characters"
        helper crd 0 (x:xs) = if isVisible x then Right $ (crd + coord x)/2   else helper crd 0 xs
        helper crd n (x:xs) = if isVisible x then helper (coord x) (n - 1) xs else helper crd n xs

-- | Returns surroundings of a given node used in insertion or removal
findPlace :: CRDT_Document -> Rational -> Int -> ([CRDT_Node], [CRDT_Node], Bool)
findPlace nodes crd prc = helper [] nodes
  where helper skipped          [] = (reverse skipped, [], False)
        helper skipped rest@(x:xs) =
          case crd `compare` (coord x) of
            LT -> (reverse skipped, rest, False)
            EQ -> case prc `compare` (precedence x) of 
                    LT -> (reverse skipped, rest, False)
                    EQ -> (reverse skipped,   xs,  True) -- Look, if current node is equal to the searched one, we discard it
                    GT -> helper (x:skipped) xs
            GT -> helper (x:skipped) xs

-- | Inserts node into CRDT_Document
insertNode :: CRDT_Document -> Rational -> Int -> Char -> CRDT_Document
insertNode doc crd prc ch = let (l, r, _) = findPlace doc crd prc
                            in l ++ [ Node crd prc True ch ] ++ r

-- | Deletes a node from a CRDT_Document
deleteNode :: CRDT_Document -> Rational -> Int -> CRDT_Document
deleteNode doc crd prc = let (l, r, found) = findPlace doc crd prc
                         in l ++ (if found then [ Node crd prc False 'X' ] else [])  ++ r

-- | Applies CRDT_Operation to CRDT_Document
applyOp :: CRDT_Document -> CRDT_Operation -> CRDT_Document
applyOp doc  CRDT_NoOp               = doc
applyOp doc (CRDT_Insert crd prc ch) = insertNode doc crd prc ch
applyOp doc (CRDT_Delete crd prc   ) = deleteNode doc crd prc

-- ----------------------+
-- Interaction  with IDE |
-- ----------------------+

-- | Creates CRDT_Document from String
create :: String -> CRDT_Document
create str = Node 0 0 False '0' : genNodes str 1
  where genNodes []     i = [ Node i 0 True '\0' ]
        genNodes (x:xs) i = Node i 0 True x : genNodes xs (i + 1)

-- | Renders CRDT_Document
render :: CRDT_Document -> String
render     [] = []
render (x:xs)
  | isVisible x = atom x : render xs
  | otherwise   = render xs

-- | Method that generates CRDT_Operation of IDE_Operation and CRDT_Document
genOpCRDT :: CRDT_Document -> Int -> IDE_Operation -> Either String CRDT_Operation
genOpCRDT   _   _  IDE_NoOp         = return CRDT_NoOp
genOpCRDT doc prc (IDE_Insert n ch) = findNthMargin  doc n >>= (\crd                -> return $ CRDT_Insert crd prc ch)
genOpCRDT doc   _ (IDE_Delete n   ) = findNthVisible doc n >>= (\(Node crd prc _ _) -> return $ CRDT_Delete crd prc)
  
-- ------------------------+
-- Compression and parsing |
-- ------------------------+

-- | Compresses CRDT_Document without loss of information for network transfer
compressSt :: CRDT_Document -> String
compressSt = concatMap compressNode
  where compressNode (Node crd prc vis ch) = 
          if vis then
                   show (  numerator crd) ++
            "%" ++ show (denominator crd) ++
            "p" ++ show prc ++
            "a" ++ [ch]
          else ""

-- | Compresses CRDT_Operation  without loss of information for network transfer
compressOp :: CRDT_Operation -> String
compressOp CRDT_NoOp = "n"
compressOp (CRDT_Insert crd prc ch) = 
  "i" ++ show (  numerator crd) ++
  "%" ++ show (denominator crd) ++
  "p" ++ show prc ++
  "a" ++ [ch]
compressOp (CRDT_Delete crd prc) = 
  "d" ++ show (  numerator crd) ++
  "%" ++ show (denominator crd) ++
  "p" ++ show prc

-- | Compresses IDE_Operation
compressIDEOp :: IDE_Operation -> String
compressIDEOp IDE_NoOp = "n"
compressIDEOp (IDE_Insert ind ch) = "i" ++ show ind ++ "a" ++ [ch]
compressIDEOp (IDE_Delete ind   ) = "d" ++ show ind

-- | Parses CRDT_Document from string
parseSt :: GenParser Char () CRDT_Document
parseSt = ((:) (Node 0 0 False '0')) <$> many node
  where node = Node <$> parseRat
                    <*> (char 'p' *> parseInt)
                    <*> (return True)
                    <*> (char 'a' *>  anyChar)
                    <?> "node (CRDT_Node)"

-- | Parses CRDT_Operation from string
parseOp :: GenParser Char () CRDT_Operation
parseOp = noop <|> ins <|> del <?> "CRDT_Operation"
  where noop = (const CRDT_NoOp) <$> char 'n'
        ins  = CRDT_Insert <$> (char 'i' *> parseRat) <*> (char 'p' *> parseInt) <*> (char 'a' *> anyChar)
        del  = CRDT_Delete <$> (char 'd' *> parseRat) <*> (char 'p' *> parseInt)

-- | Parses IDE_Operation
parseIDEOp :: GenParser Char () IDE_Operation
parseIDEOp = noop <|> ins <|> del <?> "IDE_Operation"
  where noop = (const IDE_NoOp) <$> char 'n'
        ins  = IDE_Insert <$> (char 'i' *> parseInt) <*> (char 'a' *> anyChar)
        del  = IDE_Delete <$> (char 'd' *> parseInt)

-- | Parses coord field of CRDT_Node
parseRat :: GenParser Char () Rational
parseRat = (\n d -> read n % read d) <$> (many1 digit <?> "numerator")
                                     <*> (char '%' *> many1 digit <?> "denominator")
                                     <?> "Rational"

-- | Parses precedence filed of CRDT_Node
parseInt :: GenParser Char () Int
parseInt = read <$> many1 digit <?> "Int"


