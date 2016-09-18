{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XTupleSections
 -XLambdaCase
 -XTemplateHaskell
#-}

module Main where
import System.Environment
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Traversable

import Text.ParserCombinators.Parsec
import Data.CSV
import Data.String.Utils
import Text.Printf
import Data.Char
import System.Directory
import System.IO
import System.IO.Error
import System.Console.GetOpt
import Control.Lens

import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.Query.Matchings
import Data.Binary

import qualified Data.ByteString.Lazy.Char8 as Char8

import Utilities hiding (for)

--todo: figure out flags, https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling, https://hackage.haskell.org/package/base-4.9.0.0/docs/System-Console-GetOpt.html
{-
data Flag
        = Week Int              -- -w
        | CSVFile String        -- -c
        -- | CC String             -- -d
        | From String           -- -f
        | MatchEmail String     -- -m
        | NoMatchEmail String   -- -n
        | Output String         -- -o
        | Questions String      -- -q
        | Help                  -- --help
        deriving (Eq,Ord,Enum,Show,Bounded)
-}

data Options = Options
   { _optWeek :: Int
   , _optCSV :: String
   , _optFrom :: String
   , _optMatch :: String
   , _optNoMatch :: String
   , _optOutput :: Maybe String
   , _optQuestions :: String
   , _optScript :: Maybe String
   , _optInput :: Maybe String
   , _optData :: Maybe String}

makeLenses ''Options

defaultOptions = Options
   { _optWeek = 1,
     _optCSV = "responses.csv",
     _optFrom = "holdenl@princeton.edu",
     _optMatch = "hearts.txt",
     _optNoMatch = "pass.txt",
     _optOutput = Nothing,
     _optQuestions = "questions.txt",
     _optScript = Nothing,
     _optInput = Nothing, 
     _optData = Nothing}

getOptions :: Options -> (Int, String, String, String, String, String, String, String, String)
getOptions opt = let w = opt^.optWeek in
    (w, opt^.optCSV, opt^.optFrom, opt^.optMatch, opt^.optNoMatch, case opt^.optOutput of {Nothing -> printf "hearts/output_%d.txt" w; Just y -> y}, opt^.optQuestions, case opt^.optScript of {Nothing -> printf "script_%d" w; Just y -> y}, case opt^.optInput of {Nothing -> printf "hearts/data_%d.txt" (w-1); Just y -> y}, case opt^.optData of {Nothing -> printf "hearts/data_%d.txt" (w); Just y -> y})

{-
makeOpt :: a -> String -> ArgDescr a 
makeOpt f = OptArg (\f opts -> \case 
                                   Nothing -> opts
                                   Just x -> f x opts)
-}
modOpt lens = OptArg (\my opts -> case my of
                                        Nothing -> opts
                                        Just x -> opts & lens .~ (read x))

maybeModOpt lens =  OptArg (\my opts -> case my of
                                        Nothing -> opts
                                        Just x -> opts & lens .~ Just (read x)) 
                 
options :: [OptDescr (Options -> Options)]
options =
    [ Option ['w'] ["week"] (modOpt optWeek "WEEK") "week number",
      Option ['c'] ["csv"] (modOpt optCSV "CSV") "csv file",
      Option ['f'] ["from"] (modOpt optFrom "FROM") "from email",
      Option ['m'] ["match"] (modOpt optMatch "MATCH") "match email text",
      Option ['n'] ["nomatch"] (modOpt optNoMatch "NOMATCH") "no-match email text",
      Option ['o'] ["output"] (maybeModOpt optOutput "OUTPUT") "output messages",
      Option ['q'] ["questions"] (modOpt optQuestions "QUESTIONS") "questions", 
      Option ['s'] ["script"] (maybeModOpt optScript "SCRIPT") "output script",
      Option ['i'] ["input"] (maybeModOpt optInputt "INPUT") "input data",
      Option ['d'] ["data"] (maybeModOpt optData "DATA") "output data"]      
      --Option ['h'] ["help"] (NoArg Help)]

weekMap :: M.Map Int String
weekMap = M.fromList [(1, "9/19-9/25"),
                      (2, "9/26-10/2"),
                      (3, "10/3-10/9"),
                      (4, "10/10-10/16"),
                      (5, "10/17-10/23"),
                      (6, "10/31-11/6"),
                      (7, "11/7-11/13"),
                      (8, "11/14-11/20"),
                      (9, "11/28-12/4"),
                      (10,"12/5-12/11"),
                      (11,"12/12-12/18")]

negativeGraphFromEdges :: [(Int, String)] -> [(Int, Int)] -> (G.Gr String ())
negativeGraphFromEdges nodeList li = 
    let 
        n = length nodeList
        pairs = [1..n] >>= (\x -> map (x,) [1..(x-1)])
        li' = map (\(x,y) -> (if y<x then (y,x) else (x,y))) li
        diff = pairs \\ li'
    in
      G.mkGraph nodeList (map (\(x,y) -> (x,y,())) diff)

{-
constructGraph :: [[String]] -> G.Gr String ()
constructGraph li = 
    let
        nodeList = map (\i -> (i, (li!!i)!!1)) [1..(length li)-1]
        edgeList = []
    in 
      G.mkGraph nodeList edgeList
-}

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
      case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage:" -- ic [OPTION...] files..."

main = do
  args <- getArgs
  (opts, _) <- compilerOps args
{-(w, opt^.optCSV, opt^.optFrom, opt^.optMatch, opt^.optNoMatch, case opt^.optOutput of {Nothing -> printf "output_%d.txt" w; Just y -> y}, opt^.optQuestions, case opt^.optScript of {Nothing -> printf "script_%d" w; Just y -> y}, case opt^.optInput of {Nothing -> printf "data_%d.txt" (w-1); Just y -> y}, case opt^.optInput of {Nothing -> printf "data_%d.txt" (w); Just y -> y})-}
  let (weekNum, form, from, matchEmail, nomatchEmail, outputFile, questionsFile, sname, inputFile, dataFile) = getOptions opts
  responses <- fmap fromRight $ parseFromFile csvFile form
  let l = length responses
  matchTemplate <- readFile matchEmail
  nomatchTemplate <- readFile nomatchEmail
  questions <- readFile questionsFile
  writeFile sname "#!/bin/bash\n\n" 
  -- construct graph
  let nodeList = map (\i -> (i, (responses!!i)!!1)) [1..l]
  let nodeMap = M.fromList nodeList
  edges <- catchIOError
           (do 
             f <- readFile dataFile
             return $ decode $ Char8.pack f)
           (\_ -> return [])
  let negG = negativeGraphFromEdges nodeList edges
  let toFilterOut = filter (\i -> (\case {Just True -> True; _ -> False}) $
                                do
                                  date <- M.lookup weekNum weekMap
                                  return $ isInfixOf date ((responses!!i)!!4)
                           ) [1..l]
  let newG = negG & foldIterate G.delNode toFilterOut
  let matching = maximumMatching newG
  let allEdges = edges ++ matching
  let matchMap = M.fromList (matching ++ (map (\(x,y) -> (y,x)) matching))
  for [1..l] $ \i -> do
    let personFile = (printf "week%d_%s.txt" weekNum (nodeMap M.! i))
    case M.lookup i matchMap of
      Just j -> do
          writeFile personFile 
              (printf matchTemplate
                      ((responses!!i)!!1) --name
                      ((responses!!j)!!1) --partner's name
                      ((responses!!j)!!2) --partner's email
              )
          writeFile outputFile (printf "%s, %s" ((responses!!i)!!1) ((responses!!j)!!1))
      Nothing -> 
          if i `elem` toFilterOut 
          then writeFile personFile
                   (printf matchTemplate
                    ((responses!!i)!!i)) --name
          else writeFile personFile "" >> 
               writeFile outputFile (printf "%s: UNMATCHED" ((responses!!i)!!1))
    appendFile sname (printf "cat \"%s\" | email -s \"Artichoke heart-to-heart\" -cc \"%s\" %s\n" personFile from ((responses!!i)!!2)) 

