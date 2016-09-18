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
import qualified Data.Bimap as B
import System.Random.Shuffle

import qualified Data.ByteString.Lazy.Char8 as Char8
--import qualified Data.ByteString as B

import Utilities hiding (for)

--todo: figure out flags, https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling, https://hackage.haskell.org/package/base-4.9.0.0/docs/System-Console-GetOpt.html

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

getOptions :: Options -> (Int, String, String, String, String, String, String, String, String, String)
getOptions opt = let w = opt^.optWeek in
    (w, 
     opt^.optCSV, 
     opt^.optFrom, 
     opt^.optMatch, 
     opt^.optNoMatch, 
     case opt^.optOutput of {Nothing -> printf "heart/output_%d.txt" w; Just y -> y}, 
     opt^.optQuestions, 
     case opt^.optScript of {Nothing -> printf "script_%d" w; Just y -> y}, 
     case opt^.optInput of {Nothing -> printf "heart/data_%d.txt" (w-1); Just y -> y}, 
     case opt^.optData of {Nothing -> printf "heart/data_%d.txt" (w); Just y -> y})

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
      Option ['i'] ["input"] (maybeModOpt optInput "INPUT") "input data",
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

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
      case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage:" -- ic [OPTION...] files..."

main = do
  args <- getArgs
  (opts, _) <- compilerOpts args
  let (weekNum, form, from, matchEmail, nomatchEmail, outputFile, questionsFile, sname, inputFile, dataFile) = getOptions opts
  responses <- fmap fromRight $ parseFromFile csvFile form
  -- putStrLn (show responses)
  let l = (length responses) - 1
  matchTemplate <- readFile matchEmail
  nomatchTemplate <- readFile nomatchEmail
  questions <- readFile questionsFile
  writeFile sname "#!/bin/bash\n\n" 
  -- construct graph
  let nodeList = map (\i -> (i, (responses!!i)!!1)) [1..l]
  let nodeMap = M.fromList nodeList
  edges <- catchIOError
           (parseFromFile csvFile dataFile >>= (\case
               Left err -> ioError $ userError ""
               Right li -> return $ map (\[x,y] -> (read x,read y)) li))
           (\_ -> return [])
{-
  edges <- catchIOError
           (do 
             f <- readFile dataFile
             return $ decode $ Char8.pack f)
           (\_ -> return [])-}
  let negG = negativeGraphFromEdges nodeList edges
  let toFilterOut = filter (\i -> (\case {Just True -> True; _ -> False}) $
                                do
                                  date <- M.lookup weekNum weekMap
                                  return $ isInfixOf date ((responses!!i)!!4)
                           ) [1..l]
  let newG = negG & foldIterate G.delNode toFilterOut
  shuffled <- shuffleM [1..l] --randomness!
  let b = B.fromList $ zip [1..l] shuffled
  let shuffledG = mapNodes (\i -> b B.! i) newG
  let shuffledMatching = maximumMatching shuffledG
  let matching = map (\(i, j) -> (b B.!> i, b B.!> j)) shuffledMatching
  let allEdges = edges ++ matching
  let matchMap = M.fromList (matching ++ (map (\(x,y) -> (y,x)) matching))
  writeFile outputFile ""
  writeFile sname ""
  writeFile (sname++"_n") ""
  for [1..l] $ \i -> do
    let personFile = (printf "heart/week%d_%s.txt" weekNum (nodeMap M.! i))
    sname' <- case M.lookup i matchMap of
      Just j -> do
          writeFile personFile 
              (printf matchTemplate
                      ((responses!!i)!!1) --name
                      ((responses!!j)!!1) --partner's name
                      ((responses!!j)!!2) --partner's email
                      questions
              )
          appendFile outputFile (printf "%s, %s\n" ((responses!!i)!!1) ((responses!!j)!!1))
          return sname
      Nothing -> 
          if i `elem` toFilterOut 
          then writeFile personFile
                   (printf nomatchTemplate
                    ((responses!!i)!!1)) >> return (sname++"_n")
          else writeFile personFile "" >> 
               appendFile outputFile (printf "%s: UNMATCHED\n" ((responses!!i)!!1)) >> return "scratch"
    appendFile sname' (printf "cat \"%s\" | email -s \"Artichoke heart-to-heart\" -cc \"%s\" %s\n" personFile from ((responses!!i)!!2))
--    Char8.writeFile dataFile $ encode allEdges
    writeFile dataFile $ genCsvFile $ map (\(x,y) -> [show x, show y]) allEdges

mapNodes :: (Int -> Int) -> G.Gr a b -> G.Gr a b
mapNodes f g = 
    G.gmap (\(adjs, n, x, adjs') -> 
             (map (_2 %~ f) adjs, f n, x, map (_2 %~ f) adjs')) g 
