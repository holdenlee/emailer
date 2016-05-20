{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
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


import Utilities hiding (for)

main = do
  args <- getArgs
  let subj = fromMaybe "" $ args `mindex` 0
  let from = fromMaybe "holdenl@princeton.edu" $ args `mindex` 1
  let cc = fromMaybe "" $ args `mindex` 2
  let filePath = fromMaybe "./" $ args `mindex` 3
  let gradeFile = fromMaybe "grades.csv" $ args `mindex` 4
  let emailFile = fromMaybe "email.txt" $ args `mindex` 5
  let emailSuffix = fromMaybe "@princeton.edu" $ args `mindex` 6
  let sname = fromMaybe "email_grades" $ args `mindex` 7
  grades <- fmap fromRight $ parseFromFile csvFile gradeFile
  template <- readFile emailFile
  let l = length grades
  let cols = length (grades!!0)
  dirContents <- getDirectoryContents filePath
  let lcFiles = M.fromList $ map (\x -> (map toLower x, x)) dirContents
  writeFile sname "#!/bin/bash\n\n" 
  for grades (\row -> do
    let lastName = row!!0
    let firstName = row!!1
    let id = row!!2
    let [hw, final, avg] = map (row!!) [cols-3, cols-2, cols-1]
    let scores = drop 3 row
    let scoreString = intercalate "|" scores
    let email = template |> foldIterate (uncurry replace) 
                [("NAME", firstName++" "++lastName),
                 ("HW", hw),
                 ("EXAM",final),
                 ("AVG",avg),
                 ("SCORES",scoreString)]
    let personFile = lastName++"_"++firstName++".txt"
    writeFile personFile email
    let firstMatches = filter ((map toLower firstName) `isInfixOf`) (M.keys lcFiles)
    let lastMatches = filter ((map toLower lastName) `isInfixOf`) (M.keys lcFiles)
    correct <- do
      let u = firstMatches `union` lastMatches
      let i = firstMatches `intersect` lastMatches
      if length u == 1 
         then return (u!!0)
         else if length i == 1
         then return (i!!0)
         else do
           putStrLn ("Please enter file name for "++firstName++" "++lastName)
           mapM putStrLn u
           hFlush stdout
           getLine
    let fname = fromMaybe correct $ M.lookup correct lcFiles
    appendFile sname (printf "cat \"%s\" | email -a \"%s\" -s \"%s\" -cc \"%s,%s\" %s\n" personFile (filePath++fname) subj cc from (id++emailSuffix)))


