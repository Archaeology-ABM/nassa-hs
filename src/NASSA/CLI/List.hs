module NASSA.CLI.List where

import           NASSA.ReadYml
import           NASSA.Types

import           Data.List                  (transpose, intercalate)
import           Text.Layout.Table          (asciiRoundS, column, def,
                                             expandUntil, rowsG, tableString,
                                             titlesH)

data ListOptions = ListOptions { 
      _inPath :: FilePath
    , _optRaw :: Bool
    }

runList :: ListOptions -> IO ()
runList (ListOptions baseDir rawOutput) = do
    yamlCollection <- readNassaModuleCollection baseDir
    printModuleTable rawOutput yamlCollection

printModuleTable :: Bool -> [NassaYamlStruct] -> IO ()
printModuleTable rawOutput modules = do
    let tableH = ["id", "title", "first author", "language"]
        tableB = transpose [
              map _nassaYamlID modules
            , map ((\(ModuleTitle s) -> s) . _nassaYamlTitle) modules
            , map (_contributorName . head . _nassaYamlContributors) modules
            , map (show . _nassaYamlProgrammingLanguage) modules
            ]
    if rawOutput
    then putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else do
        let colSpecs = replicate (length tableH) (column (expandUntil 60) def def def)
        putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]