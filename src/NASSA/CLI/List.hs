module NASSA.CLI.List where

import           NASSA.ReadYml
import           NASSA.Types

import           Data.List         (intercalate, transpose)
import           Text.Layout.Table (asciiRoundS, column, def, expandUntil,
                                    rowsG, singleCutMark, tableString, titlesH)

data ListOptions = ListOptions {
      _inPath :: FilePath
    , _optRaw :: Bool
    }

runList :: ListOptions -> IO ()
runList (ListOptions baseDir rawOutput) = do
    yamlCollection <- readNassaModuleCollection baseDir
    printModuleTable rawOutput yamlCollection

printModuleTable :: Bool -> [NassaModule] -> IO ()
printModuleTable rawOutput modules = do
    let yamlStruct = map (\(NassaModule (_,x)) -> x) modules
    let tableH = ["id", "title", "first author", "implementations"]
        tableB = transpose [
              map (show . _nassaYamlID) yamlStruct
            , map (show . _nassaYamlTitle) yamlStruct
            , map (_contributorName . head . _nassaYamlContributors) yamlStruct
            , map (intercalate ";" . map (show . _implementationLanguage) . _nassaYamlImplementations) yamlStruct
            ]
    if rawOutput
    then putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else do
        let columnSetting = column (expandUntil 40) def def (singleCutMark "...")
            colSpecs = replicate (length tableH) columnSetting
        putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
