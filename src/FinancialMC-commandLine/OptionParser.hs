module OptionParser 
       (
         FinMCOptions(..)
       , finMCOptionParser
       ) where


import FinancialMC.Base (LogLevel)


import Options.Applicative (help,many,argument,str,metavar,info,header,fullDesc,progDesc,helper,long,strOption,value,short,
                           optional,option,auto,flag,ParserInfo,ReadM,execParser,Parser,readerError)

import Data.Word (Word64)
import Data.List.Split (splitOn)
import Data.Monoid ((<>))

data FinMCOptions = FinMCOptions 
                    { optConfigFile::FilePath,
                      optSchemaPath::Maybe FilePath,
                      optYears::Int,
                      optPaths::Int,
                      optSeed::Maybe Word64,
                      optBins::Int,
                      optQuantiles::Int,
                      optOutPath :: Maybe FilePath, 
                      optLogLevels::[LogLevel],
                      optShowFinalStates::Bool,
                      optSingleThreaded::Bool,
                      optConfigs::[String]
                    } deriving (Show)
                   


parseBoundedEnum::(Enum e, Bounded e, Show e) => String -> ReadM e
parseBoundedEnum s = do
  let enumValues = [minBound..]  
      valueMap = (\x -> (show x,x)) <$> enumValues
      val = lookup s valueMap
      r (Nothing) = readerError ("Couldn't parse \"" ++ s ++ "\" as a member of " ++ show enumValues)  
      r (Just x) = return x 
  r val
      
  
  
  
parseListOfBoundedEnum::(Enum e, Bounded e,Show e)=> String -> String -> ReadM [e]
parseListOfBoundedEnum sep s = mapM parseBoundedEnum (splitOn sep s)     

parseLogLevel::Parser [LogLevel]
parseLogLevel = option (str >>= parseListOfBoundedEnum ",") 
                (long "logLevels" <> 
                 short 'l' <> 
                 value [] <>
                 metavar "LOGLEVELS" <> 
                 help "Log LOGLEVELS messages to stderr")  


finMCOptionParser'::Parser FinMCOptions
finMCOptionParser' = FinMCOptions 
               <$> strOption (long "config_file"  
                                <> short 'c'
                                <> metavar "CONFIG"
                                <> help "XML file with configuration details.")
               <*> (optional $ strOption (long "schema_dir"
                                          <> short 's'
                                          <> metavar "SCHEMA_DIR"
                                          <> help "Optional.  Find RelaxNG schemas in this directory"))
               <*> option auto (long "years" 
                                <> short 'y' 
                                <> value 40 
                                <> metavar "YEARS" 
                                <> help "Run each simulation path for YEARS years." ) 
               <*> option auto (long "paths" 
                                <> short 'p' 
                                <> value 100 
                                <> metavar "PATHS" 
                                <> help "Simulate using PATHS paths.")
               <*> (optional $ option auto (long "seed" 
                                            <> metavar "SEED" 
                                            <> help "If present, use to seed RNG.  Otherwise get psuedo-random seed via IO."))
               <*> option auto (long "bins" 
                                <> short 'b'
                                <> value 30
                                <> metavar "BINS"
                                <> help "Number of bins for result histogram.")
               <*> option auto (long "quantiles"
                                <> short 'q'
                                <> value 5
                                <> metavar "QUANTILES"
                                <> help "Number of quantiles for selection of example paths")
               <*> (optional $ strOption (long "output_path" 
                                          <> short 'o' 
                                          <> metavar "PATH" 
                                          <> help "Save output files in PATH."))
               <*> parseLogLevel
               <*> flag False True (long "showFinalStates" 
                                    <> help "if present, show each paths final state.") 
               <*> flag False True (long "singleThreaded" 
                                    <> help "if present, don't use Monad Parallel for execution.") 
               <*> many (argument str (metavar "Config names"))  

               
finMCOptionParser::ParserInfo FinMCOptions               
finMCOptionParser = info (helper <*> finMCOptionParser') 
                    (fullDesc 
                     <> progDesc "Simulate personal finances." 
                     <> header "Personal Finance Simulator")
               
test::IO () 
test = execParser opts >>= print  where
  opts = info (helper <*> finMCOptionParser') (fullDesc <> progDesc "Test option parsing" <> header "A test for option parsing")
  
  
main::IO ()
main = test
