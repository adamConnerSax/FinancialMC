{-# LANGUAGE TemplateHaskell #-}
module FinancialMC.PersistentTypes
       (
         FilingStatus(..)
       )
       where


       
import Database.Persist.TH (derivePersistField)
import FinancialMC.Core.Tax (FilingStatus(..))


derivePersistField "FilingStatus"
