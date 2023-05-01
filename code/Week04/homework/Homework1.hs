{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Data.Maybe                (fromJust)
import           Plutus.V1.Ledger.Interval (contains, to,)
import           Plutus.V2.Ledger.Api      (BuiltinData, POSIXTime, toBuiltin,
                                            POSIXTimeRange, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx                  (compile, unstableMakeIsData)
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (&&), (+),
                                            (||))
import           Prelude                   (IO, String, FilePath)
import           Utilities                 (Network, posixTimeFromIso8601,
                                            printDataToJSON, bytesFromHex,
                                            validatorAddressBech32,
                                            wrapValidator, writeValidatorToFile, writeDataToFile)

---------------------------------------------------------------------------------------------------
------------------------------------------ PROMPT -------------------------------------------------

{-
1- Figure out what this (already finished) validator does using all the tools at your disposal.
2- Write the off-chain code necessary to cover all possible interactions with the validator using
   the off-chain tool of your choosing.

HINT: If you get stuck, take a look at Week03's lecture
-}

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data MisteryDatum = MisteryDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''MisteryDatum

{-# INLINABLE mkMisteryValidator #-}
                    -- Datum       -> Redeemer -> ctx     -> Bool
mkMisteryValidator :: MisteryDatum -> () -> ScriptContext -> Bool
mkMisteryValidator dat () ctx =
    traceIfFalse "Benificiary1 did not sign or to late" checkCondition1 ||
    traceIfFalse "Benificiary2 did not sign or is to early" checkCondition2
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        txValidRange :: POSIXTimeRange
        txValidRange  = txInfoValidRange txInfo

        checkCondition1 :: Bool     -- signed by beneficiray 1 & deadline not reached
        checkCondition1 = txSignedBy txInfo (beneficiary1 dat) &&
                          contains (to (deadline dat)) txValidRange

        checkCondition2 :: Bool     -- signed by beneficiary 2 & deadline reached
        checkCondition2 = txSignedBy txInfo (beneficiary2 dat) &&
                          contains (from (1 + deadline dat)) txValidRange

{-# INLINABLE  mkWrappedMisteryValidator #-}
mkWrappedMisteryValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedMisteryValidator = wrapValidator mkMisteryValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedMisteryValidator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/mistery1.plutus" validator

misteryAddressBech32 :: Network -> String
misteryAddressBech32 network = validatorAddressBech32 network validator

printMisteryDatumJSON :: PubKeyHash -> PubKeyHash -> String -> IO ()
printMisteryDatumJSON pkh1 pkh2 time = printDataToJSON $ MisteryDatum
    { beneficiary1 = pkh1
    , beneficiary2 = pkh2
    , deadline    = fromJust $ posixTimeFromIso8601 time
    }

{-
Try and print the datum as a json, so the script can use it as an intput for the transaction 

pkh1 :: PubKeyHash
pkh1 = "606285429751e57aa07ab51e0ce64f20cdfda2b0d165b89a2fea77a634"

pkh2 :: PubKeyHash
pkh2 = "60d8f1e340f84f4495a636fb691fc322e08da0672c9bdd063fa8e00187"

saveDatum :: PubKeyHash -> PubKeyHash -> String -> FilePath -> a -> IO ()
saveDatum pkh11 pkh22 time = writeDataToFile "./assets/datum.plutus" printMisteryDatumJSON (pkh11 pkh22 time)

-}