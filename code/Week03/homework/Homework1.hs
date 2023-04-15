{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Data.Maybe                (fromJust)
import           Plutus.V1.Ledger.Interval (contains, after, before, to)
import           Plutus.V2.Ledger.Api      (BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx                  (compile, unstableMakeIsData)
import           PlutusTx.Prelude          (Bool(True, False), traceIfFalse, ($), (&&), (||), otherwise)
import           Prelude                   (IO, String)
import           Utilities                 (Network, posixTimeFromIso8601, wrapValidator,
                                            printDataToJSON, validatorAddressBech32,
                                            writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx
    | signedByBeneficiaryOne && deadlineNotReached  = True
    | signedByBeneficiaryTwo && deadlineReached     = True
    | otherwise                                     = False

    {-traceIfFalse "Beneficary 1 signed too late" signedByBeneficiaryOne && deadlineNotReached ||
    traceIfFalse "Beneficary 2 signed too early" signedByBeneficiaryTwo && deadlineReached -}
  
       
    where 
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signedByBeneficiaryOne :: Bool
        signedByBeneficiaryOne = txSignedBy info $ beneficiary1 dat

        signedByBeneficiaryTwo :: Bool
        signedByBeneficiaryTwo = txSignedBy info $ beneficiary2 dat

        deadlineReached :: Bool
        deadlineReached = before (deadline dat) $ txInfoValidRange info

        deadlineNotReached :: Bool
        deadlineNotReached = contains (to $ deadline dat) $ txInfoValidRange info


{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
