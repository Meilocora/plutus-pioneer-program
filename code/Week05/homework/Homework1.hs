{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy, POSIXTime,
                                       PubKeyHash, ScriptContext (scriptContextTxInfo),
                                       mkMintingPolicyScript, TxInfo (txInfoValidRange),
                                       to)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool,traceIfFalse, ($), (&&))
import           Prelude                   (IO, Show (show))
import           Utilities                 (wrapPolicy,
                                            writePolicyToFile)
import           Text.Printf               (printf)


-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.

                -- Parameter1 -> Parameter2 -> Redeemer -> Context -> Bool
{-# INLINABLE mkDeadlinePolicy #-}
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy pkh deadline () ctx =  
    traceIfFalse "missing signature" signedByBeneficiary &&
    traceIfFalse "deadline has already passed" deadlineNotReached                                         
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info pkh

    deadlineNotReached :: Bool
    deadlineNotReached = contains (to $ deadline) $ txInfoValidRange info


{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode deadline

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- serialize mintingpolicy to try and use lucid for it ... doenst work so far
-- saveSignedDeadlinePolicy :: PubKeyHash -> POSIXTime -> IO ()
--saveSignedDeadlinePolicy pkh deadline = writePolicyToFile (printf "assets/signed-%s.plutus" $ show (pkh deadline)) $ deadlinePolicy pkh deadline
