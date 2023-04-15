{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NamedFieldPuns #-}

module PatchedSwap where


import           Plutus.V2.Ledger.Api      (ScriptContext (scriptContextTxInfo), PubKeyHash, Validator,
                                            mkValidatorScript, adaToken, adaSymbol, singleton,
                                            TxInfo (txInfoInputs), Credential (ScriptCredential), 
                                            Address (addressCredential), TxOut (txOutAddress))
import           Plutus.V2.Ledger.Contexts (valuePaidTo, TxInInfo, txInInfoResolved, ownHash)
import           PlutusTx                  (compile, unstableMakeIsData)
import           PlutusTx.Builtins         (BuiltinData, Integer)
import           PlutusTx.Prelude          (Bool (..), (==), traceIfFalse, (&&), (.), filter, ($))
import           Utilities                 (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data DatumSwap = DatumSwap
  { beneficiary :: PubKeyHash
  , price       :: Integer
  }
PlutusTx.unstableMakeIsData ''DatumSwap

{-# INLINABLE mkValidator #-}
mkValidator :: DatumSwap -> () -> ScriptContext -> Bool
mkValidator ds _ ctx = traceIfFalse "Hey! You have to pay the owner!" outputToBeneficiary &&
                       traceIfFalse "Hey! You can only consume one script UTxO per Tx!" consumeOnlyOneOutput
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        -- Check that the value paid to the beneficiary is equal to the price
        outputToBeneficiary :: Bool
        outputToBeneficiary = 
          valuePaidTo txInfo (beneficiary ds) == singleton adaSymbol adaToken (price ds)

        -- Helper function to get all inputs that are locked by this script
        getScriptInputs :: [TxInInfo]
        getScriptInputs = filter isScriptInput allInputs    -- gives a list of all utxos that are inputs for this script (should be only one)
            where
              allInputs :: [TxInInfo]
              allInputs = txInfoInputs txInfo     -- gets all Transaction inputs

              isScriptInput :: TxInInfo -> Bool
              isScriptInput i = case addressCredential . txOutAddress . txInInfoResolved $ i of
                                                                      -- gives the tx output
                                                       -- gets the address of the output  
                                    -- gets the Credential of the address    
                ScriptCredential vh -> vh == ownHash ctx -- Check that the validator required to spend this output is the current one
                -- takes a validatorHash (vh)
                -- The transaction that spends this output must include the validator script and be accepted by the validator.
                _                   -> False

        -- Check that we're consuming exactly one script output
        consumeOnlyOneOutput :: Bool
        consumeOnlyOneOutput = case getScriptInputs of
          [_] -> True   -- when there is a list of inputs for the transaction with only one utxo, then it validates
          _   -> False
        


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator


validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedValidator ||])