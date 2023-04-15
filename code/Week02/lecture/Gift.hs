{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Gift where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (BuiltinData, compile)
import           Prelude              (IO)
import           Utilities            (writeValidatorToFile)

---------------------------------------------------------------------------------------------------
-------------------------------- ON-CHAIN CODE / VALIDATOR ----------------------------------------

-- This validator always succeeds
--                    Datum         Redeemer     ScriptContext
mkGiftValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkGiftValidator _ _ _ = ()
{-# INLINABLE mkGiftValidator #-}   {- to be able to put the function into the Oxford-Brackets -}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkGiftValidator ||])
                                                            -- Oxford-Brackets || take a function an give the source code of it
                                        -- compiles the code to plutus core as a syntax
                                    {-- $$ turns the syntax into source code -}
            -- produces a validator
---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/gift.plutus" validator