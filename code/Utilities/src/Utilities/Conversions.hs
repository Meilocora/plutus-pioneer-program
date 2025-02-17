{-# LANGUAGE GADTs #-}

module Utilities.Conversions
  ( Network (..)
  , validatorHash
  , validatorHash'
  , policyHash
  , currencySymbol
  , validatorAddressBech32
  , posixTimeFromIso8601
  , posixTimeToIso8601
  , bytesFromHex
  , bytesToHex
  , tryReadAddress
  ) where

import qualified Cardano.Api                 as Api
import           Cardano.Api.Shelley         (Address (..))
import qualified Cardano.Api.Shelley         as Api
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.BaseTypes    (CertIx (..), Network (..),
                                              TxIx (..))
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base16      as BS16
import           Data.Text                   (pack)
import qualified Data.Text                   as Text
import qualified Data.Time.Clock.POSIX       as Time
import qualified Data.Time.Format.ISO8601    as Time
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import           Plutus.V2.Ledger.Api        (CurrencySymbol (CurrencySymbol),
                                              MintingPolicy,
                                              MintingPolicyHash (MintingPolicyHash),
                                              POSIXTime, Validator, ValidatorHash (ValidatorHash))
import qualified Plutus.V2.Ledger.Api        as Plutus
import           PlutusTx.Builtins           (toBuiltin)
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))
import           Utilities.Serialise         (policyToScript, validatorToScript)

hashScript :: Api.PlutusScript Api.PlutusScriptV2 -> Api.ScriptHash
hashScript = Api.hashScript . Api.PlutusScript Api.PlutusScriptV2

validatorHash :: Validator -> Api.ScriptHash
validatorHash = hashScript . validatorToScript

validatorHash' :: Validator -> Plutus.ValidatorHash
validatorHash' = Plutus.ValidatorHash . BuiltinByteString . Api.serialiseToRawBytes . hashScript . validatorToScript

policyHash :: MintingPolicy -> MintingPolicyHash
policyHash = MintingPolicyHash . BuiltinByteString . Api.serialiseToRawBytes . hashScript . policyToScript

currencySymbol :: MintingPolicy -> CurrencySymbol
currencySymbol = CurrencySymbol . BuiltinByteString . Api.serialiseToRawBytes . hashScript . policyToScript

validatorAddressBech32 :: Network -> Validator -> String
validatorAddressBech32 network v =
    Text.unpack $
    Api.serialiseToBech32 $
    Api.ShelleyAddress
        network
        (ScriptHashObj $ Api.toShelleyScriptHash $ validatorHash v)
        StakeRefNull

posixTimeFromIso8601 :: String -> Maybe POSIXTime
posixTimeFromIso8601 s = do
    t <- Time.formatParseM Time.iso8601Format s
    let seconds = Time.utcTimeToPOSIXSeconds t
        milliSeconds = round $ 1000 * seconds :: Integer
    return $ fromInteger milliSeconds

posixTimeToIso8601 :: POSIXTime -> String
posixTimeToIso8601 t =
    Time.formatShow Time.iso8601Format $ Time.posixSecondsToUTCTime $ fromRational $ toRational t / 1000

bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = either error id . BS16.decode

bytesToHex :: BS.ByteString -> BS.ByteString
bytesToHex = BS16.encode

-- The following functions try to convert a String addres into a plutus address --

-- convert the payment part of the address
credentialLedgerToPlutus :: Ledger.Credential a StandardCrypto -> Plutus.Credential
    -- if its a script address:
credentialLedgerToPlutus (ScriptHashObj (ScriptHash h)) = Plutus.ScriptCredential $ Plutus.ValidatorHash $ toBuiltin $ hashToBytes h
    -- if its a PubKey address:
credentialLedgerToPlutus (KeyHashObj (KeyHash h))       = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ toBuiltin $ hashToBytes h

-- convert the staking part of the address
stakeReferenceLedgerToPlutus :: Ledger.StakeReference StandardCrypto -> Maybe Plutus.StakingCredential
    -- "base case" when there is a staking address
stakeReferenceLedgerToPlutus (StakeRefBase x)                                       =
    Just $ StakingHash $ credentialLedgerToPlutus x
    -- in case of a pointer credential, that points to a specific point of the blockchain (never used)
stakeReferenceLedgerToPlutus (StakeRefPtr (Ptr (Api.SlotNo x) (TxIx y) (CertIx z))) =
    Just $ StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
    -- if the address doesnt use staking (no DCert yet)
stakeReferenceLedgerToPlutus StakeRefNull                                           =
    Nothing

-- Try to read the String address (only works if it deserializes into a shelley era address)
tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case Api.deserialiseAddress Api.AsAddressAny $ pack x of -- deserialize a String address into a Plutus.Api address
    Nothing                                          -> Nothing
    Just (Api.AddressByron _)                        -> Nothing     -- Fail because plutus only deals with shelley era addresses
    Just (Api.AddressShelley (ShelleyAddress _ p s)) -> Just Plutus.Address
                                            -- p ... payment part, s... staking part of the address
        { Plutus.addressCredential        = credentialLedgerToPlutus p
        , Plutus.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }

