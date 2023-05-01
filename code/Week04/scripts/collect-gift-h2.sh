#!/bin/bash

assets=/workspace/code/Week04/assets
keypath=/workspace/keys 

name="$1"       # eric or franny
collateral="$2" # utxo from eric or franny (hash + # + Index)
txin="$3"       # utxo we want to consume (TxHash + # + TxId)
slot="$4"       # number of the specific slot
pp="$assets/protocol-parameters.json"
body="$assets/collect-gift.txbody"
tx="$assets/collect-gift.tx"

# Query the protocol parameters 
cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file "$pp"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-script-file "$assets/parameterized-Mistery.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$assets/unit.json" \
    --tx-in-collateral "$collateral" \
    --required-signer-hash "$(cat $keypath/$name.pkh)" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --invalid-before "$slot" \
    --protocol-params-file "$pp" \
    --out-file "$body"

# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"