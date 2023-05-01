#!/bin/bash

assets=/workspace/code/Week04/assets
keypath=/workspace/keys 

name="$1"       # eric
collateral="$2" # utxo from eric (Hash + # + Index) 
txin="$3"       # TxHash from the vested funds     (utxo = TxHash + # + TxIx)
slot="$4"       # number of a slot corresponding to the deadline from the datum
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
    --tx-in-script-file "$assets/mistery1.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$assets/unit.json" \
    --tx-in-collateral "$collateral" \
    --required-signer-hash "$(cat $keypath/$name.pkh)" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --invalid-hereafter "$slot" \
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