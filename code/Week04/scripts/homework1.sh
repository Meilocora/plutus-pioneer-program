# create vesting datum:

# cd code/Week04
# cabal repl
# import Utilities & Plutus.V2.Ledger.Api & Homework1  [danach :set prompt "> "]
# :set -XOverloadedStrings 		[to use strings in the terminal as bytestrings]
# find out payment credential with: 
#   cardano-cli address info --address <address>
# convert the payment credential (base16) text into an actual PubKeyHash
#	pkh1 = PubKeyHash $ toBuiltin $ bytesFromHex "606285429751e57aa07ab51e0ce64f20cdfda2b0d165b89a2fea77a634"
#	pkh1 = PubKeyHash $ toBuiltin $ bytesFromHex "60d8f1e340f84f4495a636fb691fc322e08da0672c9bdd063fa8e00187"
# print a vesting datum
#	printMisteryDatumJSON pkh1 pkh2 "2023-04-11T13:00:00Z"


#!/bin/bash

assets=/workspace/code/Week04/assets
keypath=/workspace/keys
name="$1"
txin="$2"  # TxHash     (utxo = TxHash + # + TxIx)
body="$assets/mistery1.txbody"
tx="$assets/mistery1.tx"

# Build gift address 
cardano-cli address build \
    --payment-script-file "$assets/mistery1.plutus" \
    --testnet-magic 2 \
    --out-file "$assets/mistery1.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-out "$(cat "$assets/mistery1.addr") + 3000000 lovelace" \
    --tx-out-inline-datum-file "$assets/datum.json" \
    --change-address "$(cat "$keypath/$name.addr")" \
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