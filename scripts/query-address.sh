#!/bin/bash
cardano-cli query utxo --address "$1" --testnet-magic 2

# use like this:
# scripts/query-address.sh $(cat keys/eric.addr)