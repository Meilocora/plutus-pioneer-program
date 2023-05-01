#!/bin/bash

txin=$1     # user must provide the utxo when calling the script
amt=$(/workspace/code/Week08/scripts/query-stake-address-info-user1.sh | jq .[0].rewardAccountBalance)  
# uses the query tool to check the reward account balance and binds it to "amt" to withdraw, 
# because in Cardano only all rewards can be withdrawn at once
body=/workspace/code/Week08/tmp/tx.txbody  
signed=/workspace/code/Week08/tmp/tx.tx

echo "txin = $1"
echo "amt = $amt"

# export the node.socket for interactions with the testnet
export CARDANO_NODE_SOCKET_PATH=/workspace/cardano-private-testnet-setup/private-testnet/node-spo1/node.sock

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 42 \
    --change-address $(cat /workspace/cardano-private-testnet-setup/private-testnet/addresses/payment1.addr) \
    --out-file $body \
    --tx-in "$txin" \
    --withdrawal "$(cat /workspace/cardano-private-testnet-setup/private-testnet/addresses/staking1.addr)+$amt" \

cardano-cli transaction sign \
    --testnet-magic 42 \
    --tx-body-file $body \
    --out-file $signed \
    --signing-key-file /workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/payment1.skey \
    --signing-key-file /workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/staking1.skey
    # payment key, because a utxo from user1 should be spent
    # staking key, because rewards at the staking address from user1 should be collected


cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file $signed

# to use
    # ./withdraw-user1.sh <insert hash of utxo>#0
