#!/bin/bash

tmp=/workspace/code/Week08/tmp
txin=$1     # input for the transaction
echo "txin: $txin"

script=/workspace/code/Week08/assets/homework.plutus     # serialized script
script_stake_addr=$tmp/user1-script-stake.addr  # write the stake address
script_payment_addr=$tmp/user1-script.addr  # write the payment address
registration=$tmp/registration.cert # write the regiftration certificate
delegation=$tmp/delegation.cert # write the delegation certificate
pp=$tmp/protocol-params.json    # write the protocol parameters
body=$tmp/tx.txbody # write the unsigned transaction
signed=$tmp/tx.tx   # write the signed transaction

# set the node.socket for interactions with the running private testnet
export CARDANO_NODE_SOCKET_PATH=/workspace/cardano-private-testnet-setup/private-testnet/node-spo1/node.sock

# build a stake address 
cardano-cli stake-address build \
    --testnet-magic 42 \
    --stake-script-file $script \
    --out-file $script_stake_addr

echo "stake address: $(cat $script_stake_addr)"

# build a payment address from the stake address [payment key is the payment key from user1]
cardano-cli address build \
    --testnet-magic 42 \
    --payment-verification-key-file=/workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/payment1.vkey \
    --stake-script-file=$script \
    --out-file $script_payment_addr

echo "payment address: $(cat $script_payment_addr)"

# create a registration certificate
cardano-cli stake-address registration-certificate \
    --stake-script-file $script \
    --out-file $registration

# create a delegation certificate [stake pool id = id of the first pool that pops up in the list when querying the chain for stake pools]
cardano-cli stake-address delegation-certificate \
    --stake-script-file $script \
    --stake-pool-id=$(/workspace/code/Week08/scripts/query-stake-pools.sh | head -n 1) \
    --out-file $delegation

# query the protocol parameters
cardano-cli query protocol-parameters \
    --testnet-magic 42 \
    --out-file $pp

# build the transaction [change address ... the script payment address to make sure, that it will be funded... 
    # input comes from user1 and goes to this address, where user1 still has the spending power but the staking power goes to the script]
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 42 \
    --change-address $(cat $script_payment_addr) \
    --out-file $body \
    --tx-in $txin \
    --tx-in-collateral $txin \
    --certificate-file $registration \
    --certificate-file $delegation \
    --certificate-script-file $script \
    --certificate-redeemer-file /workspace/code/Week08/assets/unit.json \
    --protocol-params-file $pp

# sign the transaction (with the payment key of user1)
cardano-cli transaction sign \
    --testnet-magic 42 \
    --tx-body-file $body \
    --out-file $signed \
    --signing-key-file /workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/payment1.skey

# submit the transaction
cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file $signed
