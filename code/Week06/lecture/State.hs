
module State where

import           Control.Monad.State (State, get, put, runState)

---------------------------------------------------------------------------------------------------
--------------------------------- HELPER FUNCTIONS/TYPES ------------------------------------------

-- Mock UTxO type
data UTxO = UTxO { owner :: String , value :: Integer }
    deriving (Show, Eq)

-- Mock blockchain type     => List of utxos
newtype Mock = Mock { utxos :: [UTxO] }
    deriving (Show, Eq)

-- Initial blockchain state => first utxo 
initialMockS :: Mock
initialMockS = Mock [ UTxO "Alice" 1000 ]

---------------------------------------------------------------------------------------------------
------------------------------------ WITHOUT STATE MONAD ------------------------------------------
        -- sender -> amount -> receiver -> current state of the bockchain -> (Bool, new state of the blockchain)
sendValue :: String -> Integer -> String -> Mock -> (Bool, Mock)
sendValue from amount to mockS =
    let senderUtxos = filter ((== from) . owner) (utxos mockS)  -- check for the utxos of the sender
        blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS) -- contains the whole blockchain without the utxos of the sender
        totalSenderFunds = sum (map value senderUtxos)  -- adding up all values of the senders utxos to see how much he owns
        receiverUtxo = UTxO to amount   -- create a utxo for the receiver
        senderChange = UTxO from (totalSenderFunds - amount)    -- create a utxo for the change of the sender (no fees)
    in if totalSenderFunds >= amount    -- when sender has enough fundes
        then (True, Mock $ [receiverUtxo] ++ [senderChange] ++ blockchainWithoutSenderUtxos) -- create a new state of the blockchain with: 
                            -- utxo of the receiver ++ utxo with the change for sender ++ blockchain wihtout all utxos of sender
        else (False, mockS)                                         


-- function to test the blockchain with multiple transactions
multipleTx :: (Bool, Mock)
multipleTx =
    -- (Bool, newstate) = sendValue sender amount receiver currentstate
    let (isOk,  mockS1) = sendValue "Alice" 100 "Bob"   initialMockS
        (isOk2, mockS2) = sendValue "Alice" 300 "Bob"   mockS1
        (isOk3, mockS3) = sendValue "Bob"   200 "Rick"  mockS2
    in (isOk && isOk2 && isOk3, mockS3)
-- try out:
--      cd code/Week06
--      cabal repl
--      multipleTx
-- check the states with: fst multipleTx       snd multipleTx
---------------------------------------------------------------------------------------------------
-------------------------------------- WITH STATE MONAD -------------------------------------------

-- newtype State s a = State { runState :: s -> (a, s) }

sendValue' :: String -> Integer -> String -> State Mock Bool
sendValue' from amount to = do  -- do ... startes the State Monad
    mockS <- get    -- get the initial state
    let senderUtxos = filter ((== from) . owner) (utxos mockS)
        blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS)
        totalSenderFunds = sum (map value senderUtxos)
        receiverUtxo = UTxO to amount
        senderChange = UTxO from (totalSenderFunds - amount)
    if totalSenderFunds >= amount
        then do
            put $ Mock $ [receiverUtxo] ++ [senderChange] ++ blockchainWithoutSenderUtxos   -- put ... changes the state 
            return True
        else return False

multipleTx' :: (Bool, Mock)
multipleTx' = runState (do -- stat machine automatically updates the state, we just handle the Bool (or wathever parameter we are interested in)
    isOk  <- sendValue' "Alice" 100 "Bob"
    -- <- ... saves the result of the funtcion on the right side to the variable on the left side
    isOk2 <- sendValue' "Alice" 300 "Bob"
    isOk3 <- sendValue' "Bob"   200 "Rick"
    return (isOk && isOk2 && isOk3))
    initialMockS


type Run a = State Mock a
