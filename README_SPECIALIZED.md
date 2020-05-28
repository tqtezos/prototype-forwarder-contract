
# Building

To build, run:

```bash
stack build
```

# Origination

The CLI tool's `print-specialized-fa12` command can be used to print the contract:

```bash
❯❯❯ stack exec -- prototype-forwarder-contract print-specialized-fa12 --help
Usage: prototype-forwarder-contract print-specialized-fa12 --central-wallet ADDRESS
                                                           --fa12-address ADDRESS
                                                           [-o|--output FILEPATH]
                                                           [--oneline]
  Dump FA1.2 Token Forwarder contract, specialized to paricular addresses, in
  the form of Michelson code

Available options:
  -h,--help                Show this help text
  --central-wallet ADDRESS Address of central wallet
  --fa12-address ADDRESS   Address of FA1.2 Token contract
  -o,--output FILEPATH     Output file
  --oneline                Force single line output
```


# Usage

## Overview

The forwarder contract's only entrypoint allows any user to trigger a "flush" of
a given `nat`ural number of tokens from the forwarder to a "central wallet":

```
+-------------+
|             |
|  Any user   |
|             |
+--+----------+
   |
   |
   | Call with: 42
   |
   |
+--v----------+                            +-------+
|             |      Transfer 42 tokens    |       |
|  Forwarder  +--------------------------->+ FA1.2 |
|             |      From: Forwarder       |       |
+-------------+      To: Central Wallet    +-------+
```

The central wallet is fixed upon origination.


## Setup

Originate a dummy FA1.2 that has the appropriate `transfer`
entrypoint and fails on all inputs:

```bash
❯❯❯ tezos-client --wait none originate contract DummyManagedLedger \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat dummy_fa12.tz | tr -d '\n')" \
  --burn-cap 0.378

Waiting for the node to be bootstrapped before injection...
Current head: BLYqnrwfpmKF (timestamp: 2020-05-28T18:51:47-00:00, validation: 2020-05-28T18:52:08-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 12214 units (will add 100 for safety)
Estimated storage: 378 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ong5uJLknnycztcM2VTZ8faeAGPJEhaAQbNo7fiLQKPGyAJ8qYZ'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ong5uJLknnycztcM2VTZ8faeAGPJEhaAQbNo7fiLQKPGyAJ8qYZ to be included --confirmations 30 --branch BLYqnrwfpmKFscvGCqhC5SkZKbdLvNhpQ9MTVPat5q4UrP6Jp41
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001582
    Expected counter: 623977
    Gas limit: 12314
    Storage limit: 398 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.001582
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,216) ... +ꜩ0.001582
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter
            (or (pair %transfer (address :from) (pair (address :to) (nat :value)))
                (pair %approve (address :spender) (nat :value))) ;
          storage unit ;
          code { FAILWITH } }
        Initial storage: Unit
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1B73ePQbxBHFxKgKjHE46Y6WnabnNu5i7f
        Storage size: 121 bytes
        Paid storage size diff: 121 bytes
        Consumed gas: 12214
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.121
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1B73ePQbxBHFxKgKjHE46Y6WnabnNu5i7f originated.
Contract memorized as DummyManagedLedger.
```


Originate a copy of the specialized forwarder that points to the dummy FA1.2
and uses Alice's address as the central wallet:

```bash
❯❯❯ tezos-client --wait none originate contract FA12Forwarder \
  transferring 0 from $BOB_ADDRESS running \
  "$(stack exec -- prototype-forwarder-contract print-specialized-fa12 \
  --central-wallet $ALICE_ADDRESS \
  --fa12-address KT1B73ePQbxBHFxKgKjHE46Y6WnabnNu5i7f)" \
  --burn-cap 0.493
Waiting for the node to be bootstrapped before injection...
Current head: BL8qgEypo1DM (timestamp: 2020-05-28T19:04:27-00:00, validation: 2020-05-28T19:04:29-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 15042 units (will add 100 for safety)
Estimated storage: 493 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooJYSs4R8dQkuf4mXumExcB57r4ZG6g5HWnHaveSqshJischitT'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooJYSs4R8dQkuf4mXumExcB57r4ZG6g5HWnHaveSqshJischitT to be included --confirmations 30 --branch BL8qgEypo1DMw7ngmXSVhsjkaamb4Yb4oQ34H7EJ5NLpeMqMvDc
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.00198
    Expected counter: 623978
    Gas limit: 15142
    Storage limit: 513 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.00198
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,216) ... +ꜩ0.00198
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter nat ;
          storage unit ;
          code { CAR ;
                 PUSH address "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" ;
                 PAIR ;
                 SELF ;
                 ADDRESS ;
                 PAIR ;
                 DIP { PUSH address "KT1B73ePQbxBHFxKgKjHE46Y6WnabnNu5i7f" ;
                       CONTRACT %transfer (pair address (pair address nat)) ;
                       IF_NONE { PUSH string "Internal: not FA1.2" ; FAILWITH } { PUSH mutez 0 } } ;
                 TRANSFER_TOKENS ;
                 DIP { NIL operation } ;
                 CONS ;
                 DIP { UNIT } ;
                 PAIR } }
        Initial storage: Unit
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1FgsP8zrKgG9pNW6vwK98dUB7BvjkQmqeD
        Storage size: 236 bytes
        Paid storage size diff: 236 bytes
        Consumed gas: 15042
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.236
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1FgsP8zrKgG9pNW6vwK98dUB7BvjkQmqeD originated.
Contract memorized as FA12Forwarder.
```


Call the forwarder with the parameter `42`:

```bash
❯❯❯ tezos-client --wait none transfer 0 from $BOB_ADDRESS to KT1FgsP8zrKgG9pNW6vwK98dUB7BvjkQmqeD --arg 42

Waiting for the node to be bootstrapped before injection...
Current head: BLKvZAnwhm9d (timestamp: 2020-05-28T19:05:27-00:00, validation: 2020-05-28T19:05:54-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0
    Expected counter: 623979
    Gas limit: 1040000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1FgsP8zrKgG9pNW6vwK98dUB7BvjkQmqeD
      Parameter: 42
      This transaction was BACKTRACKED, its expected effects (as follow) were NOT applied.
      Updated storage: Unit
      Storage size: 236 bytes
      Consumed gas: 26359
    Internal operations:
      Transaction:
        Amount: ꜩ0
        From: KT1FgsP8zrKgG9pNW6vwK98dUB7BvjkQmqeD
        To: KT1B73ePQbxBHFxKgKjHE46Y6WnabnNu5i7f
        Entrypoint: transfer
        Parameter: (Pair 0x014df03b5b4530dd0ac5ca1b2f4754ba4bd8e97b2100
                         (Pair 0x00003b5d4596c032347b72fb51f688c45200d0cb50db 42))
        This operation FAILED.

Runtime error in contract KT1B73ePQbxBHFxKgKjHE46Y6WnabnNu5i7f:
  1: { parameter
  2:     (or (pair %transfer (address :from) (pair (address :to) (nat :value)))
  3:         (pair %approve (address :spender) (nat :value))) ;
  4:   storage unit ;
  5:   code { FAILWITH } }
At line 5 characters 9 to 17,
script reached FAILWITH instruction
with
  (Pair (Left (Pair 0x014df03b5b4530dd0ac5ca1b2f4754ba4bd8e97b2100
                    (Pair 0x00003b5d4596c032347b72fb51f688c45200d0cb50db 42)))
        Unit)
Fatal error:
  transfer simulation failed
```

As expected, the dummy FA1.2 fails with a transfer of `42` tokens from the
forwarder contract to `$ALICE_ADDRESS`.

