# prototype-forwarder-contract

## Building

To build without `DS Token`:

```bash
stack build
```

To build with `DS Token`:

```bash
stack build --stack-yaml stack-dstoken.yaml --flag prototype-forwarder-contract:dstoken
```

## Introduction

A prototype sub-token forwarder contract:

  1. investor has sub-tokens
  2. investor assigned a forwarder contract to send sub-tokens to
  3. investor transfers sub-tokens to forwarder contract using sub-token contract
  4. investor calls forwarder contract with number of transferred sub-tokens
  5. forwarder contract:
    a. transfers sub-tokens to central wallet
    b. calculates cost of transfers, calls tezos-wallet with total fee cost
  6. tezos-wallet ensures that it's being called by a valid forwarder and then transfers fee cost in XTZ to investor


To verify a deployed copy of the forwarder, e.g. at `$ALICE_FORWARDER`:

```bash
forwarder-contract ❯❯❯ alpha-client get script code for $ALICE_FORWARDER | stack exec -- dstoken-forwarder-contract parse --central-wallet "$BOB_ADDRESS" --dstoken-address "$DSTOKEN_ADDRESS"

Contract verified successfully!
```

If the contract is not verified, you'll get an error:

```bash
~/C/forwarder-contract ❯❯❯ alpha-client get script code for $DSTOKEN_ADDRESS | ./stack exec -- dstoken-forwarder-contract parse --central-wallet "$BOB_ADDRESS" --dstoken-address "$DSTOKEN_ADDRESS"

Unexpected parameter type: ..
ExitFailure 1
```


## Validated Expiring Forwarder Contract

# Intro

## Originating the contract

The CLI `print-validated-expiring` command accepts `--help`:

```bash
❯❯❯ ./stack exec -- dstoken-forwarder-contract print-validated-expiring --help

Usage: dstoken-forwarder-contract print-validated-expiring --central-wallet ADDRESS
                                                           --dstoken-address ADDRESS
                                                           [-o|--output FILEPATH]
                                                           [--oneline]
  Dump DS Token Forwarder contract, specialized to paricular addresses, with
  sender validating and expiration, in the form of Michelson code

Available options:
  -h,--help                Show this help text
  --central-wallet ADDRESS Address of central wallet
  --dstoken-address ADDRESS
                           Address of DS Token contract
  -o,--output FILEPATH     Output file
  --oneline                Force single line output
```

We'll set the `central-wallet` address and `dstoken-address` to `$ALICE_ADDRESS`
for testing:

```bash
❯❯❯ ./stack exec -- dstoken-forwarder-contract print-validated-expiring \
  --central-wallet $ALICE_ADDRESS \
  --dstoken-address $ALICE_ADDRESS \
  --oneline
```

Next, we'll need to specify the initial storage:

```bash
❯❯❯ ./stack exec -- dstoken-forwarder-contract initial-storage-validated-expiring --help

Usage: dstoken-forwarder-contract initial-storage-validated-expiring --dstoken-address ADDRESS
                                                                     --whitelisted-investors [STRING]
                                                                     --token-limit NATURAL
                                                                     --expiration-date TIMESTAMP
                                                                     [-o|--output FILEPATH]
  Dump initial storage value for validated-expiring forwarder

Available options:
  -h,--help                Show this help text
  --dstoken-address ADDRESS
                           Address of DS Token contract
  --whitelisted-investors [STRING]
                           List of Strings representing whitelisted-investors.
  --token-limit NATURAL    Natural number representing token-limit.
  --expiration-date TIMESTAMP
                           Timestamp number representing expiration-date.
  -o,--output FILEPATH     Output file
```

We'll set:
- `token-limit` (the total number of tokens that can be forwarded) to `10`
- `expiration-date` to `9:00 next Fri`, UTC

```bash
❯❯❯ ./stack exec -- dstoken-forwarder-contract initial-storage-validated-expiring \
  --dstoken-address $ALICE_ADDRESS \
  --whitelisted-investors '["alice"]' \
  --token-limit 10 \
  --expiration-date "$(date -u -d '9:00 next Fri' +%FT%T.%NZ)"

Pair (Pair Unit (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" (Pair { "alice" } 10))) "2020-01-10T09:00:00Z"
```

Then, we can originate the forwarder:

```bash
❯❯❯ alpha-client --wait none originate contract ValidatedForwarder \
  transferring 0 from $ALICE_ADDRESS running \
  "$(./stack exec -- dstoken-forwarder-contract print-validated-expiring \
  --central-wallet $ALICE_ADDRESS \
  --dstoken-address $ALICE_ADDRESS \
  --oneline)" --init "$(./stack exec -- dstoken-forwarder-contract \
  initial-storage-validated-expiring \
  --dstoken-address $ALICE_ADDRESS \
  --whitelisted-investors '["alice"]' \
  --token-limit 10 \
  --expiration-date "$(date -u -d '9:00 next Fri' +%FT%T.%NZ)")" \
  --burn-cap 1.644

Waiting for the node to be bootstrapped before injection...
Current head: BMDM2fLkz7A9 (timestamp: 2020-01-06T21:13:14-00:00, validation: 2020-01-06T21:13:35-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 47154 units (will add 100 for safety)
Estimated storage: 1644 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oo5xbTawimvR81VNayeXr5redRtJhCZGKRjRwcCct4Wqxk9xdj2'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oo5xbTawimvR81VNayeXr5redRtJhCZGKRjRwcCct4Wqxk9xdj2 to be included --confirmations 30 --branch BMDM2fLkz7A9j4TSBpH2YdRoofH9qbcCYLG3JbX399MRm17mM8y
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.006376
    Expected counter: 69251
    Gas limit: 47254
    Storage limit: 1664 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............ -ꜩ0.006376
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,92) ... +ꜩ0.006376
    Origination:
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      Credit: ꜩ0
      Script:
        { parameter
            (or (or nat
                    (or (or (pair nat string) (pair unit (contract address)))
                        (or (pair unit (contract nat)) (pair unit (contract (set string))))))
                (pair unit (contract timestamp))) ;
          storage (pair (pair unit (pair address (pair (set string) nat))) timestamp) ;
          code { DUP ;
                 CAR ;
                 DIP { CDR } ;
                 IF_LEFT
                   { DIP { DUP ;
                           CAR ;
                           DIP { CDR } ;
                           DIP { DUP ; NOW ; COMPARE ; LT ; IF {} { PUSH string "expired" ; FAILWITH } } } ;
                     PAIR ;
                     SWAP ;
                     DIP { DUP ;
                           CAR ;
                           DIP { CDR } ;
                           IF_LEFT
                             { DIP { DUP ; CAR ; DIP { CDR } } ;
                               PAIR ;
                               SWAP ;
                               DIP { CAR ;
                                     PUSH address "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" ;
                                     PAIR ;
                                     DIP { PUSH (or unit unit) (Right Unit) } ;
                                     PAIR ;
                                     PACK ;
                                     PUSH string "callTokenTransfer" ;
                                     PAIR ;
                                     LEFT (pair nat
                                                (pair (lambda (big_map bytes bytes) (big_map bytes bytes))
                                                      (lambda
                                                         (pair (pair string bytes) (big_map bytes bytes))
                                                         (pair (list operation) (big_map bytes bytes))))) ;
                                     LEFT (or (pair unit (contract nat)) address) ;
                                     LEFT (or (or nat (lambda (big_map bytes bytes) (big_map bytes bytes)))
                                              (or (lambda
                                                     (pair (pair string bytes) (big_map bytes bytes))
                                                     (pair (list operation) (big_map bytes bytes)))
                                                  unit)) ;
                                     DIP { PUSH address "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" ;
                                           CONTRACT
                                             (or (or (or (pair string bytes)
                                                         (pair nat
                                                               (pair (lambda (big_map bytes bytes) (big_map bytes bytes))
                                                                     (lambda
                                                                        (pair (pair string bytes) (big_map bytes bytes))
                                                                        (pair (list operation) (big_map bytes bytes))))))
                                                     (or (pair unit (contract nat)) address))
                                                 (or (or nat (lambda (big_map bytes bytes) (big_map bytes bytes)))
                                                     (or (lambda
                                                            (pair (pair string bytes) (big_map bytes bytes))
                                                            (pair (list operation) (big_map bytes bytes)))
                                                         unit))) ;
                                           IF_NONE { PUSH string "Internal: not DS" ; FAILWITH } { PUSH mutez 0 } } ;
                                     TRANSFER_TOKENS ;
                                     DIP { NIL operation } ;
                                     CONS ;
                                     DIP { UNIT } ;
                                     PAIR ;
                                     DUP ;
                                     CAR ;
                                     DIP { CDR } } ;
                               SWAP ;
                               DIP { SWAP ; PAIR } ;
                               PAIR }
                             { DIP { DUP ; CAR ; DIP { CDR } ; SWAP } ;
                               PAIR ;
                               SWAP ;
                               DIP { DUP ;
                                     CAR ;
                                     DIP { CDR } ;
                                     IF_LEFT
                                       { IF_LEFT
                                           { DIP { DUP ;
                                                   CAR ;
                                                   DIP { CDR } ;
                                                   DUP ;
                                                   SENDER ;
                                                   COMPARE ;
                                                   EQ ;
                                                   IF {} { PUSH string "not DS" ; FAILWITH } ;
                                                   SWAP ;
                                                   DUP ;
                                                   CAR ;
                                                   DIP { CDR } } ;
                                             DUP ;
                                             CAR ;
                                             DIP { CDR } ;
                                             DIP { DIP { DUP } ;
                                                   MEM ;
                                                   IF {} { PUSH string "not in whitelist" ; FAILWITH } ;
                                                   SWAP } ;
                                             SWAP ;
                                             SUB ;
                                             ISNAT ;
                                             IF_NONE { PUSH string "token limit exceeded" ; FAILWITH } {} ;
                                             SWAP ;
                                             PAIR ;
                                             SWAP ;
                                             PAIR ;
                                             NIL operation ;
                                             PAIR }
                                           { DUP ;
                                             CAR ;
                                             DIP { CDR } ;
                                             DIP { DIP { DUP } ; SWAP } ;
                                             PAIR ;
                                             CDR ;
                                             CAR ;
                                             DIP { AMOUNT } ;
                                             TRANSFER_TOKENS ;
                                             NIL operation ;
                                             SWAP ;
                                             CONS ;
                                             PAIR } }
                                       { IF_LEFT
                                           { DUP ;
                                             CAR ;
                                             DIP { CDR } ;
                                             DIP { DIP { DUP } ; SWAP } ;
                                             PAIR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             DIP { AMOUNT } ;
                                             TRANSFER_TOKENS ;
                                             NIL operation ;
                                             SWAP ;
                                             CONS ;
                                             PAIR }
                                           { DUP ;
                                             CAR ;
                                             DIP { CDR } ;
                                             DIP { DIP { DUP } ; SWAP } ;
                                             PAIR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             DIP { AMOUNT } ;
                                             TRANSFER_TOKENS ;
                                             NIL operation ;
                                             SWAP ;
                                             CONS ;
                                             PAIR } } ;
                                     DUP ;
                                     CAR ;
                                     DIP { CDR } } ;
                               SWAP ;
                               DIP { PAIR } ;
                               PAIR } ;
                           DUP ;
                           CAR ;
                           DIP { CDR } } ;
                     SWAP ;
                     DIP { SWAP ; PAIR } ;
                     PAIR }
                   { DUP ;
                     CAR ;
                     DIP { CDR } ;
                     DIP { DIP { DUP } ; SWAP } ;
                     PAIR ;
                     CDR ;
                     CDR ;
                     DIP { AMOUNT } ;
                     TRANSFER_TOKENS ;
                     NIL operation ;
                     SWAP ;
                     CONS ;
                     PAIR } } }
        Initial storage:
          (Pair (Pair Unit (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" (Pair { "alice" } 10)))
                "2020-01-10T09:00:00Z")
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1LsyN1fsk6aFjwWFUvfcrP9qbcgdea4zZY
        Storage size: 1387 bytes
        Paid storage size diff: 1387 bytes
        Consumed gas: 47154
        Balance updates:
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ1.387
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.257

New contract KT1LsyN1fsk6aFjwWFUvfcrP9qbcgdea4zZY originated.
Contract memorized as ValidatedForwarder.
```

Set an alias for the resulting address:

```bash
VFORWARDER_ADDRESS="KT1LsyN1fsk6aFjwWFUvfcrP9qbcgdea4zZY"
```

We can then fetch the initial storage:

```bash
❯❯❯ alpha-client get contract storage for $VFORWARDER_ADDRESS

Pair (Pair Unit (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" (Pair { "alice" } 10)))
     "2020-01-10T09:00:00Z"
```


### Validating transfers

An example of an invalid transfer:

```bash
❯❯❯ ./stack exec -- dstoken-forwarder-contract validate-transfer --received-amount 3 --from-user "bob"
Left (Right (Left (Left (Pair 3 "bob"))))
```

Submitting the validation:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $VFORWARDER_ADDRESS \
  --arg "$(./stack exec -- dstoken-forwarder-contract validate-transfer \
  --received-amount 3 \
  --from-user "bob")" \
  --burn-cap 0.000001 --dry-run

Waiting for the node to be bootstrapped before injection...
Current head: BKzQZctoHxy4 (timestamp: 2020-01-06T21:27:36-00:00, validation: 2020-01-06T21:28:20-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0
    Expected counter: 69272
    Gas limit: 800000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      To: KT1LsyN1fsk6aFjwWFUvfcrP9qbcgdea4zZY
      Parameter: (Left (Right (Left (Left (Pair 3 "bob")))))
      This operation FAILED.

Runtime error in contract KT1LsyN1fsk6aFjwWFUvfcrP9qbcgdea4zZY:
  001: { parameter
  002:     (or (or nat
  ..
  171:              CONS ;
  172:              PAIR } } }
At line 94 characters 84 to 92,
script reached FAILWITH instruction
with "not in whitelist"
```

We then see that the storage is the same:

```bash
❯❯❯ alpha-client get contract storage for $VFORWARDER_ADDRESS

Pair (Pair Unit (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" (Pair { "alice" } 10)))
     "2020-01-10T09:00:00Z"
```


An example of a valid transfer:

```bash
❯❯❯ ./stack exec -- dstoken-forwarder-contract validate-transfer --received-amount 3 --from-user "alice"
Left (Right (Left (Left (Pair 3 "alice"))))
```

Submitting the validation:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $VFORWARDER_ADDRESS \
  --arg "$(./stack exec -- dstoken-forwarder-contract validate-transfer \
  --received-amount 3 \
  --from-user "alice")" \
  --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BKk59cCQzKhR (timestamp: 2020-01-06T21:31:16-00:00, validation: 2020-01-06T21:31:27-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 41869 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'ooCSRY7f9mWRjJDg1iQ8EYaQiDRXxGnJtfRZbGNibM6n4n6EgPv'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooCSRY7f9mWRjJDg1iQ8EYaQiDRXxGnJtfRZbGNibM6n4n6EgPv to be included --confirmations 30 --branch BKk59cCQzKhRfV3FUdCo9pd86pzgq6jufRJuPgktLgDN2Nih1rk
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.004475
    Expected counter: 69277
    Gas limit: 41969
    Storage limit: 0 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............ -ꜩ0.004475
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,92) ... +ꜩ0.004475
    Transaction:
      Amount: ꜩ0
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      To: KT1LsyN1fsk6aFjwWFUvfcrP9qbcgdea4zZY
      Parameter: (Left (Right (Left (Left (Pair 3 "alice")))))
      This transaction was successfully applied
      Updated storage:
        (Pair (Pair Unit
                    (Pair 0x00003b5d4596c032347b72fb51f688c45200d0cb50db (Pair { "alice" } 7)))
              1578646800)
      Storage size: 1387 bytes
      Consumed gas: 41869
```

And then we can check that the storage is properly updated (reducing the
`token-limit` from `10` to `7`):


```bash
❯❯❯ alpha-client get contract storage for $VFORWARDER_ADDRESS

Pair (Pair Unit (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" (Pair { "alice" } 7)))
     "2020-01-10T09:00:00Z"
```

