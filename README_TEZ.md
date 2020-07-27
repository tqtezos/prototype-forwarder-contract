
# Originated examples

- Forward Tez: [KT1Sb5rCTy76HjDtZzHJNEEDSEMm6GBobz6T](https://better-call.dev/carthagenet/KT1Sb5rCTy76HjDtZzHJNEEDSEMm6GBobz6T/operations)
- Forward Tez using an explicit Flush: [KT1DLpwM95UrHLaMkcUN9J8XZ75airZoF42p](https://better-call.dev/carthagenet/KT1DLpwM95UrHLaMkcUN9J8XZ75airZoF42p/operations)

## Setup

```bash
$ echo $FRED_ADDRESS

tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
```

# Simple Tez forwarder

```haskell
> Lorentz.printLorentzContract True (specializedTezForwarderContract . fromRight undefined $ Tezos.Address.parseAddress "tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir")

"parameter unit;storage unit;code { CDR;NIL operation;PUSH address \"tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir\";CONTRACT unit;IF_NONE { FAILWITH }        { BALANCE;UNIT;TRANSFER_TOKENS;CONS;PAIR } };"
```

```bash
$ tezos-client --wait none originate contract SimpleTezForwarder \
  transferring 0 from $BOB_ADDRESS running \
  "parameter unit;storage unit;code { CDR;NIL operation;PUSH address \"tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir\";CONTRACT unit;IF_NONE { FAILWITH }        { BALANCE;UNIT;TRANSFER_TOKENS;CONS;PAIR } };" \
  --burn-cap 0.366

Waiting for the node to be bootstrapped before injection...
Current head: BLxofu1UByRY (timestamp: 2020-07-27T19:51:56-00:00, validation: 2020-07-27T19:52:09-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 12412 units (will add 100 for safety)
Estimated storage: 366 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooDNP3EwmUU6gS6P5CW2WkyoZmLjDqPvoH9UYTD5xg9a4PAe441'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooDNP3EwmUU6gS6P5CW2WkyoZmLjDqPvoH9UYTD5xg9a4PAe441 to be included --confirmations 30 --branch BLxofu1UByRYFFK5RH8CVKEnsCLcPeKz3h2PWBYBwjxFtWWDpNf
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.00159
    Expected counter: 624004
    Gas limit: 12512
    Storage limit: 386 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.00159
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,292) ... +ꜩ0.00159
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter unit ;
          storage unit ;
          code { CDR ;
                 NIL operation ;
                 PUSH address "tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir" ;
                 CONTRACT unit ;
                 IF_NONE { FAILWITH } { BALANCE ; UNIT ; TRANSFER_TOKENS ; CONS ; PAIR } } }
        Initial storage: Unit
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1Sb5rCTy76HjDtZzHJNEEDSEMm6GBobz6T
        Storage size: 109 bytes
        Paid storage size diff: 109 bytes
        Consumed gas: 12412
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.109
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1Sb5rCTy76HjDtZzHJNEEDSEMm6GBobz6T originated.
Contract memorized as SimpleTezForwarder.
```

```bash
TEZ_FORWARDER="KT1Sb5rCTy76HjDtZzHJNEEDSEMm6GBobz6T"
```

Send `1 Tez`:

```bash
$ tezos-client --wait none transfer 1 from $BOB_ADDRESS to $TEZ_FORWARDER


Waiting for the node to be bootstrapped before injection...
Current head: BKwTdzfCJPkt (timestamp: 2020-07-27T19:55:06-00:00, validation: 2020-07-27T19:55:28-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 32625 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'opDGwWMoCY98nqDdU3SK5rSU2YKQCPqLYfE96bhpUzFFG4dYyMm'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opDGwWMoCY98nqDdU3SK5rSU2YKQCPqLYfE96bhpUzFFG4dYyMm to be included --confirmations 30 --branch BKwTdzfCJPkti7QULYkXzo1HAqJZiwdxXMMYYQFHb1X3rqBaxMn
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.003526
    Expected counter: 624005
    Gas limit: 32725
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.003526
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,292) ... +ꜩ0.003526
    Transaction:
      Amount: ꜩ1
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1Sb5rCTy76HjDtZzHJNEEDSEMm6GBobz6T
      This transaction was successfully applied
      Updated storage: Unit
      Storage size: 109 bytes
      Consumed gas: 22418
      Balance updates:
        tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ1
        KT1Sb5rCTy76HjDtZzHJNEEDSEMm6GBobz6T ... +ꜩ1
    Internal operations:
      Transaction:
        Amount: ꜩ1
        From: KT1Sb5rCTy76HjDtZzHJNEEDSEMm6GBobz6T
        To: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
        This transaction was successfully applied
        Consumed gas: 10207
        Balance updates:
          KT1Sb5rCTy76HjDtZzHJNEEDSEMm6GBobz6T ... -ꜩ1
          tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ... +ꜩ1
```


# Tez forwarder with flush

```haskell
> Lorentz.printLorentzContract True (specializedTezFlushForwarderContract . fromRight undefined $ Tezos.Address.parseAddress "tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir")

"parameter (or (unit %default) (unit %f));storage unit;code { CAST (pair (or unit unit) unit);CAR;IF_LEFT { NIL operation }        { NIL operation;PUSH address \"tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir\";CONTRACT unit;IF_NONE { FAILWITH }        { BALANCE;UNIT;TRANSFER_TOKENS;CONS } };PAIR };"
```

```bash
$ tezos-client --wait none originate contract FlushTezForwarder \
  transferring 0 from $BOB_ADDRESS running \
  "parameter (or (unit %default) (unit %f));storage unit;code { CAST (pair (or unit unit) unit);CAR;IF_LEFT { NIL operation }        { NIL operation;PUSH address \"tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir\";CONTRACT unit;IF_NONE { FAILWITH }        { BALANCE;UNIT;TRANSFER_TOKENS;CONS } };PAIR };" \
  --burn-cap 0.416

Waiting for the node to be bootstrapped before injection...
Current head: BL8RLFGp3kTS (timestamp: 2020-07-27T19:58:16-00:00, validation: 2020-07-27T19:59:01-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 13560 units (will add 100 for safety)
Estimated storage: 416 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooGzRcgFY5xwdWUN5k8U7EQ9a4bfngK226Xnxuag7V5VzrbHN45'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooGzRcgFY5xwdWUN5k8U7EQ9a4bfngK226Xnxuag7V5VzrbHN45 to be included --confirmations 30 --branch BLayDa7JQbwqa5iWAXFggsj6zGPRrUTf1NcFuqUKqWR1fuVkYLq
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001754
    Expected counter: 624006
    Gas limit: 13660
    Storage limit: 436 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.001754
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,292) ... +ꜩ0.001754
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter (or (unit %default) (unit %f)) ;
          storage unit ;
          code { CAST (pair (or unit unit) unit) ;
                 CAR ;
                 IF_LEFT
                   { NIL operation }
                   { NIL operation ;
                     PUSH address "tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir" ;
                     CONTRACT unit ;
                     IF_NONE { FAILWITH } { BALANCE ; UNIT ; TRANSFER_TOKENS ; CONS } } ;
                 PAIR } }
        Initial storage: Unit
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1DLpwM95UrHLaMkcUN9J8XZ75airZoF42p
        Storage size: 159 bytes
        Paid storage size diff: 159 bytes
        Consumed gas: 13560
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.159
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1DLpwM95UrHLaMkcUN9J8XZ75airZoF42p originated.
Contract memorized as FlushTezForwarder.
```

```bash
FLUSH_TEZ_FORWARDER="KT1DLpwM95UrHLaMkcUN9J8XZ75airZoF42p"
```

Transfer:

```bash
$ tezos-client --wait none transfer 1 from $BOB_ADDRESS to $FLUSH_TEZ_FORWARDER

Waiting for the node to be bootstrapped before injection...Current head: BMYaY5oZtyjJ (timestamp: 2020-07-27T19:59:46-00:00, validation: 2020-07-27T20:00:13-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 13269 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'ooSaZtSyLYcykpDj47yUdwenDjG97skdSvw4UhUzsLHUWhvwegq'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooSaZtSyLYcykpDj47yUdwenDjG97skdSvw4UhUzsLHUWhvwegq to be included --confirmations 30 --branch BMYaY5oZtyjJEk2cmnYU1EtgGN8EutzjoHvNwiwWnTcKTaCkpmc
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001589
    Expected counter: 624007
    Gas limit: 13369
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.001589
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,292) ... +ꜩ0.001589
    Transaction:
      Amount: ꜩ1
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1DLpwM95UrHLaMkcUN9J8XZ75airZoF42p
      This transaction was successfully applied
      Updated storage: Unit
      Storage size: 159 bytes
      Consumed gas: 13269
      Balance updates:
        tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ1
        KT1DLpwM95UrHLaMkcUN9J8XZ75airZoF42p ... +ꜩ1
```

Flush:

```bash
$ tezos-client --wait none transfer 0 from $BOB_ADDRESS to $FLUSH_TEZ_FORWARDER \
  --entrypoint f

Waiting for the node to be bootstrapped before injection...
Current head: BLj2MK5DhnzU (timestamp: 2020-07-27T20:02:16-00:00, validation: 2020-07-27T20:02:36-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 33629 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'ooxFLNSPoUeFMfc4U6RPrJht5BbbtB4c435sxL5gfzNJKDBU14P'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooxFLNSPoUeFMfc4U6RPrJht5BbbtB4c435sxL5gfzNJKDBU14P to be included --confirmations 30 --branch BLj2MK5DhnzUDbsefXrBH98ETgtYSfRStUqLmJ4wxkKiiasVyHU
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.003633
    Expected counter: 624008
    Gas limit: 33729
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.003633
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,292) ... +ꜩ0.003633
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1DLpwM95UrHLaMkcUN9J8XZ75airZoF42p
      Entrypoint: f
      This transaction was successfully applied
      Updated storage: Unit
      Storage size: 159 bytes
      Consumed gas: 23422
    Internal operations:
      Transaction:
        Amount: ꜩ1
        From: KT1DLpwM95UrHLaMkcUN9J8XZ75airZoF42p
        To: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
        This transaction was successfully applied
        Consumed gas: 10207
        Balance updates:
          KT1DLpwM95UrHLaMkcUN9J8XZ75airZoF42p ... -ꜩ1
          tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ... +ꜩ1
```

