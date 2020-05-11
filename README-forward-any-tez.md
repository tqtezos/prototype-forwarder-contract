# Forward-Any-Contract


## Originating Contract


```bash
tezos-client --wait none originate contract ForwardAnyTezContract transferring 0 from $ALICE_ADDRESS running "$(stack exec -- prototype-forwarder-contract print-specialized-any-tez-fa12 --central-wallet $ALICE_ADDRESS)" --burn-cap 0.645

Waiting for the node to be bootstrapped before injection...
Current head: BM1Libhmip9v (timestamp: 2020-05-11T18:53:09-00:00, validation: 2020-05-11T18:54:01-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 19542 units (will add 100 for safety)
Estimated storage: 645 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'onjbYzuM2pmHuoD7xCLgAg5LJFr2QR6zw3cg9AZv7hi1LRnqL9n'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onjbYzuM2pmHuoD7xCLgAg5LJFr2QR6zw3cg9AZv7hi1LRnqL9n to be included --confirmations 30 --branch BM1Libhmip9vD2Lt9jp7sFAoxnc9pWndhGkkD75q2K9b2r3qdiU
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA
    Fee to the baker: ꜩ0.001259
    Expected counter: 802087
    Gas limit: 10000
    Storage limit: 0 bytes
    Balance updates:
      tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA ............. -ꜩ0.001259
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,198) ... +ꜩ0.001259
    Revelation of manager public key:
      Contract: tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA
      Key: edpktwgy96pCWcWoVB3rbPVjtvVY3pag8UyWwrpW3L9zgG7Yoo4GdB
      This revelation was successfully applied
      Consumed gas: 10000
  Manager signed operations:
    From: tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA
    Fee to the baker: ꜩ0.002487
    Expected counter: 802088
    Gas limit: 19642
    Storage limit: 665 bytes
    Balance updates:
      tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA ............. -ꜩ0.002487
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,198) ... +ꜩ0.002487
    Origination:
      From: tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA
      Credit: ꜩ0
      Script:
        { parameter
            (or (unit %default)
                (pair %flush nat (contract (pair (address :from) (pair (address :to) (nat :value)))))) ;
          storage unit ;
          code { CAST (pair (or unit (pair nat (contract (pair address (pair address nat))))) unit) ;
                 CAR ;
                 IF_LEFT
                   { DROP ; NONE (pair nat (contract (pair address (pair address nat)))) }
                   { SOME } ;
                 DIP { PUSH address "tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA" } ;
                 IF_NONE
                   { NIL operation }
                   { DIP { DUP } ;
                     DUP ;
                     CAR ;
                     DIP { CDR } ;
                     DIG 2 ;
                     PAIR ;
                     SELF ;
                     ADDRESS ;
                     PAIR ;
                     DIP { PUSH mutez 0 } ;
                     TRANSFER_TOKENS ;
                     DIP { NIL operation } ;
                     CONS } ;
                 BALANCE ;
                 PUSH mutez 0 ;
                 COMPARE ;
                 EQ ;
                 IF { DIP { DROP } }
                    { DIP { CONTRACT unit ;
                            IF_NONE { PUSH string "not a wallet" ; FAILWITH } {} ;
                            BALANCE ;
                            UNIT ;
                            TRANSFER_TOKENS } ;
                      SWAP ;
                      CONS } ;
                 DIP { UNIT } ;
                 PAIR } }
        Initial storage: Unit
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1Juia2anJKxsLsp4FGgWU8qdzqcb3G4ZED
        Storage size: 388 bytes
        Paid storage size diff: 388 bytes
        Consumed gas: 19542
        Balance updates:
          tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA ... -ꜩ0.388
          tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA ... -ꜩ0.257

New contract KT1Juia2anJKxsLsp4FGgWU8qdzqcb3G4ZED originated.
Contract memorized as ForwardAnyTezContract.
```

## Forwarding Tokens

Let's see what happens when we transfer 0 tokens.

```bash
 export F_ADDRESS=KT1Juia2anJKxsLsp4FGgWU8qdzqcb3G4ZED

tezos-client --wait none transfer 0 from $ALICE_ADDRESS to $F_ADDRESS

Waiting for the node to be bootstrapped before injection...
Current head: BLxRFS1dhqaH (timestamp: 2020-05-11T20:23:41-00:00, validation: 2020-05-11T20:23:49-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 18214 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'oot3Q3Bxk16QqrHTkEhpmjVtFkD5J5aEioKMBEXnRpgG1KvfQ95'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oot3Q3Bxk16QqrHTkEhpmjVtFkD5J5aEioKMBEXnRpgG1KvfQ95 to be included --confirmations 30 --branch BLxRFS1dhqaH9eZddgZEcoq2VgcALB4wSUR8G1ESyfggk5WsNcq
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA
    Fee to the baker: ꜩ0.002083
    Expected counter: 802089
    Gas limit: 18314
    Storage limit: 0 bytes
    Balance updates:
      tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA ............. -ꜩ0.002083
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,198) ... +ꜩ0.002083
    Transaction:
      Amount: ꜩ0
      From: tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA
      To: KT1Juia2anJKxsLsp4FGgWU8qdzqcb3G4ZED
      This transaction was successfully applied
      Updated storage: Unit
      Storage size: 388 bytes
      Consumed gas: 18214
```

As expected, there are no unexpected transactions taking place.

Let's see what happens when we use the forwarder contract to transfer tokens from alice to bob.

```bash
 export BOB_ADDRESS=tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf

 tezos-client --wait none transfer 1 from $BOB_ADDRESS to $F_ADDRESS
Waiting for the node to be bootstrapped before injection...
Current head: BMePNBF8FrzC (timestamp: 2020-05-11T20:28:05-00:00, validation: 2020-05-11T20:29:33-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 38374 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'ooLK9UwEHZvZCvARhKE7x7zxgtVyYnmZBPjJ49LTBc9DkJZnmaR'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooLK9UwEHZvZCvARhKE7x7zxgtVyYnmZBPjJ49LTBc9DkJZnmaR to be included --confirmations 30 --branch BMePNBF8FrzCNsCHTfgqvGGHBzJ9z3c7VsE6yutNZZ4HRd2Y9wc
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
    Fee to the baker: ꜩ0.001259
    Expected counter: 802091
    Gas limit: 10000
    Storage limit: 0 bytes
    Balance updates:
      tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf ............. -ꜩ0.001259
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,198) ... +ꜩ0.001259
    Revelation of manager public key:
      Contract: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
      Key: edpktrjw1VWBM65TE43X1KraWxyXWQhqVEqEXHJ2YCedhyAhL15a8r
      This revelation was successfully applied
      Consumed gas: 10000
  Manager signed operations:
    From: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
    Fee to the baker: ꜩ0.004005
    Expected counter: 802092
    Gas limit: 38474
    Storage limit: 0 bytes
    Balance updates:
      tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf ............. -ꜩ0.004005
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,198) ... +ꜩ0.004005
    Transaction:
      Amount: ꜩ1
      From: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
      To: KT1Juia2anJKxsLsp4FGgWU8qdzqcb3G4ZED
      This transaction was successfully applied
      Updated storage: Unit
      Storage size: 388 bytes
      Consumed gas: 28167
      Balance updates:
        tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf ... -ꜩ1
        KT1Juia2anJKxsLsp4FGgWU8qdzqcb3G4ZED ... +ꜩ1
    Internal operations:
      Transaction:
        Amount: ꜩ1
        From: KT1Juia2anJKxsLsp4FGgWU8qdzqcb3G4ZED
        To: tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA
        This transaction was successfully applied
        Consumed gas: 10207
        Balance updates:
          KT1Juia2anJKxsLsp4FGgWU8qdzqcb3G4ZED ... -ꜩ1
          tz1cfEMbwo2Jyokery6tNpqhkAhQYGjzGrLA ... +ꜩ1

```

There are no internal transactions when 0 tez are transferred.

```bash
tezos-client --wait none transfer 0 from $BOB_ADDRESS to $F_ADDRESS

Waiting for the node to be bootstrapped before injection...
Current head: BMCeNNPrTaTd (timestamp: 2020-05-11T20:30:31-00:00, validation: 2020-05-11T20:30:45-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 18214 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'oo827UKkEe6dyPMGKJQ7WjFHzrh95wWcBaLouc1LaarnopnwqC7'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oo827UKkEe6dyPMGKJQ7WjFHzrh95wWcBaLouc1LaarnopnwqC7 to be included --confirmations 30 --branch BMCeNNPrTaTdVDDKd68RiurM3aVQPnqn2qaskJKJDKxDfJSG5wP
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
    Fee to the baker: ꜩ0.002083
    Expected counter: 802093
    Gas limit: 18314
    Storage limit: 0 bytes
    Balance updates:
      tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf ............. -ꜩ0.002083
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,198) ... +ꜩ0.002083
    Transaction:
      Amount: ꜩ0
      From: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
      To: KT1Juia2anJKxsLsp4FGgWU8qdzqcb3G4ZED
      This transaction was successfully applied
      Updated storage: Unit
      Storage size: 388 bytes
      Consumed gas: 18214
```
