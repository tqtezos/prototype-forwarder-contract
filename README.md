# prototype-forwarder-contract

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

