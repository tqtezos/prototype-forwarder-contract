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

