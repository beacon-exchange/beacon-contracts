# Beacon contract specs
The Beacon Asset Exchange Protocol rationale and background are
presented in [the Beacon white paper](https://beaconexchange.io/beacon.pdf),
but this spec takes precedence in the case of differences.

This spec follows [semver](https://semver.org/) but with no PATCH
component. The current version is v0.0.

## Preliminary definitions

### Vocabulary and reference clarification

The key words
"MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL"
in this document are to be interpreted as described in
[RFC2199](https://tools.ietf.org/html/rfc2119).

The Beacon contract is assumed to be deployed on an Ethereum blockchain which
adheres to [the Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf).

Types and function signatures are assumed to adhere to
[the ABIv2 spec](https://solidity.readthedocs.io/en/v0.5.2/abi-spec.html).

Signatures of structured data must conform to
[EIP712](https://github.com/ethereum/EIPs/blob/38aa52e04652f/EIPS/eip-712.md).

### Domain

`BEACON_CONTRACT` refers to the address (entry point) of the Beacon contract.

For the purposes of EIP712 signing, the domain separator is defined as
```solidity
keccak256(abi.encode(
  EIP712DOMAIN_TYPEHASH,
  keccak256(DOMAIN_NAME),
  keccak256(DOMAIN_VERSION),
  address(BEACON_CONTRACT)
  ))
```
where `DOMAIN_NAME` is equal to `"Beacon Exchange"`, and
`EIP712DOMAIN_TYPEHASH` is equal to the following value:
```
keccak256("EIP712Domain(string name,string version,address verifyingContract)")
```

`DOMAIN_VERSION` is the version of the Beacon contract and must also follow
semver. Importantly, the major and minor versions of the Beacon contract
must be equal to the major and minor versions (respectively) of this spec.

## Overview
At a high level, the Beacon contract must enforce the rules of the protocol
and provide safety to both parties. Broadly, the trading process has three
steps:
1. Broadcast an Intent-To-Trade (ITT). An ITT is an actionable quote to trade
some amount of tokens for some amount of another token. (Note: An ITT does not
have to be broadcast but could be a point-to-point message, as in a private
chat).
2. Respond to an ITT with a Proposal-On-Intent (POI). A POI offers the
token requested in the ITT in exchange for the token offered in the ITT.
3. Submit the ITT and POI to the Beacon contract. If both parties have
consented (provided their digital signatures) and both the ITT and POI are
valid (defined more precisely below, but basically both parties have
sufficient funds), then the Beacon contract executes the trade atomically.

In order to keep liquidity providers honest and not back out of trades at
their convenience, they must include a forfeiture fee (which may be zero)
with their ITT. This forfeiture fee is the compensation which they must
supply to the liquidity provider to back out of a trade.

The rules governing how and when forfeiture fees are transferred
from the liquidity provider to the liquidity taker are a key area of
difference between this spec and the white paper, and are discussed in
[this supplement to the spec](1_escrow_spec.md). (Note: the escrow spec
also relaxes the escrow requirements for trading assets).


### Data structure definitions

We define off-chain data structures as Solidity structs which are signable
(according to EIP712).

```solidity
struct ITT {
  address BEACON_CONTRACT; // The validating contract.
  address sender; // The sender of the ITT
  address base; // The address of the ERC20 token which is to be sent
  address dst; // The address of the ERC20 token which is requested
  uint256 base_amount; // The amount of the ERC20 token to be sent
  uint256 dst_amount; // The amount of the ERC20 token which is requested
  bytes32 forfeiture_fee_id; // A reference to a valid forfeiture fee
  uint256 challenge_period_seconds; // The length of the challenge period in seconds
  bytes32 nonce; // User-defined nonce to prevent replay attacks.
}

struct POI {
  address BEACON_CONTRACT; // The validating contract.
  address sender; // The sender of the POI
  bytes32 itt_hash; // The hash of the ITT, calculated with the `hashStruct` function defined in EIP712
  bytes32 nonce; // User-defined nonce to prevent replay attacks. # Revisit as this might not actually be necessary.
}

## Interface

The Beacon contract must expose the following interface.

1) Deposit ERC20 token
  - signature: deposit(address token, uint256 amount)
  - User balance must increase by `amount`.
  - The token is assumed to be a valid ERC20 token.
  - If the external call to transferFrom fails, the transaction must revert.
2) Withdraw ERC20 token
  - signature: withdraw(address token, uint256 amount)
  - User balance must be >= the amount to withdraw, and must be decreased by
    `amount`.
  - If the external call to transfer fails, the transaction must revert.

*To be continued*
