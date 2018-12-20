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
```

`LOCKUP_PERIOD_SECONDS` is a constant defined to be 600 seconds. It is the
minimum time a forfeiture fee must be locked after deposit, to give
participants opportunity to prove dishonesty.

## Interface

The Beacon contract must expose the following interface.

- Deposit ERC20 token
  1) signature: `deposit(address token, uint256 amount)`
  2) User balance must increase by `amount`.
  3) The token is assumed to be a valid ERC20 token.
  4) If the external call to transferFrom fails, the transaction must revert.
- Withdraw ERC20 token
  1) signature: `withdraw(address token, uint256 amount)`
  2) User balance must be >= the amount to withdraw, and must be decreased by
    `amount`.
  3) If the external call to transfer fails, the transaction must revert.
- Deposit forfeiture fee *should rename to deposit escrow*
  1) signature: `deposit_forfeiture_fee(address token, uint256 amount)`
  2) Must allocate a new UUID to be used as a reference for this forfeiture
    fee.
  3) Must set aside `amount*2` of user balance to forfeiture fee and buffer
    balance, which are now encumbered by lock up rules.
- Initiate forfeiture fee withdrawal
  1) signature: `initiate_escrow_withdraw(address token, bytes32 id)`
  2) Must mark the escrow as being withdrawn with no spend proof.
  3) Must mark the escrow beneficiary as the owner of the escrow.
  4) The escrow must not be withdrawable for `LOCKUP_PERIOD_SECONDS`.
- Finalize forfeiture fee withdrawal
  1) signature: `finalize_escrow_withdraw(address token, bytes32 id)`
  2) If the escrow is not double spent, the escrow beneficiary is
    `msg.sender`, and the escrow is no longer locked, credit
    the user with the full amount of the escrow (forfeiture fee +
    buffer balance).
- Exchange assets
  1) signature: `exchange(ITT itt, POI poi, bytes[65] tkr_sig)`
  2) Must ensure the ITT and POI are valid and unforged
    - `itt.sender == msg.sender`
    - `poi.itt_hash == hashStruct(itt)`
      (where `hashStruct` is defined by EIP712)
    - `tkr_sig` is the EIP712 signature of `poi` by `poi.sender`
  3) Must ensure the ITT references a valid, unspent forfeiture fee.
    - If the forfeiture fee is invalid, do not proceed with the exchange
    - If the forfeiture fee is already spent, do not proceed,
      and additionally, slash the forfeiture fee.
  4) Must ensure both parties have sufficient funds to trade.
    - If either party does not have sufficient funds,
      do not proceed with the exchange.
- Challenge an ITT
  1) signature: `initiate_challenge(ITT itt, bytes[65] mkr_sig)`
  2) Must ensure `mkr_sig` is the EIP712 signature of `itt` by `itt.sender`.
  3) Must ensure the ITT references a valid, unspent forfeiture fee.
    - If the forfeiture fee is invalid, do not proceed with the challenge.
    - If the forfeiture fee is already spent, do not proceed,
      and additionally, slash the forfeiture fee.
  3) Must ensure `msg.sender` has `itt.dst_amount` of assets, and lock them.
  4) Must ensure `itt.sender` has not initiated a withdrawal of the forfeiture
    fee and that they have sufficient assets to trade.
    - If not, `itt.sender` automatically forfeits. Assign the beneficiary of
      the forfeiture fee to be the challenger, and lock up the challenger's
      funds for the duration of the challenge period.
- Forfeit a challenge
  1) signature: `forfeit_challenge(address base, address dst, bytes32 itt_hash)`
  2) Must ensure `msg.sender` is the `itt.sender` in the challenge
  3) Must return `base` funds to `msg.sender`
  4) Challenger funds must remain locked until the challenge expires
- Accept a challenge
  1) signature: `accept_challenge(address base, address dst, bytes32 itt_hash)`
  2) Must ensure `msg.sender` is the `itt.sender` of the challenge
  3) If the challenge is not expired or forfeited, and the escrow has not been
  slashed, exchange assets.
    - As in a successful call to `exchange`, the beneficiary of the
      forfeiture fee is reassigned to be the `itt.sender`, and is locked
      for an additional `LOCKUP_PERIOD_SECONDS`.
- Return challenge funds
  1) signature: `return_challenge_funds(address base, address dst, bytes32 itt_hash)`
  2) Must ensure `msg.sender` is the `itt.sender` or `poi.sender` of the challenge.
  3) Must ensure the challenge expired or the escrow was slashed
    - (If the `itt.sender` wishes to get their funds back without one of these
        conditions holding, they should call `forfeit_challenge`.)
  4) Must return both sets of assets to their original owners
  5) Unless the escrow was already slashed, extend the lockup for the
    escrow by `LOCKUP_PERIOD_SECONDS`.

<i>Future areas of expansion/improvement:
  1) Be more consistent in terminology vis a vis escrow/forfeiture fees
  2) Partial fills
  3) Account model for forfeiture fees instead of spent/unspent
  4) Don't require deposit/withdraw to be separate steps (although more external call risk)
  </i>
