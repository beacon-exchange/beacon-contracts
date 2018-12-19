The basic concern is that Alice will skip out on forfeiture fees.
To do so, she has several attack vectors:
1. Challenge herself, locking up token B for the duration of the challenge
  period.
2. When she wants to liberate a forfeiture fee, craft a special ITT/POI
  pair (which she does not broadcast but submits directly to the Beacon
  contract) to trade an infinitesimal amount of token B for her token A.
  The trade is then executed immediately and she can claim all her tokens
  with no lockup.

The basic problem is that we account for trading assets and forfeiture fees
in the same bucket. When a trade occurs, some of those trading assets could
have been forfeiture fees. Conversely, when Alice tries to withdraw assets,
the only thing possibly at stake is any outstanding forfeiture fees - she
can't make any trades against assets in the withdrawal process. If she hasn't
forfeited anything, locking up all her assets she wants to withdraw is too
severe.
We should put forfeiture fees (FFs) in a different accounting bucket, with
different rules. The basic concept is that ordinary trading assets are easy
to deposit and withdraw, and FFs have more restrictions (to protect potential
claimants). The rules are as follows:
1. Trading assets are never encumbered except in a challenge. During a
  challenge, the challengee (incumbent) may forfeit the challenge by
  withdrawing funds from the challenge (and as usual the challenger must
  wait out the challenge period).
2. FFs are always encumbered by lockups. When a trader broadcasts an ITT,
  they must have the FF already in escrow (or they automatically forfeit
  any challenges). If a trader wants to withdraw FFs, they must wait out
  the withdrawal lockup period, during which time claims may be submitted
  against the forfeiture fee.

This reduces the problem to that of protecting against double spending FFs.
In other words, suppose Alice has an ITT floating around which references
some FFs in escrow. Alice may still craft a special ITT which references
the same FFs, ahead of any competition, and reclaim her FFs. To make the
accounting simple, let's use a UTXO model. Each ITT references a single
unique forfeiture fee deposit, which can only be used once. In other
words, if Alice plans to broadcast 10 ITTs, she must create a separate FF
deposit for each ITT. (This is super inconvenient to the user of course but
it's easier for us as the designer to reason about. There are ways to make
this more convenient for the user using Merkle proofs but it's more
technically difficult so let's leave that for a future version).

Now, the rules are fairly simple.
1. If Alice wants to withdraw a FF, she may initiate a withdrawal of the FF,
  in which
  * She must wait out the withdrawal period, during which anybody may make a
  claim against that particular FF. A claim consists of a signed ITT
  referencing that FF, which (along with her initiating a withdrawal) proves
  that Alice has forfeited (spent) the FF. If a claim is submitted, the
  Beacon contract must treat it like a challenge where Alice automatically
  forfeited, in which
  * The claimant becomes the beneficiary of the FF, must lock up the amount
  of trading funds and wait out the challenge period specified in the ITT.
  Once the challenge period is over, the claimant may withdraw their trading
  assets which were locked for the challenge but must additionally wait out
  another standard withdrawal period to earn the FF (to mitigate an attack
  where Alice challenges herself with a very short challenge period).
2. If Alice accepts a trade, trading assets from both parties are swapped,
  and Alice must wait out the withdrawal period to withdraw her FF as
  described in 1a. (during which a proof of double spend may be submitted).
3. If Alice is challenged, trading assets from both parties are locked
  for the duration of the challenge period. If Alice accepts the trade,
  the same process as 2. is followed (trading assets are swapped, and
  Alice's FF undergoes a standard withdrawal). If Alice rejects the trade,
  Alice's trading assets are unlocked and the same process as 1b. is followed
  (the challenger's assets stay locked for the duration of the challenge.
  Meanwhile, the FF remains locked for the challenge period + standard
  withdrawal period).
4. Double spends of the same FF (e.g. by crafting a malicious ITT which
  references the same FF that she has already referenced in another ITT) are
  not allowed. If a double spend is discovered while a FF is locked
  during a lockup or challenge period (by a prover submitting a different
  claim on the same FF), the contract should slash her escrow (see below),
  and any funds locked in withdrawal or challenge are immediately released.
4. Once the lockup/challenge period is over, no double spend proofs may
  be submitted and Alice is allowed to withdraw her FF + buffer.

It's not clear yet what an optimal slashing function is, but this is the
proposed slashing condition: each FF must be backed by a buffer equal to
the amount of the FF. This buffer is never at risk if Alice is honest
(broadcasts at most one ITT referencing the FF). If a double spend proof is
submitted, the FF is split between the claimants (which could possibly include
Alice), and the buffer gets burnt (e.g. by sending the buffer amount to an
unrecoverable address like 0x00, or more constructively by sending it to a
charity or insurance fund). Alice's best case scenario is to submit both
claims, in which case she gets the FF back but the buffer is burnt, leading to
the same balance sheet change as if she just let somebody else claim the FF,
except she also has to pay gas costs. It completely disintentivizes the other
scenario where she frivolously submits claims against all claimants because
she loses 1.5x her FF (half the FF + the buffer) instead of 1x her FF (and
reclaiming her buffer).
