# terminology:
# BUY 2 ETH @ 233DAI <- base cur is ETH, quote cur is DAI
# msg.sender that creates an ITT: originator
# msg.sender that challenges an ITT: challenger

from vyper.interfaces import ERC20

# This is a really big struct. Would be (much) more gas efficient to 
# only keep its hash stored and then pass in data via calldata, checking
# the calldata against storage.
units: {
  base: "base currency in wei",
  quote: "quote currency in wei"
}

struct ITTData:
    base_token: address
    quote_token: address
    base_amount: uint256(base)
    quote_amount: uint256(quote)
    forfeiture_fee: uint256(base)
    challenge_period_seconds: timedelta

struct ITT:
    originator: address
    ittdata: ITTData
    
struct ITTChallenge:
    challenger: address
    challenge_start: timestamp

CreateITT: event(
      { originator: address
      , base_token: address
      , quote_token: address
      , base_amount: uint256(base)
      , quote_amount: uint256(quote)
      , forfeiture_fee: uint256(base)
      , challenge_period_seconds: timedelta
      , itt_id: bytes32
      })
      

CancelITT: event(
    { itt_id: bytes32
    })
    

Challenge: event(
    { challenger: indexed(address)
    , itt_id: indexed(bytes32)
    })

WithdrawITTFunds: event(
    { itt_id: indexed(bytes32)
    })

AcceptChallenge: event(
    { itt_id: indexed(bytes32)
    })

RejectChallenge: event(
    { itt_id: indexed(bytes32)
    })
    
    
    
LOCKUP_PERIOD_SECONDS: timedelta # ten minutes

cancel_started_at: map(bytes32, timestamp)

next_id: uint256

# create ITT
# Create an ID (probably just increment self.next_id)
# Should store the the ITT data, keyed by the ID
# access: anybody with the funds
# emits: CreateITT log
@public
@nonreentrant('create_itt')
def create_itt(base_token: address, quote_token: address, base_amount: uint256(base), quote_amount: uint256(quote), forfeiture_fee: uint256(base), challenge_period_seconds: uint256) -> bytes32 :
    self.next_id += 1
    itt: ITTData = ITTData({ originator: msg.sender
          , base_token: base_token
          , quote_token: quote_token
          , base_amount: base_amount
          , quote_amount: quote_amount
          , forfeiture_fee: forfeiture_fee
          , challenge_period_seconds: challenge_period_seconds
          }[int128])
    # TODO ideally keccak256(abi.encode(next_id, contract_address))
    # also TODO optimize out the extra SLOAD
    itt_id = convert(self.next_id, bytes32)
    self.itts[itt_id] = itt

    # pull the funds into escrow.
    assert_modifiable(
        base_token.transferFrom(msg.sender, self, base_amount + forfeiture_fee))

    CreateITT({ 
            originator: itt.originator
          , base_token: base_token
          , quote_token: quote_token
          , base_amount: base_amount
          , quote_amount: quote_amount
          , forfeiture_fee: forfeiture_fee
          , challenge_period_seconds: challenge_period_seconds
          , itt_id: itt_id
     })
     
    return itt_id



# Cancel an ITT
# If the ITT is unchallenged, start the timer for LOCKUP_PERIOD_SECONDS
# during this time, the ITT can still be challenged. After this time,
# the ITT cannot be challenged, it can only be withdrawn using
# withdraw_itt_funds.
# access: ITT originator
# emits: CancelITT log
@public
def cancel_itt(itt_id: bytes32) :
    assert not self.challenges[itt_id].challenger, "Cancel exists"
    assert self.itts[itt_id].originator == msg.sender, "Unauthorized"

    self.cancel_started_at[itt_id] = block.timestamp
    CancelITT.log(itt_id)


# Challenge an ITT.
# If the funds are available, and ITT is unchallenged (or currently in
# lockup period), lock 'em up and start the challenge period.
# (note: if the itt doesn't exist there will be a bunch of zeroes
#    and the challenger will just waste gas)
# access: anybody with the funds
# emits: ChallengeITT log
@public
@nonreentrant('challenge_itt')
def challenge_itt(itt_id: bytes32) :
    assert not self.challenges[itt_id].challenger, "ITT already challenged"
#assert not self.cancel_started_at[itt_id], "ITT in canceling state"
    cxl_started_at: timestamp = self.cancel_started_at[itt_id]
    assert not cxl_started_at or cxl_started_at + LOCKUP_PERIOD_SECONDS < block.timestamp, "ITT past challengable period"

    # cancel the withdraw
    self.cancel_started_at[itt_id] = 0

    self.challenges[itt_id] = Challenge(
        { challenger: msg.sender
        , challenge_start: block.timestamp
        })

    quote_amount: uint256(quote) = self.itts[itt_id].quote_amount

    # TODO handle non-ERC20 shenanigans
    assert_modifiable(
        quote_token.transferFrom(msg.sender, self, quote_amount))

    ChallengeITT.log(
        { challenger: msg.sender
        , itt_id: itt_id
        })


# If the ITT has finished lockup period, send the funds back to ITT
# originator (and delete ITT data).
# access: the ITT originator
# emits: WithdrawITTFunds log
@public
@nonreentrant('withdraw_itt_funds')
def withdraw_itt_funds(itt_id: bytes32) : # after cancel period ends
    cxl_start_at: timestamp = self.cancel_started_at[itt_id]
    assert cxl_start_at, "Withdrawal not started"
    assert block.timestamp >= cxl_start_at + LOCKUP_PERIOD_SECONDS

    base_token.transfer(self.itts[itt_id].originator,
        self.itts[itt_id].base_amount + self.itts[itt_id].forfeiture_fee)

    clear(self.itts[itt_id])

    WithdrawITTFunds.log({itt_id: itt_id})

# For an ITT in a challenge period, accept the challenge:
#   - transfer quote_token to originator
#   - transfer base_token to challenger
#   - transfer forfeiture_fee back to originator
#   - (clean up: delete ITT data)
# access: ITT originator
# emits: AcceptChallenge log
@public
@nonreentrant('accept_challenge')
def accept_challenge(itt_id: bytes32) :
    orig: address = self.itts[itt_id].originator
    challenger: address = self.challenges[itt_id].challenger
    challenge_len: timedelta = self.itts[itt_id].challenge_period_seconds
    challenge_start: timestamp = self.challenges[itt_id].challenge_start
    challenge_ends_at: timestamp = challenge_start + challenge_len
    assert orig == msg.sender and challenge_ends_at < block.timestamp, "Challenge cannot be accepted"

    base_token: ERC20 = self.itts[itt_id].base_token
    quote_token: ERC20 = self.itts[itt_id].quote_token
    base_token.transfer(challenger, self.itts[itt_id].base_amount)
    base_token.transfer(orig, self.itts[itt_id].forfeiture_fee)
    quote_token.transfer(orig, self.itts[itt_id].quote_amount)

    clear(self.itts[itt_id])
    clear(self.challenges[itt_id])
    AcceptChallenge.log({itt_id: itt_id})
    
# For an ITT in a challenge period, reject challenge and reclaim funds
#   - transfer base_token back to originator
#   - transfer quote_token back to challenger
#   - forfeit forfeiture_fee by TRANSFERRING TO CHALLENGER
#   - (clean up: delete ITT data)
# access: ITT originator, or if the challenge period expired, anybody
# emits: RejectChallenge log
@public
def reject_challenge(itt_id: bytes32) :
    orig: address = self
    challenger: address = self.challenges[itt_id].challenger
    challenge_len: timedelta = self.itts[itt_id].challenge_period_seconds
    challenge_start: timestamp = self.challenges[itt_id].challenge_start
    challenge_ends_at: timestamp = challenge_start + challenge_len
    assert orig == msg.sender or challenge_ends_at >= block.timestamp, "Challenge cannot be rejected"
    base_token: ERC20 = self.itts[itt_id].base_token
    quote_token: ERC20 = self.itts[itt_id].quote_token

    base_token.transfer(orig, base_amount)
    base_token.transfer(challenger, forfeiture_fee)
    quote_token.transfer(challenge, quote_amount)

    clear(self.itts[itt_id])
    clear(self.challenges[itt_id])

    RejectChallenge.log({itt_id: itt_id})

