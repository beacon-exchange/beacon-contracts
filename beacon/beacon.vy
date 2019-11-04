
# terminology:
# BUY 2 ETH @ 233DAI <- base cur is ETH, quote cur is DAI
# msg.sender that creates an ITT: originator
# msg.sender that challenges an ITT: challenger

LOCKUP_PERIOD_SECONDS: uint256(constant) = 600 # ten minutes

units: {
  base: "base currency in wei",
  quote: "quote currency in wei"
}

CreateITT: event(
      { originator: indexed(address)
      , base_token: indexed(ERC20)
      , quote_token: index(ERC20)
      , base_amount: uint256(base)
      , quote_amount: uint256(quote)
      , forfeiture_fee: uint256(base)
      , challenge_period_seconds: uint256
      , itt_id: bytes32
      })
# create ITT
# Create an ID (probably just increment self.next_id)
# Should store the the ITT data, keyed by the ID
# access: anybody with the funds
# emits: CreateITT log
@public
def create_itt(base_token: ERC20, quote_token: ERC20, base_amount: uint256(base), quote_amount: uint256(quote), forfeiture_fee: uint256(base), challenge_period_seconds: uint256) -> bytes32 :
    pass

# Cancel an ITT
# If the ITT is unchallenged, start the timer for LOCKUP_PERIOD_SECONDS
# during this time, the ITT can still be challenged. After this time,
# the ITT cannot be challenged, it can only be withdrawn using
# withdraw_itt_funds.
# access: ITT originator
# emits: CancelITT log
@public
def cancel_itt(itt_id: bytes32) :
    pass

ChallengeITT: event(
    { challenger: indexed(address)
    , itt_id: indexed(bytes32)
    })
# Challenge an ITT.
# If the funds are available, and ITT is unchallenged (or currently in
# lockup period), lock 'em up and start the challenge period.
# access: anybody with the funds
# emits: ChallengeITT log
@public
def challenge_itt(itt_id: bytes32) :
    pass

WithdrawITTFunds: event(
    { itt_id: indexed(bytes32)
    })
# If the ITT has finished lockup period, send the funds back to ITT
# originator (and delete ITT data).
# access: the ITT originator
# emits: WithdrawITTFunds log
@public
def withdraw_itt_funds(itt_id: bytes32) : # after cancel period ends
    pass

AcceptChallenge: event(
    { itt_id: indexed(bytes32)
    })
# For an ITT in a challenge period, accept the challenge:
#   - transfer quote_token to originator
#   - transfer base_token to challenger
#   - transfer forfeiture_fee back to originator
#   - (clean up: delete ITT data)
# access: ITT originator, or if the challenge period has expired, anybody.
# emits: AcceptChallenge log
@public
def accept_challenge(itt_id: bytes32) :
    pass

RejectChallenge: event(
    { itt_id: indexed(bytes32)
    })
# For an ITT in a challenge period, reject challenge and reclaim funds
#   - transfer base_token back to originator
#   - transfer quote_token back to challenger
#   - forfeit forfeiture_fee by TRANSFERRING TO CHALLENGER
#   - (clean up: delete ITT data)
# access: ITT originator
# emits: RejectChallenge log
@public
def reject_challenge(itt_id: bytes32) :
    pass
