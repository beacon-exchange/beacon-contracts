
# BUY 2 ETH @ 233DAI <- base cur is ETH, quote cur is DAI

# create ITT
@public
def create_itt(base_token: ERC20, quote_token: ERC20, base_amount: uint256(wei), quote_amount: uint256(wei), forfeiture_fee: uint256(wei), challenge_period_blocks: uint256) -> bytes32 :
    pass

@public
def challenge_itt(itt_id: bytes32) :
    pass

@public
def cancel_itt(itt_id: bytes32) :
    pass

@public
def withdraw_itt_funds(itt_id: bytes32) : # after cancel period ends
    pass

@public
def accept_challenge(itt_id: bytes32) :
    pass

# reject challenge and reclaim funds
@public
def reject_challenge(itt_id: bytes32) :
    pass
