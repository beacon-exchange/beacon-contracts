# arbitration contract, draft

# vyper 28fe4f1 /vyper/premade_contracts.py
class ERC20():
    def name() -> bytes32: constant
    def symbol() -> bytes32: constant
    def decimals() -> uint256: constant
    def balanceOf(_owner: address) -> uint256: constant
    def totalSupply() -> uint256: constant
    def transfer(_to: address, _amount: uint256) -> bool: modifying
    def transferFrom(_from: address, _to: address, _value: uint256) -> bool: modifying
    def approve(_spender: address, _amount: uint256) -> bool: modifying
    def allowance(_owner: address, _spender: address) -> uint256: modifying

# TODO better naming, stronger types.
# Token -> User -> balance
balances: (uint256[address])[address]

# Base token -> acquiring token -> User -> balance
earmarked_balances: ((uint256[address])[address])[address]

@public
@payable
def __default__(): # == solidity function(){revert();}?
    assert False

@public
def deposit(token: address, amount: uint256) :
    self.balances[token][msg.sender] += amount

    # special cases for ETH?
    assert ERC20(token).transferFrom(msg.sender, self, amount)

@public
def deposit_market(base_token: address, acquisition_token: address, amount: uint256) :
    # Implicit underflow check
    self.balances[base_token][msg.sender] -= amount

    # todo shorter names
    self.earmarked_balances[base_token][acquisition_token][msg.sender] += amount

@public
def withdraw(token: address, amount: uint256) :
    # Implicit underflow check
    self.balances[token][msg.sender] -= amount

    # time lock?

    assert ERC20(token).transfer(msg.sender, amount)

@public
def initiate_withdraw_market(base_token: address, acquisition_token: address, amount: uint256) :
    assert False

@public
def complete_withdraw_market(base_token: address, acquisition_token: address) :
    assert False

@public
def fill() :
    assert False

@public
def challenge() :
    assert False

@public
def cancel() :
    assert False

@public
def accept() :
    assert False
