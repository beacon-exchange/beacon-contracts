// PROOF OF CONCEPT!!
// Not optimized for code size, gas, or anything really.
// Not audited. Correctness may vary.
pragma solidity ^0.5.1;
pragma experimental ABIEncoderV2;
import 'lib/SafeMath.sol';

// No way to subclass, e.g. newtypes like interface BaseToken extends ERC20
interface ERC20 {
  function totalSupply() external view returns (uint supply);
  function balanceOf(address _owner) external view returns (uint balance);
  function transfer(address _to, uint _value) external returns (bool success);
  function transferFrom(address _from, address _to, uint _value) external returns (bool success);
  function approve(address _spender, uint _value) external returns (bool success);
  function allowance(address _owner, address _spender) external view returns (uint remaining);
  function decimals() external view returns(uint digits);
  event Approval(address indexed _owner, address indexed _spender, uint _value);
}

contract Main {
  using SafeMath for uint256;

  uint256 constant LOCKUP_PERIOD_SECONDS = 600; // 10 minutes

  // can't actually use these newtypes as map keys
  // so they are not really used
  struct UserAddress {
    address addr;
  }
  struct TokenAddress {
    address addr;
  }

  struct EIP712Domain {
    string name;
    string version;
    //uint256 chainId;
  }
  function eip712domain() public pure returns (EIP712Domain memory) {
    return EIP712Domain({
      name: "Beacon Exchange Protocol",
      version: "v0.0.0"
    });
  }

  /*** Off-chain structs ***/
  struct ITT {
    address BEACON_CONTRACT;
    address base;
    address dst;
    address sender; // need this or is it inferrable?
    uint256 amount;
    uint256 forfeiture_fee;
    uint256 challenge_period_seconds;
    //bool partial_fills;
    bytes32 nonce;
  }

  struct POI {
    address BEACON_CONTRACT;
    address itt_base;
    address itt_dst;
    //uint256 amount; // assume no partial fills for now.
    bytes32 itt_hash;
    bytes32 nonce;
  }

  /*** Contract data structures ***/

  // deposit is easy to withdraw
  struct DepositBalance {
    uint256 deposit_balance;
  }
  // escrow balance must go through lockup process to withdraw to allow
  // for challenges.
  struct EscrowBalance {
    uint256 escrow_balance;
  }

  struct Withdraw {
    uint256 withdraw_amount;
  }

  struct DestinationToken {
    // Amount on deposit for this particular market.
    mapping(address/*User*/ => EscrowBalance) escrow_balances;

    // Map from user to latest withdraw
    mapping(address/*User*/ =>
            mapping(uint256/*timestamp*/ => Withdraw)
           ) withdraws;
  }

  struct BaseToken {
    mapping(address/*Token*/ => DestinationToken) markets;
    mapping(address/*User*/ => DepositBalance) balances;
  }
  mapping(address => BaseToken) entry;

  //mapping(bytes32 => Withdraw) all_withdraws;

  function() external {
    revert();
  }

  function escrow_balance(ERC20 base, ERC20 dst, address sender)
    private
    returns (EscrowBalance storage)
  {
    return entry[address(base)]
      .markets[address(dst)]
      .escrow_balances[msg.sender];
  }

  function deposit_balance(ERC20 base, address sender)
    private
    returns (DepositBalance storage)
  {
    return entry[address(base)].balances[sender];
  }

  function deposit(ERC20 tok, uint256 amount) external {
    DepositBalance storage dpst = deposit_balance(tok, msg.sender);
    dpst.deposit_balance = dpst.deposit_balance.add(amount);

    // approval in separate step
    require(tok.transferFrom(msg.sender, address(this), amount),
           "deposit failed: transferFrom");
    // TODO LOG
  }

  // Deposit tokens into a specific market.
  // Base is the token that is owned, dst is the token to acquire.
  function deposit_market(ERC20 base, ERC20 dst, uint256 amount)
    external
  {
    DepositBalance storage dpst = deposit_balance(base, msg.sender);
    dpst.deposit_balance = dpst.deposit_balance.sub(amount);

    EscrowBalance storage escr = escrow_balance(base,dst,msg.sender);
    escr.escrow_balance = escr.escrow_balance.add(amount);
    //TODO LOG
  }

  function withdraw(ERC20 tok, uint256 amount)
    external
  {
    DepositBalance storage dpst = deposit_balance(tok, msg.sender);
    dpst.deposit_balance = dpst.deposit_balance.sub(amount);
    require(tok.transfer(msg.sender, amount), "withdraw failed: transfer");
    //TODO LOG
  }

  function lookup_withdraw(ERC20 base, ERC20 dst, address sender, uint256 timestamp)
    private
    returns (Withdraw storage)
  {
    return
      entry[address(base)]
        .markets[address(dst)]
        .withdraws[sender][timestamp];
  }

  function initiate_withdraw_market(ERC20 base, ERC20 dst, uint256 amount)
    external
  {
    EscrowBalance storage escr = escrow_balance(base, dst, msg.sender);
    escr.escrow_balance = escr.escrow_balance.sub(amount);

    Withdraw storage w = lookup_withdraw(base, dst, msg.sender, block.timestamp);
    w.withdraw_amount = w.withdraw_amount.add(amount);
    // TODO LOG
  }


  function complete_withdraw_market(ERC20 base, ERC20 dst, uint256 timestamp)
    external
    returns (uint amount)
  {
    require(timestamp + LOCKUP_PERIOD_SECONDS < block.timestamp,
           "withdrawal lockup period has not expired yet.");

    Withdraw storage w = lookup_withdraw(base, dst, msg.sender, timestamp);
    DepositBalance storage dpst = deposit_balance(base, msg.sender);

    uint256 amt = w.withdraw_amount;
    // can use unsafe math?
    dpst.deposit_balance = dpst.deposit_balance.add(amt);

    // clear storage
    //delete w; <-- does not compile
    delete entry[address(base)]
      .markets[address(dst)]
      .withdraws[msg.sender][timestamp];

    // TODO LOG
    return amt;
  }

  // Passing structs via calldata fails solc with unimplemented error
  // function fill(ITT calldata itt, POI calldata poi/*, sigs*/)
  function fill(
    /*
    address itt_BEACON_CONTRACT,
    address itt_base,
    address itt_dst,
    address itt_sender,
    uint256 itt_amount,
    uint256 itt_forfeiture_fee,
    uint256 itt_challenge_period_seconds,
    bytes32 itt_nonce,
    address poi_BEACON_CONTRACT,
    address poi_itt_base,
    address poi_itt_dst,
    bytes32 poi_itt_hash,
    bytes32 poi_nonce*/)
    public
  {
    /*
    // TODO VERIFY SIGS
    require(address(this) == itt_BEACON_CONTRACT, "Wrong ITT contract");
    require(address(this) == poi_BEACON_CONTRACT, "Wrong POI contract");
    bytes32 itt_digest =
    */

    assert(false)
  }
     
  function challenge() external {
    assert(false);
  }
     
  function cancel() external {
    assert(false);
  }
     
  function accept() external {
    assert(false);
  }
}
