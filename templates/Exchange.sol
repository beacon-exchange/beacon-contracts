// PROOF OF CONCEPT!!
// Not optimized for code size, gas, or anything really.
// Not audited. Correctness may vary.
pragma solidity ^0.5.1;
pragma experimental ABIEncoderV2;
import './SafeMath.sol';
import './ECVerify.sol';

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
  uint256 forfeiture_fee_nonce = 0; // can this be memory?

  /* Maybe a way to corral all external calls to transition function
  struct JournalEntry {
    ERC20 asset;
    address from;
    address to;
    uint256 amount;
  }

  // Number of transfers per call is limited.
  function allocate_journal() returns (JournalEntry[] memory)
    private
  {
    return new JournalEntry[](16);
  }

  // Always call at end of function after internal state changes.
  function execute_journal(JournalEntry[] memory entries)
    private
  {
    for (entry in journal_entries) {
      if (entry.from == address(this)) {
        require(entry.asset.transfer(entry.to, entry.amount));
      } else {
        require(entry.asset.transferFrom(entry.from, entry.to, entry.amount));
      }
    }
  }
 */

  // where tokens go when they are burnt.
  // some sort of charity or insurance fund but 0x00 for POC.
  address constant BURN_FUND = address(0);

  ${def_struct eip712DomainTy}
  ${def_eip712StructTypeHash eip712DomainTy}

  // should be a constant
  function eip712DomainSeparator() public view returns (bytes32) {
    string memory ${ref_struct_member "eip712Domain" "name"} =
      "Beacon Exchange";
    string memory ${ref_struct_member "eip712Domain" "version"} =
      "v0.0.0";
    address ${ref_struct_member "eip712Domain" "verifyingContract"} =
      address(this);

    return keccak256(abi.encode(
      ${ref_eip712StructTypeHash eip712DomainTy},
      keccak256(bytes(${ref_struct_member "eip712Domain" "name"})),
      keccak256(bytes(${ref_struct_member "eip712Domain" "version"})),
      ${ref_struct_member "eip712Domain" "verifyingContract"}
    ));
  }

  /*** Off-chain structs ***/
  ${def_struct ittTy}
  ${def_eip712StructTypeHash ittTy}

  ${def_struct poiTy}
  ${def_eip712StructTypeHash poiTy}

  /*** Contract data structures ***/

  enum EscrowState {
    Invalid,
    OnDeposit,
    Withdrawing
  }

  // escrow balance must go through lockup process to withdraw to allow
  // for challenges.
  struct Escrow {
    EscrowState state; // this can probably be inferred by unencumbered_at
    uint256/*timestamp*/ unencumbered_at;
    bytes32 spent_proof; // non-null if proof of spend has been seen
    address beneficiary;
    uint256 amount;
    // Never accessed except for slashing in case of double spend proof
    // Can be inferred from amount.
    uint256 buffer_balance;
  }

  struct Challenge {
    uint256/*timestamp*/ ends_at;
    bytes32 escrow_id;
    // incumbent and challenger could pass as function
    // param every time to save sstore
    address incumbent;
    address challenger;
    uint256 base_amount;
    uint256 dst_amount;
    bool forfeited; // optimization: could compress this into ends_at
  }

  struct Deposit {
    uint256 deposit_balance;
  }


  mapping(address/*token*/ => mapping(address/*user*/ => mapping(bytes32 => Escrow))) escrows;

  mapping(address/*token*/ => mapping(address/*user*/ => Deposit)) deposits;

  mapping(address/*token*/ => uint256) balances;

  mapping(address/*base*/ => mapping(address/*dst*/ => mapping(bytes32/*itt hash*/ => Challenge))) challenges;


  function() external {
    revert();
  }


  function _expired(uint256 timestamp) private view returns (bool) {
    return timestamp < block.timestamp;
  }


  function min(uint256 x, uint256 y)
    private
    pure
    returns (uint256)
  {
    return x < y ? x : y;
  }

  // EIP712 encoding
  function eip712encode(bytes32 structDigest)
    private
    view
    returns (bytes32)
  {
    return keccak256(abi.encodePacked(
      "\x19\x01",
      eip712DomainSeparator(),
      structDigest));
  }


  function _lookup_deposit(ERC20 tok, address sender)
    private
    view // calculating the key should be pure but alas
    returns (Deposit storage)
  {
    return deposits[address(tok)][sender];
  }

  function _lookup_challenge(ERC20 base, ERC20 dst, bytes32 itt_hash)
    private
    view
    returns (Challenge storage)
  {
    return challenges[address(base)][address(dst)][itt_hash];
  }


  // single-entry functions!! must always have offsetting txn
  function _debit(ERC20 tok, address user, uint256 amount)
    private
  {
    if (user == address(this)) {
      // self balances are debit normal
      balances[address(tok)] = balances[address(tok)].add(amount);
    } else {
      Deposit storage d = _lookup_deposit(tok, user);
      // user deposits are credit normal
      d.deposit_balance = d.deposit_balance.sub(amount);
    }
  }
  function _credit(ERC20 tok, address user, uint256 amount)
    private
  {
    if (user == address(this)) {
      // self balances are debit normal
      balances[address(tok)] = balances[address(tok)].sub(amount);
    } else {
      Deposit storage d = _lookup_deposit(tok, user);
      // user deposits are credit normal
      d.deposit_balance = d.deposit_balance.add(amount);
    }
  }

  function deposit(ERC20 tok, uint256 amount)
    external
  {
    // JE
    //   DR
    _debit(tok, address(this), amount);
    //   CR
    _credit(tok, msg.sender, amount);

    tok.transferFrom(msg.sender, address(this), amount);
  }

  function withdraw(ERC20 tok, uint256 amount)
    external
  {
    // JE
    //   DR
    _debit(tok, msg.sender, amount);
    //   CR
    _credit(tok, address(this), amount);

    tok.transfer(msg.sender, amount);
  }

  function _lookup_escrow(ERC20 tok, address sender, bytes32 id)
    private
    view
    returns (Escrow storage)
  {
    return escrows[address(tok)][sender][id];
  }


  function deposit_forfeiture_fee(ERC20 tok, uint256 amount)
    external
    returns (bytes32 forfeiture_fee_id)
  {
    uint256 total_amount = amount.mul(2);

    // Technically there is possibility of overflow but it would be
    // practically impossible to generate enough function calls in a single
    // block to generate a collision.
    forfeiture_fee_nonce++; // Alternative: User-provided nonce?
    //   Could save gas but requires extra checks.

    bytes32 _blockhash = blockhash(block.number);
    bytes32 id = keccak256(abi.encodePacked(forfeiture_fee_nonce, _blockhash));

    Escrow storage escrow =
      _lookup_escrow(tok, msg.sender, id);

    // Check that our uuid generation worked.
    assert(escrow.state == EscrowState.Invalid);

    escrow.state = EscrowState.OnDeposit;
    escrow.unencumbered_at = uint256(-1);

    // JE
    assert(amount + escrow.buffer_balance == total_amount);
    //   DR
    _debit(tok, msg.sender, total_amount);
    //   CR
    escrow.amount = amount;
    // optimization opp: skip sstore and trust escrow_balance == buffer_balance
    escrow.buffer_balance = amount;

    return id;
    // TODO LOG
  }


  function _slash_escrow(ERC20 tok, address owner, bytes32 id, address claimant)
    private
  {
    Escrow storage escrow = _lookup_escrow(tok, owner, id);
    uint256 amount = escrow.amount;
    uint256 error_bit = amount % 2;
    // arbitrarily assign the error bit.
    uint256 claim1_amount = amount.div(2) + error_bit;
    uint256 claim2_amount = amount.div(2);

    // JE
    assert(claim1_amount + claim2_amount == amount);
    //   DR
    escrow.amount = 0; // redundant with later delete
    //   CR
    _credit(tok, escrow.beneficiary, claim1_amount);
    _credit(tok, claimant, claim2_amount);

    // JE
    assert(escrow.buffer_balance == amount);
    //   DR
    escrow.buffer_balance = 0; // redundant with later delete
    //   CR
    _credit(tok, BURN_FUND, amount);

    delete escrows[address(tok)][owner][id];
  }


  // Returns false if
  //   the escrow is expired,
  //   the escrow is already encumbered by a challenge
  //   the escrow got slashed
  function _initiate_escrow_claim(ERC20 tok, address owner, bytes32 id, uint256 unencumbered_at, address claimant, bytes32 spent_proof)
    private
    returns (bool success)
  {
    assert(unencumbered_at > block.timestamp);

    Escrow storage escrow = _lookup_escrow(tok, owner, id);

    // Perhaps this should be an assert, and this validation should
    // be performed by the caller.
    require(escrow.state != EscrowState.Invalid,
            "Escrow invalid");

    /* KEY LOGIC */

    if (_expired(escrow.unencumbered_at)) {
      return false;
    }
    if (spent_proof == bytes32(0)) { // Simple withdraw. No proof of ITT.
      if (escrow.beneficiary != claimant) {
        // Already encumbered.
        return false;
      }
    } else {
      if (escrow.spent_proof != bytes32(0) && escrow.spent_proof != spent_proof) {
        _slash_escrow(tok, owner, id, claimant);
        return false;
      }
    }

    escrow.state = EscrowState.Withdrawing;
    escrow.unencumbered_at = unencumbered_at;
    escrow.beneficiary = claimant;

    return true;
  }


  function initiate_escrow_withdraw(ERC20 tok, bytes32 id)
    external
  {
    uint256 unencumbered_at = block.timestamp + LOCKUP_PERIOD_SECONDS;
    _initiate_escrow_claim(tok, msg.sender, id, unencumbered_at, msg.sender, bytes32(0));
  }


  function contest_escrow_withdraw(ERC20 tok, ITT memory itt, bytes memory mkr_sig)
    public // should be external but no struct in calldata
  {
    bytes32 itt_digest = ${ref_eip712HashStruct "itt" ittTy};
    require(ECVerify.ecverify(eip712encode(itt_digest), mkr_sig) == itt.sender,
           "Invalid signature");

    uint256 unencumbered_at = block.timestamp + itt.challenge_period_seconds;
    _initiate_escrow_claim(tok, itt.sender, itt.forfeiture_fee_id, unencumbered_at, msg.sender, itt_digest);

    // TODO create challenge which is forfeited
  }


  function finalize_escrow_withdraw(ERC20 tok, address owner, bytes32 escrow_id)
    external
  {
    Escrow storage escrow = _lookup_escrow(tok, owner, escrow_id);

    require(_expired(escrow.unencumbered_at),
           "Escrow not withdrawable yet");

    address beneficiary = escrow.beneficiary;

    // Allow proxy to call?
    // FIXME pass sender as param
    require(msg.sender == beneficiary,
            "Not the beneficiary");

    uint256 amount = escrow.amount;
    uint256 buffer_amount = escrow.buffer_balance;
    assert(amount == buffer_amount);

    // JE
    //   DR
    escrow.amount = 0;
    //   CR
    _credit(tok, beneficiary, amount);

    // JE
    //   DR
    escrow.buffer_balance = 0;
    //   CR
    _credit(tok, owner, buffer_amount);

    //delete escrow; <-- does not compile
    delete escrows[address(tok)][owner][escrow_id];

    // TODO LOG
  }


  // Passing structs via calldata fails solc with unimplemented error
  // function exchange(ITT calldata itt, POI calldata poi,...)
  // Alternative: exchange(ITT memory itt,...) public
  // which requires ABIEncoderV2 instead of asm (pick your poison!)
  function exchange(
    ITT memory itt,
    POI memory poi,
    bytes memory tkr_sig
    )
    public //external
    returns (bool success)
  {
    // check based on domain separator?
    require(address(this) == itt.BEACON_CONTRACT,
            "Wrong ITT contract");
    require(address(this) == poi.BEACON_CONTRACT,
            "Wrong POI contract");

    // require maker signature for POI?
    // (sig for ITT hash not required since POI references the ITT)
    //require(ECVerify.ecverify(eip712encode(poi_digest), mkr_sig) == itt.sender);
    // Allow proxy to call?
    require(msg.sender == itt.sender, "Unauthorized");


    bytes32 itt_digest = ${ref_eip712HashStruct "itt" ittTy};
    require(itt_digest == poi.itt_hash,
           "ITT hash does not match");

    bytes32 poi_digest = ${ref_eip712HashStruct "poi" poiTy};

    // require taker signature for POI hash
    // TODO consider wrapping in 'Accept' struct
    require(ECVerify.ecverify(eip712encode(poi_digest), tkr_sig) == poi.sender,
           "Invalid signature");

    ERC20 base = ERC20(itt.base);
    ERC20 dst = ERC20(itt.dst);

    uint256 unencumbered_at = block.timestamp + LOCKUP_PERIOD_SECONDS;
    if (!_initiate_escrow_claim(base, itt.sender, itt.forfeiture_fee_id, unencumbered_at, itt.sender, itt_digest)) {
      return false;
    }

    // JE
    //   DR
    _debit(base, itt.sender, itt.base_amount);
    //   CR
    _credit(base, poi.sender, itt.base_amount);

    // JE
    //   DR
    _debit(dst, poi.sender, itt.dst_amount);
    //   CR
    _credit(dst, itt.sender, itt.dst_amount);

    return true;
  }

  function initiate_challenge(ITT memory itt, bytes memory mkr_sig)
    public/*should be external*/
  {
    address challenger = msg.sender;

    bytes32 itt_digest = ${ref_eip712HashStruct "itt" ittTy};
    require(ECVerify.ecverify(eip712encode(itt_digest), mkr_sig) == itt.sender,
           "Invalid signature");

    ERC20 base = ERC20(itt.base);
    ERC20 dst = ERC20(itt.dst);
    Challenge storage c = _lookup_challenge(base, dst, itt_digest);
    require(c.ends_at == 0,
            "ITT already challenged");

    uint256 ends_at = block.timestamp + itt.challenge_period_seconds;

    /* assumption: forfeiture fee token is itt.base. */
    bool valid = _initiate_escrow_claim(base, itt.sender, itt.forfeiture_fee_id, ends_at, challenger, itt_digest);
    if (valid) {
      // modify state.
      // JE
      //   DR
      _debit(base, itt.sender, itt.base_amount);
      _debit(dst, challenger, itt.dst_amount);
      //   -->
      c.ends_at = ends_at;
      c.escrow_id = itt.forfeiture_fee_id;
      c.incumbent = itt.sender;
      c.challenger = challenger;
      c.base_amount = itt.base_amount; // CR
      c.dst_amount = itt.dst_amount; // CR
      c.forfeited = false; // could omit to save sstore
    }
  }

  function forfeit_challenge(ERC20 base, ERC20 dst, bytes32 itt_hash)
    external
  {
    Challenge storage c = _lookup_challenge(base, dst, itt_hash);

    require(c.incumbent == msg.sender,
            "Not authorized");

    require(!_expired(c.ends_at),
           "Challenge ended");

    c.forfeited = true;

    // JE
    uint256 base_amount = c.base_amount;
    //   DR
    c.base_amount = 0;
    //   CR
    _credit(base, c.incumbent, base_amount);
  }

  // If msg.sender == incumbent, accept challenge and do the trade.
  // If msg.sender == challenger,
  //   check that the challenge has expired or been forfeited and return
  //   the funds to challenger.
  // Otherwise, revert.
  function resolve_challenge(ERC20 base, ERC20 dst, bytes32 itt_hash)
    external
  {
    // optimization opportunity: if read-only, just make copy.
    Challenge storage c = _lookup_challenge(base, dst, itt_hash);

    address mkr = c.incumbent;
    address tkr = c.challenger;

    Escrow storage escrow = _lookup_escrow(base, mkr, c.escrow_id);

    if (msg.sender == c.incumbent) {
      require(!c.forfeited,
              "Already forfeited");

      // JE
      //   DR
      _debit(base, mkr, c.base_amount);
      //   CR
      c.base_amount = 0; // for clarity; redundant with later delete

      // JE
      //   DR
      _debit(dst, tkr, c.dst_amount);
      //   CR
      c.dst_amount = 0; // for clarity; redundant with later delete.

      escrow.beneficiary = c.incumbent;
      escrow.unencumbered_at =
        min(block.timestamp, c.ends_at) + LOCKUP_PERIOD_SECONDS;

      assert(escrow.spent_proof == itt_hash); // TODO double check

    } else if (msg.sender == c.challenger) {

      require(_expired(c.ends_at)
              || escrow.state == EscrowState.Invalid/*escrow was slashed*/,
              "Challenge not ended yet");
      if (escrow.state != EscrowState.Invalid) {
        // challenge expired, add additional lockup to give opportunity
        // for double spend proof to be provided in the case of very
        // short lockups.
        assert(escrow.beneficiary == c.challenger);
        assert(escrow.spent_proof == itt_hash);
        escrow.unencumbered_at = c.ends_at + LOCKUP_PERIOD_SECONDS;
      } else {
        // the escrow was slashed, funds are free to withdraw
      }

      // JE
      uint256 amt = c.dst_amount;
      //   DR
      c.dst_amount = 0;
      //   CR
      _credit(dst, c.challenger, amt);

    } else {
      // Allow proxy to call?
      require(false,
              "Not authorized");
    }

    delete challenges[address(base)][address(dst)][itt_hash];
  }
}
