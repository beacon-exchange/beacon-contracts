// PROOF OF CONCEPT!!
// Not optimized for code size, gas, or anything really.
// Not audited. Correctness may vary.
pragma solidity ^0.5.1;
// pragma experimental ABIEncoderV2;
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
  uint256 nonce = 0; // can this be memory?

  // can't actually use these newtypes as map keys
  // so they are not really used
  struct UserAddress {
    address addr;
  }
  struct TokenAddress {
    address addr;
  }

  struct JournalEntry {
    ERC20 asset;
    address from;
    address to;
    uint256 amount;
  }

  // Number of transfers per call is limited.
  JournalEntry[] memory journal_entries = new JournalEntry[](16);

  // Always call at end of function after internal state changes.
  function execute_journal()
    private
  {
    for (entry in journal_entries) {
      entry.asset.transferFrom(entry.from, entry.to, entry.amount);
    }
  }

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
    uint256/*timestamp*/ unencumbered_at;
    // Non-null if proof of ITT referencing this escrow has been seen.
    address claimant;
    uint256 amount;
    // Never accessed except for slashing in case of double spend proof
    uint256 buffer_balance;
  }


  mapping(address/*user*/ => mapping(bytes32 => Escrow)) escrows;


  function() external {
    revert();
  }


  function _lookup_escrow(ERC20 tok, address sender, bytes32 id)
    private
    view // calculating the key should be pure but alas
    returns (Escrow storage)
  {
    return escrows[sender][id];
  }


  function deposit_forfeiture_fee(ERC20 tok, uint256 amount)
    external
    returns (bytes32 forfeiture_fee_id)
  {
    uint256 total_amount = amount.mul(2);

    // Technically there is possibility of overflow but it would be
    // practically impossible to generate enough function calls in a single
    // block to generate a collision.
    this.nonce++; // Alternative: User-provided nonce?
    //   Could save gas but requires extra checks.

    bytes32 constant blockhash = block.blockhash(block.number);
    bytes32 constant id = keccak256(abi.encodePacked(this.nonce, blockhash));

    Escrow storage escrow =
      _lookup_escrow(tok, msg.sender, id);

    escrow.state = EscrowState.OnDeposit;
    escrow.unencumbered_at = (-1);

    escrow.amount = amount;
    // optimization opp: skip sstore and trust escrow_balance == buffer_balance
    escrow.buffer_balance = amount;

    assert(escrow.amount + escrow.buffer_balance == total_amount);

    // approval in separate step
    require(tok.transferFrom(msg.sender, address(this), total_amount),
           "deposit failed: transferFrom");
    // TODO LOG
  }


  function _slash_escrow(ERC20 tok, address owner, bytes32 id, address claimant)
    private
  {
    assert(escrow.amount == escrow.buffer_amount);

    uint256 amount = escrow.amount;
    uint256 error_bit = amount % 2;
    uint256 claim1_amount = amount.div(2);
    // arbitrarily assign the error bit.
    uint256 claim2_amount = amount.div(2) + error_bit;

    // TODO !!!UNSAFE!!! withdraw in two steps
    tok.transfer(escrow.claimant, claim1_amount);
    tok.transfer(claimant, claim2_amount);
    tok.transfer(BURN_FUND, amount);
  }


  // claimant is nonzero iff there is record of a valid ITT referencing
  // this forfeiture fee.
  function _initiate_escrow_claim(ERC20 tok, address owner, bytes32 id, uint256 unencumbered_at, address claimant)
    private
    returns (bool success)
  {
    assert(unencumbered_at > block.timestamp);

    Escrow storage escrow = _lookup_escrow(tok, owner, id);

    require(escrow.state != EscrowState.Invalid,
            "Escrow invalid");

    /* KEY LOGIC */

    if (escrow.unencumbered_at < block.timestamp) {
      return false;
    }
    if (!claimant) { // Simple withdraw. No proof of ITT.
      // TODO double check msg.sender semantics for private calls
      if (escrow.claimant != msg.sender) {
        // Already encumbered.
        return false;
      }
    } else {
      if (escrow.claimant != claimant) {
        _slash_escrow(escrow, claimant);
        delete escrows[tok][owner][id];
        return false;
      }
      escrow.claimant = claimant;
    }

    escrow.state = EscrowState.Withdrawing;
    escrow.unencumbered_at = unencumbered_at;

    return true;
  }


  function initiate_escrow_withdraw(ERC20 tok, bytes32 id)
    external
  {
    uint256 unencumbered_at = block.timestamp + LOCKUP_PERIOD_SECONDS;
    address claimant = address(0);
    _initiate_escrow_claim(tok, msg.sender, id, unencumbered_at, claimant);
  }


  function contest_escrow_withdraw(ERC20 tok, ITT memory itt, bytes memory mkr_sig)
    public // should be external but no struct in calldata
  {
    // TODO verify itt hash/sig

    unencumbered_at = block.timestamp + itt.challenge_period_seconds;
    _initiate_escrow_claim(tok, itt.sender, itt.forfeiture_fee_id, unencumbered_at, msg.sender);
  }


  function finalize_escrow_withdraw(ERC20 tok, address owner, bytes32 escrow_id)
    external
  {
    _finalize_escrow_withdraw(tok, owner, escrow_id, msg.sender);
  }

  function _finalize_escrow_withdraw(ERC20 tok, address owner, bytes32 escrow_id)
    private
  {
    Escrow storage escrow = lookup_escrow(tok, owner, escrow_id);

    require(escrow.unencumbered_at < block.timestamp,
           "Escrow not withdrawable yet");

    address beneficiary;

    if (escrow.claimant) {
      beneficiary = escrow.claimant;
    } else {
      beneficiary = owner;
    }
    // Allow proxy to call?
    // FIXME pass sender as param
    require(msg.sender == beneficiary,
            "Not the beneficiary");

    // can use unsafe math?
    uint256 amount = escrow.amount.add(escrow.buffer_amount);

    // clear storage
    //delete escrow; <-- does not compile
    delete escrow[tok][owner][escrow_id];

    // TODO move into separate step.
    tok.transfer(beneficiary, amount);

    // TODO LOG
  }


  // Passing structs via calldata fails solc with unimplemented error
  // function exchange(ITT calldata itt, POI calldata poi,...)
  // Alternative: exchange(ITT memory itt,...) public
  // which requires ABIEncoderV2 instead of asm (pick your poison!)
  function exchange(
    bytes32[${length (_members ittTy)}] calldata ittBytes,
    bytes32[${length (_members poiTy)}] calldata poiBytes,
    bytes calldata tkr_sig
    ) returns (bool success)
    external
  {
    ITT memory itt;
    ${unpack_struct ittTy "ittBytes" "itt"}

    POI memory poi;
    ${unpack_struct poiTy "poiBytes" "poi"}

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
    require(ECVerify.ecverify(eip712encode(poi_digest), tkr_sig) == poi.sender);

    escrow_unencumbered_at = block.timestamp + LOCKUP_PERIOD_SECONDS;
    escrow_valid = _initiate_escrow_claim(tok, itt.sender, id, unencumbered_at, itt.sender);
    if (!escrow_valid) {
      return false;
    }

    require(
      ERC20(itt.base).transferFrom(itt.sender, poi.sender, itt.base_amount),
      "exchange: base -> dst");
    require(
      ERC20(itt.dst).transferFrom(poi.sender, itt.sender, itt.dst_amount),
      "exchange: dst -> base");

    return true;
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

  struct Challenge {
    uint256/*timestamp*/ ends_at;
    bytes32 escrow_id;
    address incumbent;
    address challenger;
    uint256 base_amount;
    uint256 dst_amount;
    bool forfeited;
  }
  function initiate_challenge(ITT memory itt, POI memory poi, bytes memory mkr_sig)
    public/*should be external*/
  {
    // TODO validate sigs, hashes

    Challenge storage c = _lookup_challenge(itt.base, itt.dst, poi.itt_hash);
    require(!c.ends_at,
            "ITT already challenged");

    uint256 ends_at = block.timestamp + itt.challenge_period_seconds;

    // TODO xfer in

    /* TODO revisit assumption: forfeiture fee token is itt.base. */
    if (_initiate_escrow_claim(itt.base, itt.sender, itt.forfeiture_fee_id, ends_at, poi.sender)) {
      // modify state.
      c = Challenge({
        ends_at: ends_at,
        escrow_id: itt.forfeiture_fee_id,
        incumbent: itt.sender,
        challenger: poi.sender,
        base_amount: itt.base_amount,
        dst_amount: itt.dst_amount,
        forfeited: false
      });
    }
  }

  function forfeit_challenge(ERC20 base, ERC20 dst, bytes32 itt_hash)
    external
  {
    Challenge storage c = _lookup_challenge(base, dst, itt_hash);
    require(c.incumbent == itt.sender,
            "Not authorized");
    require(c.ends_at > block.timestamp,
           "Challenge ended");

    uint256 base_amount = c.base_amount;
    c.base_amount = 0;
    c.forfeited = true;

    base.transfer(c.incumbent, base_amount);
  }

  function resolve_challenge(ERC20 base, ERC20 dst, bytes32 itt_hash)
    external
  {
    // Don't grab pointer.
    Challenge c = _lookup_challenge(base, dst, itt_hash);

    if (msg.sender == c.incumbent) {
      require(!c.forfeited,
              "Already forfeited");
      address mkr = c.incumbent;
      address tkr = c.challenger;

      journal_entries.push(JournalEntry(base, address(this), tkr, c.base_amount));
      journal_entries.push(JournalEntry(dst, address(this), mkr, c.dst_amount));

      // TODO reassign escrow beneficiary to incumbent, restart withdrawal period.
    } else if (msg.sender == c.challenger) {
      require(c.ends_at < block.timestamp,
              "Challenge not ended yet");
      if (c.forfeited) {
        journal_entries.push(JournalEntry(dst, address(this), tkr, dst_amount));
        _finalize_escrow_withdrawal(base, c.incumbent, c.escrow_id, msg.sender);
      }
    } else {
      // Allow proxy to call?
      require(false,
              "Not authorized");
    }

    delete challenges[base][dst][itt_hash];

    execute_journal();
  }
}
