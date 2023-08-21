contract Unprotected {
  address owner = msg.sender;

  modifier onlyOwner {
    require (msg.sender == owner);
    _;
  }

  function kill () public {
    selfdestruct (owner);
  }

}
