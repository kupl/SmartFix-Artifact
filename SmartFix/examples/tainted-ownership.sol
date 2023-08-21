contract TaintedOwnership {
  address owner = msg.sender;

  modifier onlyOwner {
    require (msg.sender == owner);
    _;
  }

  function setOwner(address _owner)  {
    owner = _owner;
  }

  function kill () public onlyOwner {
    selfdestruct (owner);
  }

}
