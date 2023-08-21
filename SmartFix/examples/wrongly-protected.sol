contract wronglyProtected {
  address owner = msg.sender;

  modifier onlyOwner {
    if (msg.sender == owner) revert();
    _;
  }

  function kill () public onlyOwner {
    selfdestruct (owner);
  }

}
