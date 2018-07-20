pragma solidity ^0.4.24;

contract Linearization {

  event E1(address sender, uint amount);
  event E2(string tag, bytes4 uuid);

  function e12() public {
    emit E1(msg.sender, 0);
    emit E2("hello", 0xdeadbeef);
  }

  function e21() public {
    emit E2("hello", 0xdeadbeef);
    emit E1(msg.sender, 0);
  }

  function e1() public {
    emit E1(msg.sender, 0);
  }

  function e2() public {
    emit E2("hello", 0xdeadbeef);
  }
}