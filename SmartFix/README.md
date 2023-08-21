# Implemenation of SmartFix (for FSE '23 review)
This repository contains the implementation of SmartFix tool, which we developed on top of an open-sourced verification tool (https://github.com/kupl/VeriSmart-public, please see Section 4).

## Benchmark
We provide benchmarks via a separate anonymous repository: https://anonymous.4open.science/r/benchmarks-8171/README.md

***NOTE*** If clicking the above url does not work, please copy and paste it to see the material.


## Modified source code of sGuard
We provide the modified source code of sGuard via a separte anonymous repository: https://anonymous.4open.science/r/sguard-modification-4A35/README.md

***NOTE*** If clicking the above url does not work, please copy and paste it to see the material.


## Overall Structure of SmartFix Implementation
The main logic for our repair algorithm is implemented in the directory ``src/repair``. In particular, the following files are the core of our algorithm.
- ``repair.ml``: implements the generate-and-verify repair-loop (Algorithm 1).
- ``genPatch.ml``: implements the patch generation procedure (Section 3.1.1).
- ``runTool.ml``: implements the procedures for invoking the patch verifier (Section 3.1.2).
- ``onlineLearning.ml``: implements the procedures for learning statistical models online (Section 3.2.1).
- ``offlineLearning.ml``: implements the procedures for learning statistical models offline (Section 3.2.2).

## Running Example
* Consider the vulnerable contract in ``examples/unprotected.sol``.
* To fix the contract, run the following command:
```
./main.native -input examples/unprotected.sol -mode repair
```
* Then, SmartFix will produce the following safe contract The fixed part can be seen in the line that contains the ``<FIX>`` comment, which was absent in the original vulnerable contract.
```
contract Unprotected {
  address owner = msg.sender;

  modifier onlyOwner {
    require (msg.sender == owner);
    _;
  }

  function kill () onlyOwner /* <FIX> Add Modifier */ public {
    selfdestruct (owner);
  }

}
```
