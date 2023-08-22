# Setup
To run experiments in a dockerized environment, build a docker image via the following command.
```
docker build -t smartfix --build-arg CORE=40 .
```
* The above command will use 40 cores in parallel when installing Z3 SMT solver. You can replace the above argument ``40`` depending on your hardware specifications.
* *Expected running time with 40 cores: 12 minutes*


### Basic Testing (Running Example)
Following the instructions below, you can check whether the installation was successful or not.
* Consider the vulnerable contract in [SmartFix/examples/unprotected.sol](./SmartFix/examples/unprotected.sol). You can observe that the function ``kill`` is not properly protected, i.e., the safety-critical instruction ``selfdestruct`` can be executed by anyone.
* To fix the vulnerability of this contract, run the following command.
```
docker run --rm -it -v `pwd`/fix_result:/home/opam/fix_result smartfix /bin/bash -c "cd SmartFix; ./main.native -input examples/unprotected.sol -mode repair -outdir ../fix_result/my_output"
```
*Note:* For each execution, provide fresh output directory names to the ``-outdir`` option: ``-outdir ../fix_result/my_output``, ``-outdir ../fix_result/my_output2``, ``-outdir ../fix_result/my_output3``, etc.
* Then, you will see a log similar to be below on your terminal screen.
```
...
========== Patch info ==========
- 1, Size: 1., Edit: 1, Iter: 1, Time: 0.287813901901, ../fix_result/my_output/candidates/cand_1/patch.sol
...
```
* The above log states that the contract repaired by SmartFix is located at ``fix_result/my_output/candidates/cand_1/patch.sol`` of your host machine. In that contract, the parts patched by SmartFix will have comments that start with ``<FIX>``.
