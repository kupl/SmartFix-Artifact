# SmartFix-Artifact (ESEC/FSE 2023)
SmartFix is a tool for automatically fixing vulnerable smart contracts written in Solidity.
This repository contains the artifact for reproducing the main experimental results in our paper accepted to ESEC/FSE 2023:

[SmartFix: Fixing Vulnerable Smart Contracts by Accelerating Generate-and-Verify Repair using Statistical Models](./paper.pdf)

Specifically, we provide step-by-step instructions for reproducing Table 1, Table 2, and Figure 4 in the paper.
This artifact is organized as follows.
* ``SmartFix``: the source code of SmartFix
* ``benchmarks``: the benchmarks used for the experiments
* ``fix_experiment``: the directory that contains scripts that will be executed within docker containers
* ``fix_result``: the directory that will store the outputs of each tool
* ``index_sGuard``: the modified file from [the implementation of sGuard](https://github.com/duytai/sGuard/tree/643c5f67f21d5a433965218a84ce407d93ccdc23) for enabling to specify Solidity compiler versions and the names of main contracts to be patched (see Section 5.1 in our paper)
* ``wrapper``: the directory that contains scripts that will be executed on your host machine 

## Requirements
See [REQUIREMENTS.md](./REQUIREMENTS.md).

## Installation
See [INSTALL.md](./INSTALL.md); you can find a basic usage example of the artifact.

## Table 1 (Section 5.2)

#### Step 1: Running SmartFix (Ours)
```
./wrapper/smartfix.sh 40
```
* The above command invokes the tool using 40 cores at most (i.e., up to 40 contracts are fixed in parallel). You can replace the above argument ``40`` depending on your hardware specifications.
* *Expected runtime with 40 cores: 8 hours 30 minutes*
* The fixing results of SmartFix will be stored in the directory ``fix_result/smartfix_MMDD_hhmm`` of your host machine (MM: month, DD: day, hh: hour, mm: minute).

#### Step 2: Running sGuard (IEEE S&P'21)
```
./wrapper/sguard.sh 40
```
* The above command invokes the tool using 40 cores at most (i.e., up to 40 contracts are fixed in parallel). You can replace the above argument ``40`` depending on your hardware specifications.
* *Expected runtime with 40 cores: 1 hour 40 minutes*
* The fixing results of sGuard will be stored in the directory ``fix_result/sguard_MMDD_hhmm`` of your host machine (MM: month, DD: day, hh: hour, mm: minute).

#### Step 3: Checking the execution results
* The summarized execution results of SmartFix can be found in ``fix_result/smartfix_MMDD_hhmm/all_stat.txt``.
  * In that file, ``X_Lab`` (where ``X`` is one of ``IO``, ``RE``, ``TX``, ``EL``, ``SU``, and ``ALL``) indicates ``#B`` in Table 1 of the paper, ``X_Run`` indicates ``#B^R``, and ``X_Gen`` indicates ``#G``.
  * Regarding ``X_Cor`` (which corresponds to ``#C`` in Table 1 of the paper), please refer to *Note2* in the below.
* The summarized execution results of sGuard can be found in ``fix_result/sguard_MMDD_hhmm/io_stat.txt``, ``fix_result/sguard_MMDD_hhmm/re_stat.txt``, and ``fix_result/sguard_MMDD_hhmm/tx_stat.txt``.
  * In each file, ``#Bug Run`` and ``#Bug Fix Gen`` indicate ``#B^R`` and ``#G`` in Table 1 of the paper, respectively.
  * Regarding ``#C`` in Table 1 of the paper, please refer to *Note2* in the below.
* *Note1:* The obtained numbers can be slightly different from the numbers in Table 1 of the paper (e.g., the randomness of Z3). Nevertheless, the overall tendency will be similar. 
* *Note2:* To obtain the numbers in the column ``#C`` (the number of correctly fixed bugs) of Table 1, you need to manually verify correctness of generated patches, by comparing an original contract (in [benchmarks](./benchmarks)) and a corresponding patched contract.
  * Patches generated by SmartFix can be found by inspecting the last part of the repair log file for each benchmark contract.
    * For example, to validate the patch for the benchmark contract [benchmarks/cve/2018-11411.sol](./benchmarks/cve/2018-11411.sol), a corresponding repair log file can be found in ``fix_result/smartfix_MMDD_hhmm/io/2018-11411/2018-11411.txt``.
    * If you open the log file, you can see a log similar to the below.
      ```
      ...
      ========== Patch info ==========
      - 1, Size: 4., Edit: 4, Iter: 23, Time: 4220.26358104, /home/opam/fix_result/0816_1325/io/2018-11411/candidates/cand_117/patch.sol
      ...      
      ```
    * The above log states that the contract repaired by SmartFix is located at ``fix_result/smartfix_MMDD_hhmm/io/2018-11411/candidates/cand_117/patch.sol`` of your host machine. In that contract, the parts patched by SmartFix will have comments that start with ``<FIX>``.
  * Patches generated by sGuard can be found in the following directories of your host machine: ``fix_result/sguard_MMDD_hhmm/io/fixed``, ``fix_result/sguard_MMDD_hhmm/re/fixed``, and ``fix_result/sguard_MMDD_hhmm/tx/fixed``.

## Table 2 (Section 5.2)
```
./wrapper/patch_simplicity.sh fix_result/smartfix_MMDD_hhmm fix_result/sguard_MMDD_hhmm
```
* In the above command, ``fix_result/smartfix_MMDD_hhmm`` and ``fix_result/sguard_MMDD_hhmm`` are the fixing results of SmartFix and sGuard (obtained through Step 1,2 for reproducing Table 1) respectively.
* The results will be shown on your terminal screen and also will be stored in the directory ``fix_result/patch_simplicity.txt`` of your host machine.
* *Note:* The obtained numbers can be slightly different from the numbers in Table 2 of the paper (e.g., the randomness of Z3). Nevertheless, the overall tendency will be similar.

## Figure 4 (Section 5.3)
#### Step 1: Running the two variants (Basic, Online) of SmartFix
```
./wrapper/smartfix_variants.sh 40
```
* The above command invokes the tool using 40 cores at most (i.e., up to 40 contracts are fixed in parallel). You can replace the above argument ``40`` depending on your hardware specifications.
* *Expected runtime with 40 cores: 19 hours 40 minutes (11 hours for ``Basic`` + 8 hours 40 minutes for ``On``)*
* The execution results will be stored in the following three directories of your host machine:
  * ``fix_result/basic_MMDD_hhmm`` (MM: month, DD: day, hh: hour, mm: minute)
  * ``fix_result/on_MMDD_hhmm`` (MM: month, DD: day, hh: hour, mm: minute)

#### Step 2: Drawing the plot
```
./wrapper/variant_comparison.sh fix_result/basic_MMDD_hhmm fix_result/on_MMDD_hhmm fix_result/smartfix_MMDD_hhmm
```
* In the above command, ``fix_result/smartfix_MMDD_hhmm`` is the execution result of the final version of SmartFix (obtained through Step 1 for reproducing Table 1).
* The cactus plot will be generated in the directory ``fix_result/variant_comparison/cactus.pdf`` of your host machine.
* *Note:* The obtained figure can be slightly different from Figure 4 of the paper (e.g., the randomness of Z3). Nevertheless, the overall tendency will be similar.

## Tool Maintenance
SmartFix will be maintained in a separate repository: https://github.com/kupl/VeriSmart-public

## Contact
sunbeom_so@korea.ac.kr
