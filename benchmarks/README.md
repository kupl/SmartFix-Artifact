# SmartFix benchmarks

## Structure
- `cve`: corresponds to ``IO Bench`` in the paper. The directory contains 200 contracts with integer over/underflow bugs. Each bug is annotated with ``<IO_VUL>``.
- `leaking_suicidal`: corresponds to ``EL-SU Bench`` in the paper. The directory contains 104 contracts with ether-leak and suicidal vulnerabilities. Each vulnerability is annotated with ``<LEAKING_VUL>`` (for ether-leak) or ``<SUICIDAL_VUL>`` (for suicidal). 
- `reentrancy`: corresponds to ``RE Bench`` in the paper. The directory contains 47 contracts that contain reentrancy bugs. Each bug is annotated with ``<RE_VUL>``.
- `txorigin`: corresponds to ``TX Bench`` in the paper. The directory contains 10 contracts with improper use of tx.origin. Each bug is annotated with ``<TX_VUL>``.
- `mix-meta.csv`: meta information for 361 benchmark contracts.
