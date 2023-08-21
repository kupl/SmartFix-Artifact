FROM ocaml/opam:ubuntu-20.04

LABEL maintainer "Sunbeom So <sunbeom_so@korea.ac.kr>"

ARG CORE=40

RUN sudo apt-get update && sudo apt-get -y upgrade
RUN sudo apt-get install -y wget unzip git build-essential software-properties-common curl m4 ocaml-findlib libgmp-dev autoconf vim

# OCaml 4.11.1
RUN opam init && eval $(opam env)
RUN opam switch create 4.11.1 && eval $(opam env)

# OCaml deps
RUN opam update && opam upgrade
RUN eval $(opam env) && opam install -y conf-m4.1 ocamlfind ocamlbuild num yojson batteries ocamlgraph zarith csv pyml

# Z3
WORKDIR /home/opam
RUN sudo wget -q https://github.com/Z3Prover/z3/archive/refs/tags/z3-4.11.2.tar.gz
RUN sudo tar -xvzf z3-4.11.2.tar.gz
RUN sudo chown -R opam z3-z3-4.11.2
WORKDIR /home/opam/z3-z3-4.11.2
RUN eval $(opam env) && \
    python3 scripts/mk_make.py --ml && \
    cd build && \
    make -j $CORE && sudo make install
WORKDIR /home/opam

# Python 3.8
RUN sudo apt-get install -y python3.8 python3.8-distutils python3-pip
RUN pip3 install matplotlib numpy web3 scikit-learn==0.24.1

# Solc
RUN for i in \
   "0.4.11" "0.4.16" "0.4.17" "0.4.18" "0.4.19" "0.4.20" "0.4.21" "0.4.23" "0.4.24" "0.4.25" "0.4.26" \
   "0.5.0" "0.5.1" "0.5.2" "0.5.3" "0.5.4" "0.5.5" "0.5.6" "0.5.7" "0.5.9" \
   "0.5.10" "0.5.11" "0.5.12" "0.5.13" "0.5.14" "0.5.15" "0.5.16" "0.5.17" \
   "0.6.0" "0.6.1" "0.6.2" "0.6.3" "0.6.4" "0.6.5" "0.6.6" "0.6.7" "0.6.8" "0.6.9" "0.6.10" "0.6.11" "0.6.12" \
   "0.7.0" "0.7.1" "0.7.2" "0.7.3" "0.7.4" "0.7.5" "0.7.6" \
   "0.8.0" "0.8.1" "0.8.2" "0.8.3" "0.8.4" "0.8.5" "0.8.6" \
   "0.8.7" "0.8.8" "0.8.9" "0.8.10" "0.8.11" "0.8.12" "0.8.13" \
   "0.8.14" "0.8.15" "0.8.16" "0.8.17"; \
    do \
      wget -q https://github.com/ethereum/solidity/releases/download/v$i/solc-static-linux; \
#      sudo chown -R opam solc-static-linux; \
      chmod +x solc-static-linux; \
      sudo mv solc-static-linux /usr/bin/solc_$i; \
    done
RUN sudo cp /usr/bin/solc_0.4.26 /usr/bin/solc

# SmartFix
WORKDIR /home/opam
COPY ./SmartFix /home/opam/SmartFix
RUN sudo chown -R opam /home/opam/SmartFix
WORKDIR /home/opam/SmartFix
RUN eval $(opam env) && chmod +x build; ./build
# RUN cp main.native ../. && rm -rf * && mv ../main.native .
# RUN mkdir output && sudo chown -R opam output

# sGuard
WORKDIR /home/opam
RUN git clone https://github.com/duytai/sGuard.git
WORKDIR /home/opam/sGuard
RUN git checkout 643c5f67f21d5a433965218a84ce407d93ccdc23
RUN sudo apt-get install -y npm
RUN npm install
COPY ./index_sGuard/index.js /home/opam/sGuard/src/index.js

WORKDIR /home/opam

# benchmarks and scripts
COPY ./benchmarks /home/opam/benchmarks
COPY ./fix_experiment /home/opam/fix_experiment
RUN sudo chown -R opam /home/opam/benchmarks
RUN sudo chown -R opam /home/opam/fix_experiment
RUN mkdir /home/opam/fix_result

CMD ["/bin/bash"]
