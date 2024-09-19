<h1 align="center">
  hant
</h1>
<p align="center">
  <img src="./img/logo.png" width="200" />
</p>

`hani` is a verification framework for hybrid automata networks. It utilizes event and bound sequences as verification instructions, encoding both these instruction and the HAN under verification as SMT formulas. 

### Preliminary

Install Z3 SMT solver on your computer.

#### Ubuntu
```bash
sudo apt update
sudo apt -y install z3
```

#### macOS
```bash
brew install z3
```

#### Windows
Follow the official [installation guide](https://github.com/Z3Prover/z3).

### Installation

To run `hani`, follow these steps:
1. Install Haskell toolchains using [GHCup](https://www.haskell.org/ghcup/).
  - GHC 9.2.7
  - cabal 3.10.1.0
  - Stack 2.9.3

2. Clone this repository.


3. Navigate to the directory where the repository was downloaded.
### Reproducing experimental results

#### Observing the experimental results

- Reproduce RQ1 with the following command.
```bash
stack run +RTS -N -- parallel experiment1
```

- Run the following command that measures the coverage of RQ1.

```bash
stack run +RTS -N -- coverage experiment1
```

- Reproduce RQ2 with the following command.

To reproduce the results of `hani`:
```bash
stack run +RTS -N -- parallel experiment2
```

To reproduce the results of `s-hani`:
```bash
stack run +RTS -N -- sequential experiment2
```

To reproduce the results of `n-hani`
```bash
stack run +RTS -N -- nopruning experiment2
```

### Raw Data

The raw data of the experimental results are available in the `data` directory.