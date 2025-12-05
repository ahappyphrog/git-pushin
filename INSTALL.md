# Installation Instructions

## Prerequisites

### OCaml Packages
Install required OCaml packages via opam:
```bash
opam install dune yojson graphics
```

### macOS GUI Support (XQuartz) 
For GUI mode on macOS, install and run XQuartz:
```bash
brew install --cask xquartz

open -a XQuartz

export DISPLAY=:0
# If XQuartz doesnt open automatically, open the app yourself on mac
# If you get any GUI errors, try closing and relaunching XQuartz
```

## Setup

```bash
git clone https://github.com/ahappyphrog/git-pushin.git
cd git-pushin
dune build
```

`dune build` compiles the CLI and writes artifacts under `_build/default`.

## Run the app

- GUI (default): `dune exec git-pushin`
- CLI: `dune exec git-pushin -- --cli`

Any events collected are stored in `meetings.json`.
