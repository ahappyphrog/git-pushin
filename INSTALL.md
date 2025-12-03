# Installation Instructions

## Prerequisites

- Dune `opam install dune`

## Setup

```bash
git clone https://github.com/ahappyphrog/git-pushin.git
cd git-pushin
dune build
```

`dune build` compiles the CLI and writes artifacts under `_build/default`.

## Run the app

```bash
dune exec git-pushin
```

This launches the CLI and any events collected are stored 
in `meetings.json`.
