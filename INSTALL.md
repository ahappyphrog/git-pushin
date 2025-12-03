# Installation Instructions

## Prerequisites

- Dune `opam install dune`
- Bogue UI stack `opam install bogue`

## Setup

```bash
git clone https://github.com/ahappyphrog/git-pushin.git
cd git-pushin
dune build
```

`dune build` compiles the GUI and writes artifacts under `_build/default`.

## Run the app

```bash
dune exec git-pushin
```

This launches the GUI; scheduled events are stored in `meetings.json`.
