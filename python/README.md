# Python workflow

This directory contains the thesis agent-based MRSA model.

## Setup

From this directory, create an environment and install the package:

```bash
python -m venv .venv
. .venv/bin/activate
python -m pip install -r requirements.txt
python -m pip install -e .
```

## Run

From the repository root, after activating the environment:

```bash
python python/scripts/run_batch.py
```

Raw batch output is written to `data/raw/`. The model implementation is in `src/mrsa_abm/`.
