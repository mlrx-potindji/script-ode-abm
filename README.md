# script-ode-abm
This repository contains the R and Python scripts for public access. Repository created 31-08-2025.

**Tchamou Malraux Fleury POTINDJI (2025).**
*"From Compartments to Individuals: A Complementary Analysis of ODE and Agent-Based Models for Simulating methicillin-resistant Staphylococcus aureus Control Strategies in Hospitals."*

The code provides a modular framework for simulating hospital transmission dynamics of MRSA using both compartmental ODE and agent-based models (ABM), complementing the analyses discussed in the thesis.

## Repository Structure

- `R/` – R workflow for the compartmental ODE models.
- `python/` – Python workflow for the agent-based models.
- `data/raw/` – Generated raw simulation output.
- `results/` – Generated figures and tables.

See [R/README.md](R/README.md) and [python/README.md](python/README.md) for workflow-specific instructions. The Python batch run writes CSV files to `data/raw/`; plotting reads those files from there.

## Requirements

- Python 3.9+
- R (for the ODE workflow)
- Python dependencies listed in `python/requirements.txt`

# Citation

If you use this repository (or any code in this repository) in your wourk, please cite

Tchamou Malraux Fleury POTINDJI (2025). 
From Compartments to Individuals: A Complementary Analysis of ODE and Agent-Based Models for Simulating methicillin-resistant Staphylococcus aureus Control Strategies in Hospitals. 
Master’s Thesis, Eberhard Karls University Tübingen.
GitHub repository: https://github.com/mlrx-potindji/script-ode-abm.git
