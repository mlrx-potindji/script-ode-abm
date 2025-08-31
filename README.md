# script-ode-abm
This repository contains the R and Python script for users and readers public access. Repository created 31-08-2025. 

**Tchamou Malraux Fleury POTINDJI (2025).**
*"From Compartments to Individuals: A Complementary Analysis of ODE and Agent-Based Models for Simulating methicillin-resistant Staphylococcus aureus Control Strategies in Hospitals."*

The code provides a modular framework for simulating hospital transmission dynamics of MRSA using an agent-based model (ABM). It complements the compartmental ODE models discussed in the thesis.

## Repository Structure

- `agents.py` – Core Python script for the agent.
- `models.py` – Model classes definitions and configurations.
- `batch_run.py` – Code to run simulations and batch experiments.
- `plots.py` – Scripts for generating figures and summary statistics.

The batch_run.py file imports classes from both the agents.py and the models.py. Thus, only the batch_run.py needs to be executed. The staffing ration can be customized from the models.py file. After results of the batch run are saved, plots.py can be used to reproduce the plots for the incidence rate of new infected cases and for the cumulative number of resistant cases over time.

pass

- 'The R scripts for ODE models will be added soon'

pass

## Requirements

- Python 3.9+
- Mesa (for agent-based modeling)

# Citation

If you use this repository (or any code in this repository) in your wourk, please cite

Tchamou Malraux Fleury POTINDJI (2025). 
From Compartments to Individuals: A Complementary Analysis of ODE and Agent-Based Models for Simulating methicillin-resistant Staphylococcus aureus Control Strategies in Hospitals. 
Master’s Thesis, Eberhard Karls University Tübingen.
GitHub repository: https://github.com/mlrx-potindji/script-ode-abm.git
