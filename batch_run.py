import math
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import random
import statsmodels.api as sm
from mesa import Agent, Model
from mesa.datacollection import DataCollector
from models import NoWardModel, WardModel, PatientAssignmentModel, AdmissionWardModel, AdmissionPatientAssignmentModel
from agents import Patients, Nurses 

####

# --- Model Parameters ---
PARAMS = {
    "a": 5.0, "x": 0.2, "n": 0.3, "s_b": 0.25, "s_p": 0.1,
    "theta": 0.8, "low": 0.1, "high": 0.5,
    "psi_s": 0.0833, "psi_r": 0.0454, "iota": 0.8,
    "kappa": 0.1428, "m_s": 0.3, "delta_m": 0.25,
    "mu_c": 0.015, "b_np": 0.09, "b_pn": 0.3,
    "lambda_cs": 0.02, "lambda_cr": 0.02,
    "lambda_is": 0.01, "lambda_ir": 0.01,
    # Granular model parameters
    "nurse_max_interactions": 7,
    "compliance_decrease_rate": 0.05,
    "prob_resistance_emergence": 0.001,
    "initial_patients": 275,
    "max_patient_capacity": 400,
    "admission_rate_per_step": 10,
    # Admission/Cohort model parameters
    "admission_ward_id": 0,
    "resistant_cohort_ward_id": 1,
    "admission_period": 3,
    "compliance_boost": 0.25,
    # Nurse ratio control
    "target_patient_to_nurse_ratio": 10 # Set > 0 to control ratio (e.g., 4 for 1:4). If 0, uses fixed num_nurses.
}

####

# --- Batch run ---

def run_simulation(model_class, max_iterations, max_steps, output_csv_path, model_title):
    """
    Runs a batch simulation for a given model class, saves the raw data to CSV,
    and returns the aggregated mean results.
    """
    print(f"Starting batch run for {model_title}...")
    
    # Create a list to hold each iteration's DataFrame
    all_run_data = []

    for it in range(max_iterations):
        # Initialize the model
        model = model_class()
        
        # Run the model for max_steps
        for _ in range(max_steps):
            model.step()
            
        # Get the model data for this run
        model_data = model.datacollector.get_model_vars_dataframe()
        model_data['iteration'] = it
        model_data['Step'] = range(1, max_steps + 1)
        all_run_data.append(model_data)
        
        if (it + 1) % 10 == 0:
            print(f"  ...completed {it + 1}/{max_iterations} iterations for {model_title}.")

    # Concatenate all run data into a single DataFrame
    full_df = pd.concat(all_run_data, ignore_index=True)
    
    # Save the raw data to CSV
    full_df.to_csv(output_csv_path, index=False)
    print(f"Saved raw data for {model_title} to {output_csv_path}")

    # Calculate and return the mean DataFrame
    df_mean = full_df.groupby("Step").mean(numeric_only=True)
    return df_mean

# --- Main execution ---
if __name__ == "__main__":
    MAX_ITERATIONS = 50
    MAX_STEPS = 365

    models_to_run = [
        {"class": NoWardModel, "title": "No Ward Model", "csv": "no_ward_data.csv"},
        {"class": WardModel, "title": "Ward Model", "csv": "ward_data.csv"},
        {"class": PatientAssignmentModel, "title": "Patient Assignment Model", "csv": "patient_assignment_data.csv"},
        {"class": AdmissionWardModel, "title": "Admission Ward Model", "csv": "admission_ward_data.csv"},
        {"class": AdmissionPatientAssignmentModel, "title": "Admission Patient Assignment Model", "csv": "admission_assignment_data.csv"}
    ]

    all_means = {}
    for model_info in models_to_run:
        mean_df = run_simulation(
            model_class=model_info["class"],
            max_iterations=MAX_ITERATIONS,
            max_steps=MAX_STEPS,
            output_csv_path=model_info["csv"],
            model_title=model_info["title"]
        )
        all_means[model_info["title"]] = mean_df
    
