import pandas as pd
from pathlib import Path

from .models import (
    AdmissionPatientAssignmentModel,
    AdmissionWardModel,
    NoWardModel,
    PatientAssignmentModel,
    WardModel,
)

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

def main():
    project_root = Path(__file__).resolve().parents[3]
    output_dir = project_root / "data" / "raw"
    output_dir.mkdir(parents=True, exist_ok=True)

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
            output_csv_path=output_dir / model_info["csv"],
            model_title=model_info["title"]
        )
        all_means[model_info["title"]] = mean_df


if __name__ == "__main__":
    main()
    
