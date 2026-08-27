from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.api as sm


MODEL_FILES = {
    "No Ward Model": "no_ward_data.csv",
    "Ward Model": "ward_data.csv",
    "Patient Assignment Model": "patient_assignment_data.csv",
    "Admission Ward Model": "admission_ward_data.csv",
    "Admission Patient Assignment Model": "admission_assignment_data.csv",
}


def load_means(data_dir):
    return {
        name: pd.read_csv(data_dir / filename).groupby("Step").mean(numeric_only=True)
        for name, filename in MODEL_FILES.items()
    }


def main():
    project_root = Path(__file__).resolve().parents[3]
    all_means = load_means(project_root / "data" / "raw")
    output_dir = project_root / "results" / "figures"
    output_dir.mkdir(parents=True, exist_ok=True)

    plt.figure(figsize=(12, 7))
    for name, df_mean in all_means.items():
        denominator = df_mean["Susceptible"].replace(0, np.nan)
        proportion = (df_mean["New_Colonized_R"] + df_mean["New_Infected_R"]) / denominator
        smoothed = sm.nonparametric.lowess(proportion.fillna(0), df_mean.index, frac=0.022)
        plt.plot(smoothed[:, 0], smoothed[:, 1], label=name, linewidth=2)

    plt.xlabel("Time Steps", fontsize=16)
    plt.ylabel("Proportion of new resistant cases, staff ratio 1:10", fontsize=16)
    plt.legend(fontsize=10)
    plt.grid(False)
    incidence_path = output_dir / "comparative_proportion_new_cases_smoothed.png"
    plt.savefig(incidence_path, dpi=300, bbox_inches="tight")
    print(f"Saved plot to {incidence_path}")
    plt.show()
    plt.close()

    plt.figure(figsize=(12, 7))
    for name, df_mean in all_means.items():
        cumulative_resistant = (df_mean["New_Colonized_R"] + df_mean["New_Infected_R"]).cumsum()
        plt.plot(df_mean.index, cumulative_resistant, label=name, linewidth=2)

    plt.xlabel("Time Steps", fontsize=16)
    plt.ylabel("Cumulative number of resistant cases, staff ratio 1:10", fontsize=16)
    plt.legend(fontsize=10)
    plt.grid(False)
    cumulative_path = output_dir / "comparative_cumulative_resistant_cases.png"
    plt.savefig(cumulative_path, dpi=300, bbox_inches="tight")
    print(f"Saved plot to {cumulative_path}")
    plt.show()
    plt.close()


if __name__ == "__main__":
    main()
