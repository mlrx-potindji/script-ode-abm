####

# --- Plot 1 ---

plt.figure(figsize=(12, 7))
for name, df_mean in all_means.items():
    # --- Calculate the raw proportion ---
    # Replace 0s in the denominator with NaN to avoid division errors, then fill resulting NaNs
    denominator = df_mean['Susceptible'].replace(0, np.nan)
    proportion = (df_mean['New_Colonized_R'] + df_mean['New_Infected_R']) / denominator
    proportion = proportion.fillna(0)

    # --- Apply LOESS smoothing ---
    smoothed = sm.nonparametric.lowess(proportion, df_mean.index, frac=0.022)

    # --- Plot the smoothed curve ---
    plt.plot(smoothed[:, 0], smoothed[:, 1], label=name, linewidth=2)

plt.xlabel("Time Steps", fontsize=16)
plt.ylabel("Proportion of new resistant cases, staff ratio 1:10", fontsize=16)
#plt.title("Incidence Rate of New Resistant Cases (LOESS Smoothed)", fontsize=18)
plt.legend(fontsize = 10)
plt.grid(False)
# plt.savefig("comparative_proportion_new_cases_smoothed.png")
plt.show()
# plt.close()

####

# --- Plot 2 ---

plt.figure(figsize=(12, 7))
for name, df_mean in all_means.items():
    # --- Calculate cumulative resistant cases ---
    cumulative_resistant = (df_mean['New_Colonized_R'] + df_mean['New_Infected_R']).cumsum()

    # --- Plot curve ---
    plt.plot(df_mean.index, cumulative_resistant, label=name, linewidth=2)

plt.xlabel("Time Steps", fontsize=16)
plt.ylabel("Cumulative number of resistant cases, staff ratio 1:10", fontsize=16)
# plt.title("Cumulative Resistant Cases: Comparison of Models (Averaged)", fontsize=18)
plt.legend(fontsize=10)
plt.grid(False)
# plt.savefig("comparative_cumulative_resistant_cases.png")
plt.show()
# plt.close()
