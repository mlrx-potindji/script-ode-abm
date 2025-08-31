####

class NoWardModel(Model):
    def __init__(self, num_nurses=150, **params):
        super().__init__()
        self.params = {**PARAMS, **params}

        ratio = self.params.get("target_patient_to_nurse_ratio")
        if ratio and ratio > 0:
            self.num_nurses = math.ceil(self.params["initial_patients"] / ratio)
        else:
            self.num_nurses = num_nurses

        self.schedule = self.agents
        self.running = True

        for i in range(self.params["initial_patients"]):
            p = Patient(self)
            self.schedule.add(p)

        for i in range(self.num_nurses):
            n = Nurse(self)
            self.schedule.add(n)

        self.datacollector = DataCollector(
            agent_reporters={"State": "state"},
            model_reporters={
                "Current_Patients": lambda m: sum(1 for a in m.schedule if isinstance(a, Patient) and a.state != "R"),
                "Susceptible": lambda m: sum(1 for a in m.schedule if isinstance(a, Patient) and a.state == "S"),
                "Colonized_S": lambda m: sum(1 for a in m.schedule if isinstance(a, Patient) and a.state == "Cp_s"),
                "Infected_S": lambda m: sum(1 for a in m.schedule if isinstance(a, Patient) and a.state == "Ip_s"),
                "Colonized_R": lambda m: sum(1 for a in m.schedule if isinstance(a, Patient) and a.state == "Cp_r"),
                "Infected_R": lambda m: sum(1 for a in m.schedule if isinstance(a, Patient) and a.state == "Ip_r"),
                "New_Colonized_S": lambda m: sum(1 for a in m.schedule if isinstance(a, Patient) and a.newly_infected and a.state == "Cp_s"),
                "New_Infected_S": lambda m: sum(1 for a in m.schedule if isinstance(a, Patient) and a.newly_infected and a.state == "Ip_s"),
                "New_Colonized_R": lambda m: sum(1 for a in m.schedule if isinstance(a, Patient) and a.newly_infected and a.state == "Cp_r"),
                "New_Infected_R": lambda m: sum(1 for a in m.schedule if isinstance(a, Patient) and a.newly_infected and a.state == "Ip_r"),
                "Resistance_Emergence": lambda m: sum(1 for a in m.schedule if isinstance(a, Patient) and a.state == "Ip_r" and a.prev_state == "Ip_s"),
                "Average_Nurse_Workload_Factor": lambda m: m.calculate_workload_factor(),
            }
        )

    def add_new_patient(self):
        """Adds a new patient to the model."""
        p = Patient(self)
        self.schedule.add(p)

    def calculate_workload_factor(self):
        patients_in_hospital = sum(1 for a in self.schedule if isinstance(a, Patient) and a.state != "R")
        nurses_on_duty = sum(1 for a in self.schedule if isinstance(a, Nurse))
        return patients_in_hospital / nurses_on_duty if nurses_on_duty > 0 else 0.0

    def handle_granular_interactions(self):
        patients = [a for a in self.schedule if isinstance(a, Patient) and a.state != "R"]
        nurses = [a for a in self.schedule if isinstance(a, Nurse)]
        params = self.params
        if not patients or not nurses: return

        workload_factor = self.calculate_workload_factor()
        for nurse in nurses: nurse.update_compliance(workload_factor)

        random.shuffle(patients)
        for nurse in nurses:
            num_interactions = min(params["nurse_max_interactions"], len(patients))
            if num_interactions <= 0: continue
            patients_to_interact = random.sample(patients, num_interactions)

            for patient in patients_to_interact:
                base_prob_pn = params["a"] * params["b_pn"] * (1 - params["theta"])
                if nurse.state == "U":
                    if patient.state in ["Cp_s", "Ip_s"] and random.random() < base_prob_pn * (params["n"] if patient.state == "Cp_s" else 1.0) * nurse.compliance:
                        nurse.state = "Cn_s"
                    elif patient.state in ["Cp_r", "Ip_r"] and random.random() < base_prob_pn * (1 - params["s_b"]) * (params["n"] if patient.state == "Cp_r" else 1.0) * nurse.compliance:
                        nurse.state = "Cn_r"

                base_prob_np = params["a"] * params["b_np"] * (1 - params["theta"])
                if patient.state == "S":
                    if nurse.state == "Cn_s" and random.random() < base_prob_np:
                        patient.state = "Ip_s" if random.random() < params["x"] else "Cp_s"
                        patient.newly_infected = True
                    elif nurse.state == "Cn_r" and random.random() < base_prob_np * (1 - params["s_b"]):
                        patient.state = "Ip_r" if random.random() < params["x"] else "Cp_r"
                        patient.newly_infected = True

    def step(self):
        for agent in self.schedule: agent.prev_state = agent.state
        self.schedule.shuffle_do("step")
        removed_this_step = sum(1 for a in self.schedule if isinstance(a, Patient) and a.state == "R" and a.prev_state != "R")
        current_patients = sum(1 for a in self.schedule if isinstance(a, Patient) and a.state != "R")
        num_to_admit = min(self.params["admission_rate_per_step"], self.params["max_patient_capacity"] - current_patients) if current_patients < self.params["max_patient_capacity"] else removed_this_step
        for _ in range(num_to_admit):
            self.add_new_patient()
        self.handle_granular_interactions()
        self.datacollector.collect(self)

####

class WardModel(NoWardModel):
    def __init__(self, num_nurses=150, num_wards=10, **params):
        self.num_wards = num_wards
        super().__init__(num_nurses=num_nurses, **params)
        patients = [a for a in self.schedule if isinstance(a, Patient)]
        nurses = [a for a in self.schedule if isinstance(a, Nurse)]
        for i, p in enumerate(patients): p.ward_id = i % self.num_wards
        nurses_per_ward = self.num_nurses // self.num_wards
        for i, n in enumerate(nurses): n.ward_id = i // nurses_per_ward if nurses_per_ward > 0 else i % self.num_wards

    def add_new_patient(self):
        p = Patient(self, ward_id=random.randrange(self.num_wards))
        self.schedule.add(p)

    def _calculate_ward_workload_factor(self, ward_id):
        patients_in_ward = sum(1 for a in self.schedule if isinstance(a, Patient) and a.state != "R" and a.ward_id == ward_id)
        nurses_in_ward = sum(1 for a in self.schedule if isinstance(a, Nurse) and a.ward_id == ward_id)
        return patients_in_ward / nurses_in_ward if nurses_in_ward > 0 else 0.0

    def calculate_workload_factor(self):
        workload_factors = [self._calculate_ward_workload_factor(i) for i in range(self.num_wards)]
        return np.mean(workload_factors) if workload_factors else 0.0

    def handle_granular_interactions(self):
        params = self.params
        for ward_id in range(self.num_wards):
            patients_in_ward = [a for a in self.schedule if isinstance(a, Patient) and a.state != "R" and a.ward_id == ward_id]
            nurses_in_ward = [a for a in self.schedule if isinstance(a, Nurse) and a.ward_id == ward_id]
            if not patients_in_ward or not nurses_in_ward: continue

            workload_factor = self._calculate_ward_workload_factor(ward_id)
            for nurse in nurses_in_ward: nurse.update_compliance(workload_factor)

            random.shuffle(patients_in_ward)
            for nurse in nurses_in_ward:
                num_interactions = min(params["nurse_max_interactions"], len(patients_in_ward))
                if num_interactions <= 0: continue
                patients_to_interact = random.sample(patients_in_ward, num_interactions)

                for patient in patients_to_interact:
                    base_prob_pn = params["a"] * params["b_pn"] * (1 - params["theta"])
                    if nurse.state == "U":
                        if patient.state in ["Cp_s", "Ip_s"] and random.random() < base_prob_pn * (params["n"] if patient.state == "Cp_s" else 1.0) * nurse.compliance:
                            nurse.state = "Cn_s"
                        elif patient.state in ["Cp_r", "Ip_r"] and random.random() < base_prob_pn * (1 - params["s_b"]) * (params["n"] if patient.state == "Cp_r" else 1.0) * nurse.compliance:
                            nurse.state = "Cn_r"

                    base_prob_np = params["a"] * params["b_np"] * (1 - params["theta"])
                    if patient.state == "S":
                        if nurse.state == "Cn_s" and random.random() < base_prob_np:
                            patient.state = "Ip_s" if random.random() < params["x"] else "Cp_s"
                            patient.newly_infected = True
                        elif nurse.state == "Cn_r" and random.random() < base_prob_np * (1 - params["s_b"]):
                            patient.state = "Ip_r" if random.random() < params["x"] else "Cp_r"
                            patient.newly_infected = True

####

class PatientAssignmentModel(WardModel):
    def __init__(self, num_nurses=150, num_wards=10, **params):
        super().__init__(num_nurses=num_nurses, num_wards=num_wards, **params)
        self.patient_assignments = {n.unique_id: [] for n in self.schedule if isinstance(n, Nurse)}
        self._assign_initial_patients()

    def _assign_initial_patients(self):
        for ward_id in range(self.num_wards):
            patients_in_ward = [p for p in self.schedule if isinstance(p, Patient) and p.ward_id == ward_id]
            nurses_in_ward = [n for n in self.schedule if isinstance(n, Nurse) and n.ward_id == ward_id]
            if not nurses_in_ward: continue
            for i, p in enumerate(patients_in_ward):
                nurse = nurses_in_ward[i % len(nurses_in_ward)]
                self.patient_assignments[nurse.unique_id].append(p)

    def add_new_patient(self):
        super().add_new_patient()
        new_patient = self.schedule[-1]
        nurses_in_ward = [n for n in self.schedule if isinstance(n, Nurse) and n.ward_id == new_patient.ward_id]
        if not nurses_in_ward: return
        nurse_assignments = {nid: len(p_list) for nid, p_list in self.patient_assignments.items() if nid in [n.unique_id for n in nurses_in_ward]}
        least_burdened_id = min(nurse_assignments, key=nurse_assignments.get) if nurse_assignments else random.choice(nurses_in_ward).unique_id
        self.patient_assignments[least_burdened_id].append(new_patient)

    def step(self):
        super().step()
        for nid in self.patient_assignments: self.patient_assignments[nid] = [p for p in self.patient_assignments[nid] if p.state != 'R']

    def handle_granular_interactions(self):
        params = self.params
        for ward_id in range(self.num_wards):
            nurses_in_ward = [n for n in self.schedule if isinstance(n, Nurse) and n.ward_id == ward_id]
            if not nurses_in_ward: continue
            workload_factor = self._calculate_ward_workload_factor(ward_id)
            for nurse in nurses_in_ward:
                nurse.update_compliance(workload_factor)
                assigned_patients = [p for p in self.patient_assignments.get(nurse.unique_id, []) if p.state != 'R']
                for patient in assigned_patients:
                    base_prob_pn = params["a"] * params["b_pn"] * (1 - params["theta"])
                    if nurse.state == "U":
                        if patient.state in ["Cp_s", "Ip_s"] and random.random() < base_prob_pn * (params["n"] if patient.state == "Cp_s" else 1.0) * nurse.compliance:
                            nurse.state = "Cn_s"
                        elif patient.state in ["Cp_r", "Ip_r"] and random.random() < base_prob_pn * (1 - params["s_b"]) * (params["n"] if patient.state == "Cp_r" else 1.0) * nurse.compliance:
                            nurse.state = "Cn_r"

                    base_prob_np = params["a"] * params["b_np"] * (1 - params["theta"])
                    if patient.state == "S":
                        if nurse.state == "Cn_s" and random.random() < base_prob_np:
                            patient.state = "Ip_s" if random.random() < params["x"] else "Cp_s"
                            patient.newly_infected = True
                        elif nurse.state == "Cn_r" and random.random() < base_prob_np * (1 - params["s_b"]):
                            patient.state = "Ip_r" if random.random() < params["x"] else "Cp_r"
                            patient.newly_infected = True

####

class AdmissionWardModel(WardModel):
    def __init__(self, num_nurses=150, num_wards=10, **params):
        super().__init__(num_nurses=num_nurses, num_wards=num_wards, **params)
        self.general_wards = [i for i in range(num_wards) if i not in [self.params["admission_ward_id"], self.params["resistant_cohort_ward_id"]]]

    def add_new_patient(self):
        p = Patient(self, ward_id=self.params["admission_ward_id"])
        self.schedule.add(p)

    def triage_patient(self, patient):
        if patient.state in ["Cp_r", "Ip_r"]:
            patient.ward_id = self.params["resistant_cohort_ward_id"]
        else:
            patient.ward_id = random.choice(self.general_wards) if self.general_wards else self.params["resistant_cohort_ward_id"]

    def step(self):
        patients_in_admission = [p for p in self.schedule if isinstance(p, Patient) and p.ward_id == self.params["admission_ward_id"]]
        for p in patients_in_admission:
            if p.days_in_admission >= self.params["admission_period"]:
                self.triage_patient(p)
        super().step()
        for p in self.schedule:
            if isinstance(p, Patient) and p.ward_id == self.params["admission_ward_id"]:
                p.days_in_admission += 1

    def handle_granular_interactions(self):
        params = self.params
        for ward_id in range(self.num_wards):
            patients_in_ward = [a for a in self.schedule if isinstance(a, Patient) and a.state != "R" and a.ward_id == ward_id]
            nurses_in_ward = [a for a in self.schedule if isinstance(a, Nurse) and a.ward_id == ward_id]
            if not patients_in_ward or not nurses_in_ward: continue

            workload_factor = self._calculate_ward_workload_factor(ward_id)
            is_high_risk = (ward_id == params["admission_ward_id"] or ward_id == params["resistant_cohort_ward_id"])

            for nurse in nurses_in_ward:
                nurse.update_compliance(workload_factor)
                if is_high_risk:
                    nurse.compliance += (1 - nurse.compliance) * params["compliance_boost"]

            random.shuffle(patients_in_ward)
            for nurse in nurses_in_ward:
                num_interactions = min(params["nurse_max_interactions"], len(patients_in_ward))
                if num_interactions <= 0: continue
                patients_to_interact = random.sample(patients_in_ward, num_interactions)

                for patient in patients_to_interact:
                    base_prob_pn = params["a"] * params["b_pn"] * (1 - params["theta"])
                    if nurse.state == "U":
                        if patient.state in ["Cp_s", "Ip_s"] and random.random() < base_prob_pn * (params["n"] if patient.state == "Cp_s" else 1.0) * nurse.compliance:
                            nurse.state = "Cn_s"
                        elif patient.state in ["Cp_r", "Ip_r"] and random.random() < base_prob_pn * (1 - params["s_b"]) * (params["n"] if patient.state == "Cp_r" else 1.0) * nurse.compliance:
                            nurse.state = "Cn_r"

                    base_prob_np = params["a"] * params["b_np"] * (1 - params["theta"])
                    if patient.state == "S":
                        if nurse.state == "Cn_s" and random.random() < base_prob_np:
                            patient.state = "Ip_s" if random.random() < params["x"] else "Cp_s"
                            patient.newly_infected = True
                        elif nurse.state == "Cn_r" and random.random() < base_prob_np * (1 - params["s_b"]):
                            patient.state = "Ip_r" if random.random() < params["x"] else "Cp_r"
                            patient.newly_infected = True

####

class AdmissionPatientAssignmentModel(PatientAssignmentModel):
    def __init__(self, num_nurses=150, num_wards=10, **params):
        super().__init__(num_nurses=num_nurses, num_wards=num_wards, **params)
        self.general_wards = [i for i in range(num_wards) if i not in [self.params["admission_ward_id"], self.params["resistant_cohort_ward_id"]]]

    def add_new_patient(self):
        p = Patient(self, ward_id=self.params["admission_ward_id"])
        self.schedule.add(p)
        nurses_in_admission = [n for n in self.schedule if isinstance(n, Nurse) and n.ward_id == self.params["admission_ward_id"]]
        if not nurses_in_admission: return
        nurse_assignments = {nid: len(p_list) for nid, p_list in self.patient_assignments.items() if nid in [n.unique_id for n in nurses_in_admission]}
        least_burdened_id = min(nurse_assignments, key=nurse_assignments.get) if nurse_assignments else random.choice(nurses_in_admission).unique_id
        self.patient_assignments[least_burdened_id].append(p)

    def triage_patient(self, patient):
        current_nurse_id = next((nid for nid, p_list in self.patient_assignments.items() if patient in p_list), None)
        if current_nurse_id: self.patient_assignments[current_nurse_id].remove(patient)

        new_ward_id = self.params["resistant_cohort_ward_id"] if patient.state in ["Cp_r", "Ip_r"] else (random.choice(self.general_wards) if self.general_wards else self.params["resistant_cohort_ward_id"])
        patient.ward_id = new_ward_id

        nurses_in_new_ward = [n for n in self.schedule if isinstance(n, Nurse) and n.ward_id == new_ward_id]
        if not nurses_in_new_ward: return
        nurse_assignments = {nid: len(p_list) for nid, p_list in self.patient_assignments.items() if nid in [n.unique_id for n in nurses_in_new_ward]}
        least_burdened_id = min(nurse_assignments, key=nurse_assignments.get) if nurse_assignments else random.choice(nurses_in_new_ward).unique_id
        self.patient_assignments[least_burdened_id].append(patient)

    def step(self):
        patients_in_admission = [p for p in self.schedule if isinstance(p, Patient) and p.ward_id == self.params["admission_ward_id"]]
        for p in patients_in_admission:
            if p.days_in_admission >= self.params["admission_period"]:
                self.triage_patient(p)
        super().step()
        for p in self.schedule:
            if isinstance(p, Patient) and p.ward_id == self.params["admission_ward_id"]:
                p.days_in_admission += 1

    def handle_granular_interactions(self):
        params = self.params
        for ward_id in range(self.num_wards):
            nurses_in_ward = [n for n in self.schedule if isinstance(n, Nurse) and n.ward_id == ward_id]
            if not nurses_in_ward: continue
            workload_factor = self._calculate_ward_workload_factor(ward_id)
            is_high_risk = (ward_id == params["admission_ward_id"] or ward_id == params["resistant_cohort_ward_id"])
            for nurse in nurses_in_ward:
                nurse.update_compliance(workload_factor)
                if is_high_risk:
                    nurse.compliance += (1 - nurse.compliance) * params["compliance_boost"]
                assigned_patients = [p for p in self.patient_assignments.get(nurse.unique_id, []) if p.state != 'R']
                for patient in assigned_patients:
                    base_prob_pn = params["a"] * params["b_pn"] * (1 - params["theta"])
                    if nurse.state == "U":
                        if patient.state in ["Cp_s", "Ip_s"] and random.random() < base_prob_pn * (params["n"] if patient.state == "Cp_s" else 1.0) * nurse.compliance:
                            nurse.state = "Cn_s"
                        elif patient.state in ["Cp_r", "Ip_r"] and random.random() < base_prob_pn * (1 - params["s_b"]) * (params["n"] if patient.state == "Cp_r" else 1.0) * nurse.compliance:
                            nurse.state = "Cn_r"

                    base_prob_np = params["a"] * params["b_np"] * (1 - params["theta"])
                    if patient.state == "S":
                        if nurse.state == "Cn_s" and random.random() < base_prob_np:
                            patient.state = "Ip_s" if random.random() < params["x"] else "Cp_s"
                            patient.newly_infected = True
                        elif nurse.state == "Cn_r" and random.random() < base_prob_np * (1 - params["s_b"]):
                            patient.state = "Ip_r" if random.random() < params["x"] else "Cp_r"
                            patient.newly_infected = True
