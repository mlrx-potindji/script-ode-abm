####

class Patient(Agent):
    def __init__(self, model, ward_id=None):
        super().__init__(model)
        self.ward_id = ward_id
        self.assign_initial_state()
        self.newly_infected = False
        self.prev_state = self.state
        self.days_in_admission = 0

    def assign_initial_state(self):
        r = random.random()
        params = self.model.params
        if r < params["lambda_cr"]:
            self.state = "Cp_r"
        elif r < params["lambda_cr"] + params["lambda_cs"]:
            self.state = "Cp_s"
        elif r < params["lambda_cr"] + params["lambda_cs"] + params["lambda_ir"]:
            self.state = "Ip_r"
        elif r < params["lambda_cr"] + params["lambda_cs"] + params["lambda_ir"] + params["lambda_is"]:
            self.state = "Ip_s"
        else:
            self.state = "S"

    def step(self):
        params = self.model.params
        self.newly_infected = False
        self.prev_state = self.state

        if self.state == "R": return

        if self.state == "Cp_s":
            if random.random() < params["kappa"] * params["m_s"]:
                self.state = "Ip_s"
        elif self.state == "Cp_r":
            if random.random() < params["kappa"] * params["m_s"] * (1 + params["delta_m"]):
                self.state = "Ip_r"

        if self.state == "Ip_s":
            if random.random() < params["psi_s"] * params["iota"]:
                self.state = "R"
        elif self.state == "Ip_r":
            if random.random() < params["psi_r"] * params["iota"] * (1 - params["s_p"]):
                self.state = "R"

        if self.state == "Cp_s" and random.random() < params["mu_c"]:
            self.state = "R"
        elif self.state == "Cp_r" and random.random() < params["mu_c"]:
            self.state = "R"
        elif self.state == "Ip_s" and random.random() < params["psi_s"] * (1 - params["iota"]):
            self.state = "R"
        elif self.state == "Ip_r" and random.random() < params["psi_r"] * (1 - (params["iota"] * (1 - params["s_p"]))):
            self.state = "R"

        if self.state == "Ip_s" and random.random() < params["prob_resistance_emergence"]:
            self.state = "Ip_r"

####

class Nurse(Agent):
    def __init__(self, model, ward_id=None):
        super().__init__(model)
        self.ward_id = ward_id
        self.state = "U"
        self.has_contacted = False
        self.interactions_this_step = 0
        self.compliance = 1.0

    def step(self):
        params = self.model.params
        if self.state in ["Cn_s", "Cn_r"]:
            base_delta = (1 - params["theta"]) * params["low"] + params["theta"] * params["high"]
            if random.random() < base_delta * self.compliance:
                self.state = "U"
                self.has_contacted = False
        self.interactions_this_step = 0

    def update_compliance(self, workload_factor):
        params = self.model.params
        self.compliance = max(0.0, 1.0 - workload_factor * params["compliance_decrease_rate"])
