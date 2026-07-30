#===============================================================================

# Clear workspace and set options for reproducible research
rm(list = ls())
options(scipen = 999)  # Avoid scientific notation for clarity
options(digits = 4)    # Reasonable precision for display

#-------------------------------------------------------------------------------
# SECTION 1: PACKAGE MANAGEMENT WITH ERROR HANDLING
#-------------------------------------------------------------------------------

# Essential packages we absolutely need
# Using explicit error handling to identify package issues
essential_packages <- c("tidyverse", "haven", "stargazer", "MASS", "GGally")

for(pkg in essential_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

# Handle specialized packages that might fail
specialized_packages <- list(
  randomizr = "For advanced randomization",
  RCT = "For balance testing", 
  lfe = "For fixed effects with clustering",
  fixest = "Alternative for fixed effects",
  rdrobust = "For RDD analysis"
)

for(pkg in names(specialized_packages)) {
  tryCatch({
    if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, quiet = TRUE)
      library(pkg, character.only = TRUE, quietly = TRUE)
    }
    cat(sprintf("✓ %s loaded: %s\n", pkg, specialized_packages[[pkg]]))
  }, error = function(e) {
    cat(sprintf("✗ %s unavailable: %s\n", pkg, specialized_packages[[pkg]]))
    cat("  Will use base R alternatives\n")
  })
}

#-------------------------------------------------------------------------------
# SECTION 2: THE FUNDAMENTAL PROBLEM OF CAUSAL INFERENCE
#-------------------------------------------------------------------------------

# We begin by demonstrating why randomization is necessary
# The key insight: selection bias corrupts naive comparisons

set.seed(1747)  # Your randomization seed for reproducibility
n <- 5000

# Create a world where we can see both potential outcomes (impossible in reality)
fundamental_problem_data <- dplyr::tibble(
  id = 1:n,
  
  # Unobserved characteristics that affect both treatment and outcome
  ability = rnorm(n, mean = 0, sd = 1),
  motivation = rnorm(n, mean = 0, sd = 1),
  
  # Potential outcomes under Neyman-Rubin framework
  # Y(0): outcome if NOT treated
  Y0 = 50 + 5*ability + 3*motivation + rnorm(n, 0, 10),
  
  # Y(1): outcome if treated  
  # True causal effect τ = 20 for everyone (constant treatment effect)
  Y1 = Y0 + 20,
  
  # Selection into treatment based on unobservables
  # Higher ability/motivation → higher probability of treatment
  prob_treatment = plogis(ability + 0.5*motivation),  # Logistic function
  D_observed = rbinom(n, 1, prob_treatment),
  
  # What we actually observe (switching equation)
  Y_observed = D_observed * Y1 + (1 - D_observed) * Y0
)

# Calculate and display selection bias
naive_estimate <- coef(lm(Y_observed ~ D_observed, data = fundamental_problem_data))["D_observed"]
true_effect <- 20  # We know this because we created the data
selection_bias <- naive_estimate - true_effect

cat("\n════════════════════════════════════════════════════════════\n")
cat("THE FUNDAMENTAL PROBLEM OF CAUSAL INFERENCE\n")
cat("════════════════════════════════════════════════════════════\n")
cat(sprintf("True causal effect (τ):        %6.2f\n", true_effect))
cat(sprintf("Naive observational estimate:  %6.2f\n", naive_estimate))
cat(sprintf("Selection bias:                %6.2f\n", selection_bias))
cat("\nWhy? E[Y(0)|D=1] ≠ E[Y(0)|D=0] due to selection on unobservables\n")

# Visualize the selection problem
selection_plot <- ggplot(fundamental_problem_data, 
                         aes(x = ability, y = Y_observed, color = factor(D_observed))) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("0" = "#E41A1C", "1" = "#377EB8"),
                     labels = c("Control", "Treated")) +
  labs(title = "Selection Bias: Why Randomization Matters",
       subtitle = sprintf("Bias = %.2f (Naive estimate %.2f - True effect %.2f)", 
                          selection_bias, naive_estimate, true_effect),
       x = "Unobserved Ability (drives both treatment and outcome)",
       y = "Observed Outcome",
       color = "Treatment Status") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(selection_plot)

#-------------------------------------------------------------------------------
# SECTION 3: DATA GENERATION FOLLOWING código_taller_1.Rmd EXACTLY
#-------------------------------------------------------------------------------

# Generate correlated covariates as in INFONAVIT example
set.seed(806)  # Sampling seed - different from randomization seed!

n_population <- 50000

# Create correlation structure for continuous variables
# This matches real-world data where age, salary, and savings are correlated
correlation_matrix <- matrix(c(
  100,     50,        200,      # age variance and covariances
  50,      225000000, 500000,   # salary variance and covariances  
  200,     500000,    2500000000 # savings variance and covariances
), nrow = 3, byrow = TRUE)

# Generate correlated continuous variables
continuous_vars <- MASS::mvrnorm(
  n = n_population,
  mu = c(35, 15000, 50000),  # means: age=35, salary=15k, savings=50k
  Sigma = correlation_matrix
)

# Build complete population dataset
SIM_DATA <- dplyr::tibble(
  ID = 1:n_population,
  edad = round(pmax(18, pmin(65, continuous_vars[,1]))),  # Bounded age
  salario_cotizacion = pmax(0, continuous_vars[,2]),      # Non-negative salary
  saldo_ssv = pmax(0, continuous_vars[,3]),               # Non-negative savings
  codigo_postal = sample(1000:9999, n_population, replace = TRUE),
  estado_civil = sample(c("Casado", "Soltero"), n_population, 
                        replace = TRUE, prob = c(0.6, 0.4)),
  reus = sample(c("REUS", "Otro"), n_population, 
                replace = TRUE, prob = c(0.02, 0.98)),
  contactabilidad = sample(c("Contactable", "No Contactable"), n_population,
                           replace = TRUE, prob = c(0.7, 0.3)),
  densidad_cotizacion = runif(n_population, 0, 1)
)

# Display correlation structure (pedagogical value)
cor_check <- cor(SIM_DATA[, c("edad", "salario_cotizacion", "saldo_ssv")])
cat("\n════════════════════════════════════════════════════════════\n")
cat("CORRELATION STRUCTURE IN POPULATION DATA\n")
cat("════════════════════════════════════════════════════════════\n")
print(round(cor_check, 3))

#-------------------------------------------------------------------------------
# SECTION 4: SAMPLE SELECTION AND ELIGIBILITY CRITERIA
#-------------------------------------------------------------------------------

# Apply eligibility filters (important for external validity discussion)
df_eligible <- SIM_DATA %>%
  dplyr::filter(reus == "Otro") %>%           # Regulatory constraint
  dplyr::filter(contactabilidad == "Contactable")  # Operational constraint

cat("\n════════════════════════════════════════════════════════════\n")
cat("SAMPLE SELECTION FUNNEL\n")
cat("════════════════════════════════════════════════════════════\n")
cat(sprintf("Initial population:     %6d\n", nrow(SIM_DATA)))
cat(sprintf("After REUS filter:      %6d (%.1f%% excluded)\n", 
            sum(SIM_DATA$reus == "Otro"),
            100 * sum(SIM_DATA$reus == "REUS")/nrow(SIM_DATA)))
cat(sprintf("After contact filter:   %6d (%.1f%% of eligible)\n", 
            nrow(df_eligible),
            100 * nrow(df_eligible)/sum(SIM_DATA$reus == "Otro")))

# Sample size determination (from power calculation)
n_min <- 3000       # Minimum from power analysis
n_budget <- 3000    # Additional budget constraint
n_sample <- n_min + n_budget

# Draw experimental sample
set.seed(806)  # Same seed ensures reproducibility
df_experiment <- df_eligible %>%
  dplyr::sample_n(n_sample, replace = FALSE)

cat(sprintf("Final sample:           %6d\n", n_sample))
cat("\nExternal validity note: Results apply to contactable, non-REUS population\n")

#-------------------------------------------------------------------------------
# SECTION 5: SIMPLE RANDOMIZATION WITH COMPLETE BALANCE TESTING
#-------------------------------------------------------------------------------

cat("\n════════════════════════════════════════════════════════════\n")
cat("SIMPLE RANDOMIZATION\n")
cat("════════════════════════════════════════════════════════════\n")

# Implement complete randomization
set.seed(1747)  # Randomization seed - DIFFERENT from sampling seed

if(exists("complete_ra", where = "package:randomizr", mode = "function")) {
  # Use randomizr if available
  treatment <- randomizr::complete_ra(
    N = nrow(df_experiment),
    m = nrow(df_experiment)/2  # Exactly 50% treated
  )
} else {
  # Base R alternative
  treatment <- sample(c(rep(1, n_sample/2), rep(0, n_sample/2)))
}

df_experiment$treatment <- treatment

# Verify exact 50-50 split
treatment_table <- table(df_experiment$treatment)
cat(sprintf("Control: %d | Treated: %d | Ratio: %.3f\n", 
            treatment_table[1], treatment_table[2], 
            treatment_table[2]/treatment_table[1]))

#-------------------------------------------------------------------------------
# SECTION 6: BALANCE TESTING - CRITICAL FOR VALIDITY
#-------------------------------------------------------------------------------

# Prepare data for balance tests (all variables must be numeric)
balance_vars <- df_experiment %>%
  dplyr::select(edad, salario_cotizacion, saldo_ssv, densidad_cotizacion, 
                estado_civil, treatment) %>%
  dplyr::mutate(
    estado_civil = ifelse(estado_civil == "Casado", 1, 0),
    treatment = as.character(treatment)  # RCT package requires character
  )

# Method 1: Individual t-tests (simple but multiple testing issue)
balance_tests <- list()
numeric_vars <- c("edad", "salario_cotizacion", "saldo_ssv", 
                  "densidad_cotizacion", "estado_civil")

cat("\nINDIVIDUAL BALANCE TESTS\n")
cat("Variable            | Diff    | SE      | t-stat  | p-value\n")
cat("────────────────────────────────────────────────────────────\n")

for(var in numeric_vars) {
  treated <- balance_vars[[var]][balance_vars$treatment == "1"]
  control <- balance_vars[[var]][balance_vars$treatment == "0"]
  test <- t.test(treated, control)
  
  diff <- mean(treated) - mean(control)
  se <- sqrt(var(treated)/length(treated) + var(control)/length(control))
  
  cat(sprintf("%-18s | %7.2f | %7.2f | %7.2f | %.3f%s\n", 
              var, diff, se, test$statistic, test$p.value,
              ifelse(test$p.value < 0.05, " *", "")))
  
  balance_tests[[var]] <- test
}

# Method 2: Joint F-test (if RCT package available)
balance_regression <- RCT::balance_regression(
    data = balance_vars,
    treatment = "treatment")

# Multiple testing consideration
cat("\n⚠ MULTIPLE TESTING NOTE:\n")
cat(sprintf("With %d tests at α=0.05, expect %.1f false positives by chance\n", 
            length(numeric_vars), length(numeric_vars)*0.05))
cat("Consider Bonferroni correction: α/k = 0.05/5 = 0.01 per test\n")

#-------------------------------------------------------------------------------
# SECTION 7: STRATIFIED RANDOMIZATION FOR IMPROVED PRECISION
#-------------------------------------------------------------------------------

cat("\n════════════════════════════════════════════════════════════\n")
cat("STRATIFIED RANDOMIZATION\n")
cat("════════════════════════════════════════════════════════════\n")

# Create stratification variables
if("RCT" %in% (.packages())) {
  df_experiment_strat <- df_experiment %>%
    dplyr::mutate(
      strata_edad = RCT::ntile_label(edad, 3),
      strata_densidad = RCT::ntile_label(densidad_cotizacion * 100, 3),
      strata_ingreso = RCT::ntile_label(salario_cotizacion, 3)
    )
  
  # Perform stratified randomization
  stratified_assignment <- RCT::treatment_assign(
    df_experiment_strat,
    n_t = 1,
    share_control = 0.5,
    strata_varlist = dplyr::vars(strata_edad, strata_ingreso, strata_densidad),
    key = "ID",
    missfits = "global",
    seed = 1747
  )
  
  # Analyze stratification quality
  n_strata <- length(unique(interaction(df_experiment_strat$strata_edad,
                                        df_experiment_strat$strata_ingreso,
                                        df_experiment_strat$strata_densidad)))
  
  misfit_pct <- 100 * sum(stratified_assignment$data$missfit) / nrow(df_experiment_strat)
  
  cat(sprintf("Number of strata: %d (3³ from three trichotomized variables)\n", n_strata))
  cat(sprintf("Average observations per stratum: %.1f\n", nrow(df_experiment_strat)/n_strata))
  cat(sprintf("Misfit percentage: %.2f%% (target < 3%%)\n", misfit_pct))
  
  if(misfit_pct < 3) {
    cat("✓ Stratification successful - low misfit rate\n")
  } else {
    cat("⚠ High misfit rate - consider fewer strata\n")
  }
}

#-------------------------------------------------------------------------------
# SECTION 8: CLUSTER RANDOMIZATION
#-------------------------------------------------------------------------------

cat("\n════════════════════════════════════════════════════════════\n")
cat("CLUSTER RANDOMIZATION\n")
cat("════════════════════════════════════════════════════════════\n")

# Prepare cluster-level data
df_cluster <- SIM_DATA %>%
  dplyr::mutate(
    CP = codigo_postal,
    estado_civil_num = ifelse(estado_civil == "Casado", 1, 0)
  ) %>%
  dplyr::group_by(CP) %>%
  dplyr::mutate(cluster_size = n())

# Sample 200 clusters
set.seed(806)
sampled_clusters <- unique(df_cluster$CP) %>%
  sample(200)

df_cluster_exp <- df_cluster %>%
  dplyr::filter(CP %in% sampled_clusters)

# Randomize at cluster level
set.seed(1747)

if(exists("cluster_ra", where = "package:randomizr")) {
  cluster_treatment <- randomizr::cluster_ra(
    clusters = df_cluster_exp$CP,
    m = 100  # Treat 100 clusters
  )
} else {
  # Base R alternative
  treated_clusters <- sample(unique(df_cluster_exp$CP), 100)
  cluster_treatment <- ifelse(df_cluster_exp$CP %in% treated_clusters, 1, 0)
}

df_cluster_exp$treatment <- cluster_treatment

# Verify cluster-level randomization
cluster_check <- df_cluster_exp %>%
  dplyr::group_by(CP) %>%
  dplyr::summarise(
    treat = mean(treatment),
    size = n()
  )

cat(sprintf("Clusters treated: %d\n", sum(cluster_check$treat == 1)))
cat(sprintf("Clusters control: %d\n", sum(cluster_check$treat == 0)))
cat(sprintf("Total units in treated clusters: %d\n", 
            sum(cluster_check$size[cluster_check$treat == 1])))
cat(sprintf("Total units in control clusters: %d\n", 
            sum(cluster_check$size[cluster_check$treat == 0])))

# Important: Balance should be checked at cluster level!
cluster_balance <- cluster_check %>%
  dplyr::left_join(
    df_cluster_exp %>%
      dplyr::group_by(CP) %>%
      dplyr::summarise(
        mean_age = mean(edad),
        mean_salary = mean(salario_cotizacion)
      ),
    by = "CP"
  )

cluster_balance_test <- t.test(
  mean_age ~ treat, 
  data = cluster_balance
)

cat(sprintf("\nCluster-level balance check (age): p = %.3f\n", 
            cluster_balance_test$p.value))
