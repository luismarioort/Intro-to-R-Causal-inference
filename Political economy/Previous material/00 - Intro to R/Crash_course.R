################################################################################
# R DATA MANIPULATION FOR ECONOMETRICS
# Focus: Data Frames, dplyr, and Joins
# Applied Econometrics Course
################################################################################
install.packages("dplyr")
# Required packages
library(dplyr)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Set working directory (adjust as needed)
setwd("~/Library/CloudStorage/Dropbox/Applied Research I - ECO-40211/class/00 - Intro to R")

################################################################################
# PART 1: QUICK REVIEW - BASIC DATA TYPES
################################################################################

# 1.1 BASIC DATA TYPES
my_double <- 3.14
my_integer <- 42L
my_logical <- TRUE
my_character <- "Econometrics"

# 1.2 VECTORS - Foundation of R
numeric_vector <- c(1.2, 3.4, 5.6, 7.8)
character_vector <- c("Q1", "Q2", "Q3", "Q4")

my_list <- list(
  name = "Alice",
  age = 30,
  is_student = TRUE,
  grades = c(85, 92, 78),
  address = list(street = "Main St", city = "Anytown")
)


# Type coercion - R converts to most flexible type
mixed <- c(TRUE, 2, "3")  # All become character
typeof(mixed)

# Named vectors
gdp_growth <- c(USA = 2.3, UK = 1.8, Japan = 0.9, Germany = 1.5)
gdp_growth["USA"]

numeric_vector[1]

################################################################################
# PART 2: DATA FRAMES - THE ECONOMETRIC WORKHORSE
################################################################################

# 2.1 Creating a data frame from scratch
firms_data <- data.frame(
  firm_id = 1:10,
  firm_name = paste0("Firm_", LETTERS[1:10]),
  revenue = c(45.2, 38.7, 52.1, 41.3, 48.9, 39.5, 44.6, 51.3, 42.8, 46.7),
  employees = c(120, 95, 150, 110, 135, 98, 115, 145, 108, 125),
  industry = c("Tech", "Finance", "Tech", "Retail", "Finance", 
               "Retail", "Tech", "Finance", "Retail", "Tech"),
  public_firm = c(TRUE, FALSE, TRUE, FALSE, TRUE, 
                  FALSE, TRUE, TRUE, FALSE, TRUE),
  founded_year = c(1995, 2001, 1998, 2005, 1999, 
                   2003, 2000, 1997, 2004, 2002),
  stringsAsFactors = FALSE
)

# Examine structure
str(firms_data)
head(firms_data)
dim(firms_data)
names(firms_data)

# 2.2 Accessing data frame elements
# By column
firms_data$revenue
firms_data[["employees"]]
firms_data[, "industry"]
firms_data[, 3]  # Third column

# By row
firms_data[1, ]  # First row
firms_data[c(1, 3, 5), ]  # Rows 1, 3, and 5

# Specific elements
firms_data[2, 3]  # Row 2, Column 3
firms_data[1:5, c("firm_name", "revenue")]

# 2.3 Basic subsetting with conditions
large_firms <- firms_data[firms_data$employees > 120, ]
tech_firms <- firms_data[firms_data$industry == "Tech", ]
recent_firms <- firms_data[firms_data$founded_year >= 2000, ]

# Multiple conditions
large_tech <- firms_data[firms_data$employees > 120 & firms_data$industry == "Tech", ]

# 2.4 Adding and modifying columns
firms_data$revenue_millions <- firms_data$revenue
firms_data$revenue_per_employee <- firms_data$revenue * 1000000 / firms_data$employees
firms_data$firm_age <- 2024 - firms_data$founded_year

# Conditional column creation
firms_data$size_category <- ifelse(firms_data$employees >= 130, "Large", 
                                   ifelse(firms_data$employees >= 110, "Medium", "Small"))

# 2.5 Summary statistics
summary(firms_data)
summary(firms_data[, c("revenue", "employees", "revenue_per_employee")])

################################################################################
# PART 3: CREATING PANEL DATA STRUCTURE
################################################################################

# Create panel data (firms over multiple years)
set.seed(123)
years <- 2019:2023
n_firms <- 5

# Expand grid creates all combinations
panel_base <- expand.grid(
  firm_id = 1:n_firms,
  year = years
)

# Add firm characteristics
panel_data <- panel_base %>%
  arrange(firm_id, year) %>%
  mutate(
    # Time-invariant characteristics (same for all years of a firm)
    industry = rep(c("Manufacturing", "Services", "Retail", "Tech", "Finance"), 
                   each = length(years)),
    region = rep(c("North", "South", "East", "West", "Central"), 
                 each = length(years)),
    
    # Time-varying variables
    revenue = 100 + firm_id * 10 + (year - 2019) * 5 + rnorm(n_firms * length(years), 0, 10),
    employees = 50 + firm_id * 8 + (year - 2019) * 2 + round(rnorm(n_firms * length(years), 0, 5)),
    investment = 10 + firm_id * 2 + (year - 2019) * 1 + rnorm(n_firms * length(years), 0, 3),
    exports = pmax(0, 20 + firm_id * 3 + rnorm(n_firms * length(years), 0, 5))
  )

# Make revenue and investment positive
panel_data$revenue <- abs(panel_data$revenue)
panel_data$investment <- abs(panel_data$investment)

head(panel_data, 10)

# Check panel structure
table(panel_data$firm_id)  # Should show 5 observations per firm
table(panel_data$year)     # Should show 5 observations per year

################################################################################
# PART 4: MODERN DATA MANIPULATION WITH dplyr
################################################################################

# The pipe operator %>% chains operations
result <- panel_data %>%
  filter(year == 2023) %>%
  select(firm_id, revenue, employees) %>%
  arrange(desc(revenue))

print(result)

################################################################################
# 4.1 FILTER - Selecting Rows
################################################################################

# Single condition
year_2022 <- filter(panel_data, year == 2022)

# Multiple conditions with AND
large_2023 <- panel_data |> filter(
                     year == 2023, 
                     employees > 70)

# OR conditions
tech_or_finance <- filter(panel_data, 
                          industry == "Tech" | industry == "Finance")

# Using %in% for multiple values
selected_industries <- filter(panel_data, 
                              industry %in% c("Tech", "Manufacturing"))

# Complex conditions
complex_filter <- filter(panel_data,
                         (revenue > 120 & employees > 60) |
                           (exports > 30 & year >= 2021)) #&

################################################################################
# 4.2 SELECT - Choosing Columns
################################################################################

# Select specific columns
core_vars <- select(panel_data, firm_id, year, revenue, employees)

# Select range of columns
select(panel_data, firm_id:revenue)

# Exclude columns
select(panel_data, -exports, -investment)

# Select with helper functions
select(panel_data, firm_id, year, starts_with("rev"))
select(panel_data, firm_id, year, contains("emp"))
select(panel_data, firm_id, year, everything())  # Reorder with firm_id, year first

################################################################################
# 4.3 MUTATE - Creating New Variables (CRITICAL FOR ECONOMETRICS)
################################################################################

panel_data <- panel_data %>%
  arrange(firm_id, year) %>%
  mutate(
    # Basic transformations
    revenue_squared = revenue^2,
    revenue_per_employee = revenue / employees,
    
    # Logarithmic transformations (handle zeros)
    log_revenue = log(revenue),
    log_employees = log(employees),
    log_exports = log(exports + 1),  # Add 1 to handle zeros
    
    # Time trends
    time_trend = year - min(year) + 1,
    time_trend_sq = time_trend^2,
    
    # Size categories
    size = case_when(
      employees < 60 ~ "Small",
      employees < 80 ~ "Medium",
      TRUE ~ "Large"
    )
  )

# Creating lagged variables (ESSENTIAL for panel data)
panel_data <- panel_data %>%
  arrange(firm_id, year) %>%
  group_by(firm_id) %>%
  mutate(
    # Lags
    revenue_lag1 = lag(revenue, 1),
    revenue_lag2 = lag(revenue, 2),
    employees_lag1 = lag(employees, 1),
    
    # Leads
    revenue_lead1 = lead(revenue, 1),
    
    # Growth rates
    revenue_growth = (revenue - revenue_lag1) / revenue_lag1 * 100,
    employment_growth = (employees - employees_lag1) / employees_lag1 * 100,
    
    # First differences
    d_revenue = revenue - revenue_lag1,
    d_log_revenue = log_revenue - lag(log_revenue),
    
    # Within-firm averages (for fixed effects)
    revenue_mean = mean(revenue, na.rm = TRUE),
    revenue_demean = revenue - revenue_mean
  ) %>%
  ungroup()

# View the results
head(select(panel_data, firm_id, year, revenue, revenue_lag1, revenue_growth), 10)

################################################################################
# 4.4 INTRODUCING AND HANDLING MISSING VALUES (NA)
################################################################################

# Introduce some missing values for demonstration
set.seed(456)
panel_with_na <- panel_data

# Create different types of missing data
# Random missing (5% of export values)
missing_indices <- sample(1:nrow(panel_with_na), size = 0.05 * nrow(panel_with_na))
panel_with_na$exports[missing_indices] <- NA

# Systematic missing (small firms less likely to report investment)
panel_with_na$investment[panel_with_na$employees < 60 & runif(nrow(panel_with_na)) < 0.3] <- NA

# Check missing values
colSums(is.na(panel_with_na))

# Create missing indicators (important for analysis)
panel_with_na <- panel_with_na %>%
  mutate(
    exports_missing = is.na(exports),
    investment_missing = is.na(investment)
  )

# Different ways to handle missing values
panel_imputed <- panel_with_na %>%
  group_by(firm_id) %>%
  mutate(
    # Method 1: Replace with mean (within firm)
    exports_mean_imputed = ifelse(is.na(exports), 
                                  mean(exports, na.rm = TRUE), 
                                  exports),
    
    # Method 2: Forward fill (carry last observation forward)
    exports_ffill = zoo::na.locf(exports, na.rm = FALSE),
    
    # Method 3: Linear interpolation
    exports_interpolated = zoo::na.approx(exports, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    # Method 4: Replace with overall mean
    investment_overall_mean = ifelse(is.na(investment),
                                     mean(investment, na.rm = TRUE),
                                     investment)
  )

# Remove rows with NA in critical variables
panel_complete <- panel_with_na %>%
  filter(!is.na(revenue), !is.na(employees))

################################################################################
# 4.5 SUMMARISE - Creating Summary Statistics
################################################################################

# Overall summary
overall_summary <- panel_data %>%
  summarise(
    n_obs = n(),
    mean_revenue = mean(revenue),
    sd_revenue = sd(revenue),
    median_revenue = median(revenue),
    min_revenue = min(revenue),
    max_revenue = max(revenue)
  )

print(overall_summary)

# Summary by year
yearly_summary <- panel_data %>%
  group_by(year) %>%
  summarise(
    n_firms = n(),
    mean_revenue = mean(revenue),
    mean_employees = mean(employees),
    total_exports = sum(exports),
    sd_revenue = sd(revenue)
  )

print(yearly_summary)

# Summary by industry
industry_summary <- panel_data %>%
  group_by(industry) %>%
  summarise(
    n_obs = n(),
    mean_revenue = mean(revenue),
    mean_employees = mean(employees),
    mean_productivity = mean(revenue_per_employee)
  )

print(industry_summary)

################################################################################
# 4.6 GROUP_BY with MUTATE - Within-group calculations
################################################################################

panel_grouped <- panel_data %>%
  group_by(year) %>%
  mutate(
    # Relative to year average
    revenue_vs_year_avg = revenue / mean(revenue),
    
    # Rank within year
    revenue_rank = rank(desc(revenue)),
    
    # Percentile within year
    revenue_percentile = percent_rank(revenue) * 100
  ) %>%
  ungroup()

# Industry-year calculations
panel_industry <- panel_data %>%
  group_by(industry, year) %>%
  mutate(
    industry_mean_revenue = mean(revenue),
    revenue_vs_industry = revenue / industry_mean_revenue,
    n_firms_industry = n()
  ) %>%
  ungroup()

################################################################################
# PART 5: CRITICAL - JOINS AND THE DUPLICATION PROBLEM
################################################################################

print("=== UNDERSTANDING JOINS AND THEIR DANGERS ===")

# Dataset 1: Firm information (one row per firm)
firm_info <- data.frame(
  firm_id = 1:5,
  firm_name = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon"),
  industry = c("Tech", "Finance", "Tech", "Retail", "Finance"),
  founded = c(2010, 2012, 2011, 2013, 2009),
  stringsAsFactors = FALSE
)

# Dataset 2: Yearly performance (multiple rows per firm)
yearly_performance <- data.frame(
  firm_id = c(1,1,1, 2,2,2, 3,3,3, 4,4,4, 5,5,5),
  year = rep(2021:2023, 5),
  revenue = runif(15, 100, 200),
  profit = runif(15, 10, 50)
)

# Dataset 3: Industry information (with problematic duplicate)
industry_info <- data.frame(
  industry = c("Tech", "Tech", "Finance", "Retail"),  # Tech appears TWICE!
  avg_margin = c(0.25, 0.30, 0.15, 0.10),
  risk_level = c("High", "Very High", "Medium", "Low"),
  stringsAsFactors = FALSE
)

print("Dataset 1: firm_info (5 rows)")
print(firm_info)
print("\nDataset 2: yearly_performance (15 rows)")
print(head(yearly_performance, 9))
print("\nDataset 3: industry_info (4 rows - Tech duplicated!)")

print(industry_info)

################################################################################
# 5.1 ONE-TO-MANY JOIN (Common and usually safe)
################################################################################

print("\n=== ONE-TO-MANY JOIN ===")

# Join firm info with yearly performance
result_one_to_many <- firm_info %>%
  left_join(yearly_performance, by = "firm_id")

print(paste("Original firm_info rows:", nrow(firm_info)))
print(paste("Result rows:", nrow(result_one_to_many)))
print(paste("Expected duplication: Each firm repeated for each year"))
print(head(result_one_to_many, 9))

# This is CORRECT - we expect 5 firms × 3 years = 15 rows

################################################################################
# 5.2 MANY-TO-MANY JOIN (DANGEROUS - Creates unwanted duplication)
################################################################################

print("\n=== MANY-TO-MANY JOIN PROBLEM ===")

# First join: firm_info with yearly_performance
temp_result <- firm_info %>%
  left_join(yearly_performance, by = "firm_id")

print(paste("After first join:", nrow(temp_result), "rows"))

# Second join: adding industry_info (PROBLEMATIC!)
problematic_result <- temp_result %>%
  left_join(industry_info, by = "industry")

print(paste("After second join:", nrow(problematic_result), "rows"))
print(paste("PROBLEM: We have", nrow(problematic_result), "rows instead of expected 15!"))

# Show the duplication for Tech firms
tech_problem <- problematic_result %>%
  filter(industry == "Tech") %>%
  select(firm_id, firm_name, year, industry, avg_margin, risk_level)

print("\nTech firms are duplicated:")
print(tech_problem)
print("Each Tech firm-year appears TWICE because Tech appears twice in industry_info")


################################################################################
# 5.4 FIXING THE DUPLICATE PROBLEM
################################################################################

print("\n=== FIXING DUPLICATE KEYS ===")

# Remove duplicates before joining
industry_info_clean <- industry_info %>%
  distinct(industry, .keep_all = TRUE)  # Keep first occurrence

print("Cleaned industry_info:")
print(industry_info_clean)

# Now join works correctly
correct_result <- temp_result %>%
  left_join(industry_info_clean, by = "industry")

print(paste("\nAfter clean join:", nrow(correct_result), "rows (correct!)"))

################################################################################
# 5.5 TYPES OF JOINS IN dplyr
################################################################################

print("\n=== DIFFERENT JOIN TYPES ===")

# Create example datasets
df_a <- data.frame(
  id = c(1, 2, 3, 4),
  value_a = c("A", "B", "C", "D")
)

df_b <- data.frame(
  id = c(2, 3, 4, 5),
  value_b = c("W", "X", "Y", "Z")
)

print("Dataset A:")
print(df_a)
print("\nDataset B:")
print(df_b)

# LEFT JOIN - Keep all from left
left_result <- df_a %>% left_join(df_b, by = "id")
print("\nLEFT JOIN (keep all from A):")
print(left_result)

# RIGHT JOIN - Keep all from right
right_result <- df_a %>% right_join(df_b, by = "id")
print("\nRIGHT JOIN (keep all from B):")
print(right_result)

# INNER JOIN - Keep only matches
inner_result <- df_a %>% inner_join(df_b, by = "id")
print("\nINNER JOIN (only matches):")
print(inner_result)

# FULL JOIN - Keep all from both
full_result <- df_a %>% full_join(df_b, by = "id")
print("\nFULL JOIN (all from both):")
print(full_result)

# ANTI JOIN - Keep non-matches from left
anti_result <- df_a %>% anti_join(df_b, by = "id")
print("\nANTI JOIN (A not in B):")
print(anti_result)

################################################################################
# EXERCISE 1: Data Frame Manipulation (10 minutes)
################################################################################
# Using panel_data:
# 1. Calculate the mean revenue by industry
# 2. Find the firm-year with highest revenue growth
# 3. Create a dummy variable for post-2021 period
# 4. Calculate the correlation between log_revenue and log_employees

# Solution hints:
# aggregate(revenue ~ industry, data = panel_data, mean)
# panel_data[which.max(panel_data$revenue_growth), ]

################################################################################
# PART 6: VARIABLE TRANSFORMATIONS FOR ECONOMETRICS
################################################################################

# Create comprehensive set of econometric variables
econometric_data <- panel_data %>%
  arrange(firm_id, year) %>%
  group_by(firm_id) %>%
  mutate(
    # === LOG TRANSFORMATIONS ===
    # Standard log (for positive values)
    log_revenue = log(revenue),
    log_investment = log(investment),
    
    # Log with offset for zeros
    log_exports_adj = log(exports + 1),
    
    # Inverse hyperbolic sine (handles zeros and negatives)
    asinh_exports = asinh(exports),
    
    # === GROWTH RATES ===
    # Percentage growth
    revenue_growth_pct = (revenue - lag(revenue)) / lag(revenue) * 100,
    
    # Log difference (approximates growth rate)
    revenue_growth_log = log(revenue) - log(lag(revenue)),
    
    # === SQUARED AND INTERACTION TERMS ===
    employees_squared = employees^2,
    log_revenue_squared = log_revenue^2,
    
    # Interaction between size and growth
    size_growth_interaction = as.numeric(size == "Large") * revenue_growth,
    
    # === MOVING AVERAGES ===
    revenue_ma2 = (revenue + lag(revenue)) / 2,
    revenue_ma3 = (lag(revenue) + revenue + lead(revenue)) / 3,
    
    # === DUMMY VARIABLES ===
    post_2021 = as.numeric(year > 2021),
    is_tech = as.numeric(industry == "Tech"),
    is_large = as.numeric(size == "Large"),
    
    # === RATIOS ===
    productivity = revenue / employees,
    export_intensity = exports / revenue,
    
    # === TIME TRENDS ===
    firm_specific_trend = row_number(),
    
    # === DEVIATIONS FROM MEAN (for fixed effects) ===
    revenue_firm_mean = mean(revenue, na.rm = TRUE),
    revenue_demeaned = revenue - revenue_firm_mean
  ) %>%
  ungroup()

# Add year dummies for time fixed effects
for(yr in unique(econometric_data$year)) {
  econometric_data[[paste0("year_", yr)]] <- as.numeric(econometric_data$year == yr)
}

# Add industry dummies
for(ind in unique(econometric_data$industry)) {
  econometric_data[[paste0("ind_", ind)]] <- as.numeric(econometric_data$industry == ind)
}

################################################################################
# PART 7: DATA QUALITY CHECKS
################################################################################

# Check for outliers using multiple methods
outlier_analysis <- econometric_data %>%
  group_by(industry) %>%
  mutate(
    # Z-score method
    revenue_z = (revenue - mean(revenue)) / sd(revenue),
    is_outlier_z = abs(revenue_z) > 3,
    
    # IQR method
    q1 = quantile(revenue, 0.25),
    q3 = quantile(revenue, 0.75),
    iqr = q3 - q1,
    is_outlier_iqr = revenue < (q1 - 1.5 * iqr) | revenue > (q3 + 1.5 * iqr)
  ) %>%
  ungroup()

# Summary of outliers
outlier_summary <- outlier_analysis %>%
  summarise(
    outliers_zscore = sum(is_outlier_z),
    outliers_iqr = sum(is_outlier_iqr)
  )

print("Outlier Detection Summary:")
print(outlier_summary)

# Winsorize extreme values
winsorize <- function(x, probs = c(0.01, 0.99)) {
  quantiles <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  return(x)
}

econometric_data$revenue_winsorized <- winsorize(econometric_data$revenue)
econometric_data$employees_winsorized <- winsorize(econometric_data$employees)

################################################################################
# VISUALIZATION: Panel Data Patterns
################################################################################

# Revenue trends by firm
ggplot(panel_data, aes(x = year, y = revenue, color = factor(firm_id))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ industry) +
  theme_minimal() +
  labs(title = "Revenue Trends by Firm and Industry",
       x = "Year", y = "Revenue", color = "Firm ID")

# Distribution of revenue growth
ggplot(filter(panel_data, !is.na(revenue_growth)), 
       aes(x = revenue_growth)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "Distribution of Revenue Growth Rates",
       x = "Revenue Growth (%)", y = "Frequency")

################################################################################
# PART 8: PREPARING DATA FOR REGRESSION
################################################################################

# Create final dataset for analysis
regression_ready <- econometric_data %>%
  # Remove first year for each firm (no lags)
  filter(!is.na(revenue_lag1)) %>%
  # Select relevant variables
  select(
    # Identifiers
    firm_id, year,
    # Dependent variables
    revenue, log_revenue, revenue_growth_pct,
    # Independent variables
    employees, log_employees, employees_squared,
    investment, log_investment,
    exports, log_exports_adj,
    # Lagged variables
    revenue_lag1, employees_lag1,
    # Fixed effects indicators
    industry, starts_with("year_"), starts_with("ind_"),
    # Other controls
    firm_specific_trend, productivity
  ) %>%
  # Remove any remaining NAs in key variables
  filter(complete.cases(revenue, employees, investment))