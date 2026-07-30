#title: 'Introducción a R'
#output: Taller

# Load necessary libraries for data manipulation, reshaping, and reporting
library(tidyverse)  # Collection of R packages for data science
library(reshape2)   # For reshaping data between wide and long formats
library(stargazer)  # For creating LaTeX tables from regression results

# Clear the workspace to remove any existing objects
rm(list = ls())  # This ensures a clean start by removing any previously stored variables.

# Set the working directory where the code and data are stored
# Replace this path with your own directory
setwd("~/Downloads/class/00 - Intro to R")

# Generate a dataset with random numbers from an exponential distribution
datos_x <- cbind.data.frame(floor(matrix(rexp(25, rate = .1), ncol = 5)), 
                            c(NA, NA, 1, 1, 1), c(2, 2, NA, NA, NA), c(0:4))
# Name the columns of the dataset
names(datos_x) <- c(paste0("X", c(1:7)), "id")

# Display the first few rows of the dataset to check the structure
head(datos_x, 5)

# Add a new variable as the cumulative sum of X1, X2, and X3
datos_x <- datos_x %>%
  mutate(cum_sum = X1 + X2 + X3)  # Creating the cumulative sum column

# Filter rows where the cumulative sum is greater than 15
filtered_data <- datos_x %>% filter(cum_sum > 15)

# Display the filtered dataset
# filtered_data

####Exercise 1-----
#Create a new variable in datos_x that represents the cumulative sum of the first three columns (X1, X2, X3).
#Filter the rows where the sum of X1, X2, and X3 is greater than 15.

# Your code for Exercise 1


# Generate a dataset with random numbers from a normal distribution
datos_y <- cbind.data.frame(matrix(rnorm(50), ncol = 5), c(1:10))
# Name the columns of the dataset
names(datos_y) <- c(paste0("Y", c(1:5)), "id")

# Display the first few rows of the dataset to check the structure
head(datos_y, 5)

# Add a new variable as the square of Y1
datos_y <- datos_y %>%
  mutate(Y1_square = Y1^2)  # Squaring Y1 and creating a new column

# Create a summary showing the mean and standard deviation of each column
summary_stats <- datos_y %>%
  summarise(across(everything(), list(mean = mean, sd = sd)))

# Display the summary statistics
summary_stats

# Manipulate the 'datos_x' dataset
nueva_base_x <- datos_x %>%
  select(-X4) %>%                # Remove the 4th column
  mutate(X8 = X1 - X2 + X3) %>%  # Create a new variable 'X8' as X1 - X2 + X3
  slice(-n()) %>%                # Remove the last row
  mutate(NUEVA = coalesce(X6, X7)) %>%  # Merge columns X6 and X7, keeping the first non-NA value
  filter(X6 == 1) %>%            # Filter rows where X6 equals 1
  rename(EDAD = X2)              # Rename column X2 to EDAD

# Display the modified dataset to review changes
nueva_base_x

# Create a new variable X9 as the product of X1 and X3
nueva_base_x <- nueva_base_x %>%
  mutate(X9 = X1 * X3)  # Creating X9 as the product of X1 and X3

# Filter the dataset where X9 is greater than the median of X9
filtered_nueva_base_x <- nueva_base_x %>%
  filter(X9 > median(X9))

# Display the filtered dataset
filtered_nueva_base_x

# Clean the workspace but retain the essential datasets
rm(list = setdiff(ls(), c("datos_x", "datos_y")))

# Read in the ENPOL data and control variables
datos <- read.csv("./data/datos.csv")
controles <- read.csv("./data/controles.csv")

# Display the first few rows of each dataset to understand their structure
head(datos, 5)
head(controles, 5)

# Calculate proportions by age groups

controles <- controles %>%
  mutate(age_group = cut(age, breaks = seq(20, 80, by = 10)))  # Create age groups

proportions <- controles %>%
  group_by(age_group) %>%
  summarise(reading = mean(lit_read == 1, na.rm = TRUE),
            writing = mean(lit_write == 1, na.rm = TRUE))

# Visualize the proportions
ggplot(proportions, aes(x = age_group)) +
  geom_bar(aes(y = reading, fill = "Reading"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = writing, fill = "Writing"), stat = "identity", position = "dodge") +
  labs(title = "Proportion of Reading and Writing by Age Group", x = "Age Group", y = "Proportion") +
  theme_minimal()

# Summarize the 'controles' dataset by 'private' status
controles_sum <- controles %>%
  group_by(private) %>%
  summarise(edad_m = mean(age),
            across(c(lit_write, lit_read), mean, na.rm = TRUE),
            n = n())#ojo revisa que ves raro en este código

sum(is.na(controles$age)) #así revisas

# Display the summarized data to verify
controles_sum

# Extend the summary with min and max age
controles_sum_extended <- controles %>%
  summarise(min_age = min(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE))#sucede otra vez?

# Display the extended summary
controles_sum_extended

# Filter and select relevant columns for reshaping
datos_reshape <- controles %>%
  filter(child == 1) %>% 
  select(ID_PER, agechild1:agechild3) %>% 
  arrange(desc(ID_PER))

# Display the reshaped data
head(datos_reshape)

# Create a new variable categorizing the minimum age into brackets
datos_reshape <- datos_reshape %>%
  mutate(age_bracket = case_when(
    agechild1 <= 5 ~ "0-5",
    agechild1 <= 10 ~ "6-10",
    TRUE ~ "11+"
  ))

# Count the number of individuals in each age bracket category
age_bracket_counts <- datos_reshape %>%
  count(age_bracket)

# Display the counts
age_bracket_counts

# Different types of joins with simulated data
inner_join(datos_x, datos_y, by = "id")   # Inner join
left_join(datos_x, datos_y, by = "id")    # Left join
right_join(datos_x, datos_y, by = "id")   # Right join
full_join(datos_x, datos_y, by = "id")    # Full join

# Randomly sample 300 observations from 'datos'
datos_rest <- datos %>% slice_sample(n = 300)

# Merge 'controles' and 'datos_age' with the sampled 'datos_rest'
datos_final <- datos_rest %>% 
  left_join(controles %>% select(-private), by = "ID_PER") %>% 
  left_join(datos_age, by = "ID_PER")

# Display the final merged dataset
head(datos_final, 5)


# Identify and replace missing values with the column mean
datos_final <- datos_final %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Create a subset for a specific age group (e.g., 30-40)
age_subset <- datos_final %>%
  filter(age >= 30 & age <= 40)

# Display the subset
head(age_subset)


# Identify and replace missing values with the column mean
datos_final <- datos_final %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Create a subset for a specific age group (e.g., 30-40)
age_subset <- datos_final %>%
  filter(age >= 30 & age <= 40)

# Display the subset
head(age_subset)

# Clear the workspace to prepare for the next analysis
rm(list = ls())

# Load the iris dataset
data(iris)

# Display the first few rows of the iris dataset
head(iris, 5)

# Create a scatter plot to visualize the relationship between Sepal.Length and Petal.Width
g1 <- ggplot(iris, aes(Sepal.Length, Petal.Width)) + 
  geom_point(aes(Sepal.Length, Petal.Width, color = factor(Species))) + # Scatter plot with color by Species
  theme(legend.title = element_blank()) + 
  geom_smooth(method = 'lm', se = FALSE) # Add a linear regression line without the confidence interval

# Save the plot as a PDF file
ggsave(g1, filename = 'scatter_iris.pdf', device = cairo_pdf,
       dpi = 300, width = 12, height = 10, units = 'cm')

# Display the plot in the RMarkdown document
g1


####some important issues!
# Generate example datasets
# Left dataset
left_data <- data.frame(
  id = c(1, 1, 2, 3),
  value_left = c("A", "B", "C", "D")
)

# Right dataset
right_data <- data.frame(
  id = c(1, 1, 2, 4),
  value_right = c("W", "X", "Y", "Z")
)

# Display the datasets
print(left_data)
print(right_data)

# Perform a left join
merged_data <- left_join(left_data, right_data, by = "id")

# Display the merged dataset
print(merged_data)

# Solution: Aggregating or summarizing the data before the merge

# Summarize the right dataset to avoid duplication
right_data_summarized <- right_data %>%
  group_by(id) %>%
  summarize(value_right = paste(value_right, collapse = ", "))  # Combine multiple values into one

# Perform the left join again with the summarized data
merged_data_solved <- left_join(left_data, right_data_summarized, by = "id")





