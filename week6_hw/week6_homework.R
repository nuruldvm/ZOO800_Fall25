#Zoo800_Fall2025: Course 
#homework for week 6
#Nurul Islam
#date: 10/11/2024
#load the required packages
library(ggplot2)

# --- Model Parameters ---
K <- 1000      # Carrying capacity 
N0 <- 50       # Initial population size 
r_mean <- 0.2  # Mean intrinsic rate of growth 
r_sd <- 0.03   # Standard deviation of r 

## Objective 1: Simulating and Plotting a Single Population Trajectory 
#In this first part, I'll simulate the "most likely" outcome by using just the average growth 
#rate, r=0.2 to see how the hellbender population is expected to grow over 10 years 
#without considering any uncertainty yet

  years <- 10

# 1. Create a vector to store the results. 
# We'll start it with the initial population size.
  pop_size <- numeric(years + 1) # We need space for N0 (year 0) plus 10 more years
  pop_size[1] <- N0             # The first entry is our N0 at year 0

# 2. Loop through each year to calculate the next year's population
# We loop from 1 to 'years' (10)
for (t in 1:years) {
  # The population at the start of the year is pop_size[t]
  Nt <- pop_size[t] 
  
  # Apply the logistic growth formula to get the next year's population
  # This result will be stored in the next position in our vector
  pop_size[t+1] <- Nt + r_mean * Nt * (1 - Nt / K)
}
# Let's look at the result!
print(pop_size)

# 1. Create a data frame for plotting
# The 'Year' column will go from 0 to 10
sim_df_obj1 <- data.frame(
  Year = 0:years,
  Population = pop_size
)

# 2. Build the plot
ggplot(data = sim_df_obj1, aes(x = Year, y = Population)) +
  geom_line(color = "blue", size = 1.5) + # Draw a nice, thick blue line
  geom_point(color = "blue", size = 2) +  # Add points for each year's data
  labs(
    title = "Expected Hellbender Population Growth Over 10 Years",
    x = "Year",
    y = "Population Size (N)"
  ) +
  theme_minimal() # A clean theme for the plot
# This plot shows how the population is expected to grow over 10 years
# --- Objective 2: Simulating Multiple Trajectories with Uncertainty ---
# Number of simulations and years
num_sims <- 50
years <- 10

# 1. Generate 50 random r values
r_values <- rnorm(n = num_sims, mean = r_mean, sd = r_sd)

# 2. Create a matrix to store all results
# Rows = years (0 to 10), Columns = simulations
# We use 'NA' as a placeholder
sim_matrix <- matrix(NA, nrow = years + 1, ncol = num_sims)

# Set the first row of the matrix to the initial population size for all simulations
sim_matrix[1, ] <- N0

# 3. Run the nested loops
# Outer loop: for each simulation
for (i in 1:num_sims) {
  # Inner loop: for each year
  for (t in 1:years) {
    Nt <- sim_matrix[t, i]
    # Use the i-th value from our vector of random r's
    sim_matrix[t+1, i] <- Nt + r_values[i] * Nt * (1 - Nt / K)
  }
}
# We need to turn our 'wide' matrix into a 'long' data frame
# First, convert the matrix to a data frame
sim_df_obj2_wide <- as.data.frame(sim_matrix)
sim_df_obj2_wide$Year <- 0:years # Add a year column

# Now, 'pivot' the data frame from wide to long
# This requires the 'tidyr' package (part of the tidyverse)
library(tidyr)
sim_df_obj2_long <- pivot_longer(
  sim_df_obj2_wide,
  cols = -Year, # Pivot all columns except 'Year'
  names_to = "SimulationID",
  values_to = "Population"
)

# Now let's create the plot
ggplot() +
  # Add the 50 simulations. We use aes(group = SimulationID) to tell ggplot
  # to draw a separate line for each simulation. We make them semi-transparent.
  geom_line(data = sim_df_obj2_long, aes(x = Year, y = Population, group = SimulationID), color = "gray", alpha = 0.6) +
  # Add the 'mean' trajectory from Objective 1 on top so it stands out
  geom_line(data = sim_df_obj1, aes(x = Year, y = Population), color = "red", size = 1.5) +
  labs(
    title = "Range of Possible Population Trajectories (10 Years)",
    subtitle = "50 simulations with uncertainty in growth rate 'r'",
    x = "Year",
    y = "Population Size (N)"
  ) +
  theme_minimal()
# --- Rerunning simulation for 25 years and more runs ---
num_sims <- 1000 # More simulations for a better probability estimate
years <- 25      # New time frame 

# Generate new r values for these simulations
r_values <- rnorm(n = num_sims, mean = r_mean, sd = r_sd)

# Create the storage matrix
sim_matrix_25yr <- matrix(NA, nrow = years + 1, ncol = num_sims)
sim_matrix_25yr[1, ] <- N0

# Run the nested loops again
for (i in 1:num_sims) {
  for (t in 1:years) {
    Nt <- sim_matrix_25yr[t, i]
    sim_matrix_25yr[t+1, i] <- Nt + r_values[i] * Nt * (1 - Nt / K)
  }
}

# Convert to a long data frame for plotting
sim_df_25yr_wide <- as.data.frame(sim_matrix_25yr)
sim_df_25yr_wide$Year <- 0:years
sim_df_25yr_long <- pivot_longer(
  sim_df_25yr_wide,
  cols = -Year,
  names_to = "SimulationID",
  values_to = "Population"
)

# Define the target population
target_pop <- 0.80 * K # 80% of K [cite: 9]

ggplot(sim_df_25yr_long, aes(x = Year, y = Population, group = SimulationID)) +
  geom_line(color = "lightblue", alpha = 0.4) +
  # Add the horizontal line for the target population
  geom_hline(yintercept = target_pop, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Population Projections vs. Conservation Target (25 Years)",
    x = "Year",
    y = "Population Size (N)"
  ) +
  theme_minimal()

# Extract the population sizes from the final year (row 'years + 1')
final_pops <- sim_matrix_25yr[years + 1, ]

# Create a data frame for the histogram
final_pops_df <- data.frame(FinalPopulation = final_pops)

# Plot the histogram
ggplot(final_pops_df, aes(x = FinalPopulation)) +
  geom_histogram(binwidth = 25, fill = "darkgreen", color = "white", alpha = 0.8) +
  # Add a vertical line for the target population
  geom_vline(xintercept = target_pop, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Distribution of Population Size at Year 25",
    x = "Final Population Size",
    y = "Frequency (Number of Simulations)"
  ) +
  theme_minimal()
# Count how many simulations met or exceeded the target
success_count <- sum(final_pops >= target_pop)

# Calculate the fraction
success_fraction <- success_count / num_sims

# Print the result
print(paste("Fraction of populations meeting the target:", success_fraction))

# Now let's add this information to our histogram [cite: 28]
# We'll use annotate() to place the text on the plot
ggplot(final_pops_df, aes(x = FinalPopulation)) +
  geom_histogram(binwidth = 25, fill = "darkgreen", color = "white", alpha = 0.8) +
  geom_vline(xintercept = target_pop, linetype = "dashed", color = "red", size = 1) +
  annotate(
    "text", 
    x = 400, y = 100, # Position of the text (you may need to adjust this)
    label = paste("Fraction reaching target =", round(success_fraction, 2)),
    color = "black",
    size = 5
  ) +
  labs(
    title = "Distribution of Population Size at Year 25",
    x = "Final Population Size",
    y = "Frequency (Number of Simulations)"
  ) +
  theme_minimal()

# Count how many simulations met or exceeded the target
success_count <- sum(final_pops >= target_pop)

# Calculate the fraction
success_fraction <- success_count / num_sims

# Print the result
print(paste("Fraction of populations meeting the target:", success_fraction))

# Now let's add this information to our histogram [cite: 28]
# We'll use annotate() to place the text on the plot
ggplot(final_pops_df, aes(x = FinalPopulation)) +
  geom_histogram(binwidth = 25, fill = "darkgreen", color = "white", alpha = 0.8) +
  geom_vline(xintercept = target_pop, linetype = "dashed", color = "red", size = 1) +
  annotate(
    "text", 
    x = 400, y = 100, # Position of the text (you may need to adjust this)
    label = paste("Fraction reaching target =", round(success_fraction, 2)),
    color = "black",
    size = 5
  ) +
  labs(
    title = "Distribution of Population Size at Year 25",
    x = "Final Population Size",
    y = "Frequency (Number of Simulations)"
  ) +
  theme_minimal()
