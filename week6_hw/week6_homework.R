# ZOO 800: Week 6 Homework
# Name: Nurul Islam
# Date: 10/13/2025

# -- 1. Setup: Load Packages and Define Global Parameters --

# I'm loading the 'tidyverse' package, which includes ggplot2 for plotting
# and tidyr for data manipulation. It's good practice to load all
# necessary packages at the start of the script.
library(tidyverse)

# These are the core parameters for our model, based on the problem description in the hw
K <- 1000      # Carrying capacity of the environment
N0 <- 50       # Initial population size of the hellbenders
r_mean <- 0.2  # The mean intrinsic rate of growth (r) after the intervention
r_sd <- 0.03   # The standard deviation of r, representing our uncertainty

# -- Objective 1: Simulate and Plot a Single Population Trajectory --

# For this objective, I'm simulating the population change over 10 years
# using only the mean growth rate (r = 0.2). This gives us a baseline
# expectation of the population's recovery without considering uncertainty.

# Set the time frame for this initial simulation
years_obj1 <- 10

# I'll create a numeric vector to store the population size for each year.
# It needs to be 'years + 1' long to include the initial size at Year 0.
pop_scenario_mean <- numeric(years_obj1 + 1)
pop_scenario_mean[1] <- N0 # The first element is our starting population.

# This 'for' loop iterates from year 1 to 10. In each step, it calculates
# the next year's population based on the previous year's, using the
# [cite_start]discrete-time logistic growth equation. [cite: 12]
for (t in 1:years_obj1) {
  Nt <- pop_scenario_mean[t]
  pop_scenario_mean[t + 1] <- Nt + r_mean * Nt * (1 - Nt / K)
}

# To plot this with ggplot2, I first need to put the results into a data frame.
df_obj1 <- data.frame(
  Year = 0:years_obj1,
  Population = pop_scenario_mean
)

# Now, I'll create the plot showing the expected population trajectory
plot_obj1 <- ggplot(data = df_obj1, aes(x = Year, y = Population)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 2.5) +
  labs(
    title = "Expected Hellbender Population Growth Over 10 Years",
    subtitle = "Based on the mean intrinsic growth rate (r = 0.2)",
    x = "Year",
    y = "Population Size"
  ) +
  theme_bw() # Using a clean black and white theme

print(plot_obj1)


# -- Objective 2: Incorporate Uncertainty with Multiple Simulations --

# The first plot is our best guess, but it doesn't show the uncertainty in 'r'.
# Here, I'll run 50 simulations, each with a different 'r' value drawn from a
# normal distribution. This will show a range of plausible outcomes.

# Set parameters for this simulation
num_sims_obj2 <- 50
years_obj2 <- 10

# First, I'll generate 50 random 'r' values from a normal distribution.
r_values_50 <- rnorm(n = num_sims_obj2, mean = r_mean, sd = r_sd)

# I'm creating a matrix to store the results of all 50 simulations.
# Each column represents a different simulation, and each row represents a year.
sim_matrix_50 <- matrix(nrow = years_obj2 + 1, ncol = num_sims_obj2)
sim_matrix_50[1, ] <- N0 # Set the first row (Year 0) to N0 for all simulations.

# This loop simulates all trajectories. I've made it more efficient by removing
# the inner loop. At each time step 't', this code calculates the next population
# size for ALL 50 simulations at once (a vectorized operation).
for (t in 1:years_obj2) {
  Nt <- sim_matrix_50[t, ]
  sim_matrix_50[t + 1, ] <- Nt + r_values_50 * Nt * (1 - Nt / K)
}

# For plotting with ggplot2, I need to convert the 'wide' matrix into a 'long'
# data frame. I'll add a 'Year' column first, then use pivot_longer().
df_obj2_long <- as.data.frame(sim_matrix_50) %>%
  mutate(Year = 0:years_obj2) %>%
  pivot_longer(
    cols = -Year,
    names_to = "SimulationID",
    values_to = "Population"
  )

# This plot shows all 50 simulations as gray lines, with the "mean" trajectory
# rom Objective 1 layered on top in red to make it stand out.
plot_obj2 <- ggplot() +
  # The uncertain trajectories (using transparency to see them all)
  geom_line(data = df_obj2_long, aes(x = Year, y = Population, group = SimulationID), color = "grey80", alpha = 0.8) +
  # The mean trajectory from Objective 1
  geom_line(data = df_obj1, aes(x = Year, y = Population), color = "red", size = 1.2) +
  labs(
    title = "Hellbender Population Trajectories with Uncertainty",
    subtitle = "50 simulations with random growth rates 'r' over 10 years",
    x = "Year",
    y = "Population Size"
  ) +
  theme_bw()

print(plot_obj2)


# -- Objective 3: Assess Long-Term Success Against a Conservation Target --

# Finally, I'll address the main policy question: what are the chances of the
# population reaching the target size within 25 years? 
# To get a good estimate of this probability, I'll increase the number of simulations to 1,000.

# Set parameters for the final, longer-term simulation
num_sims_obj3 <- 1000
years_obj3 <- 25

# [The conservation target is 80% of the carrying capacity (K).
target_pop <- 0.8 * K

# Generate 1,000 new random 'r' values for this simulation.
r_values_1000 <- rnorm(n = num_sims_obj3, mean = r_mean, sd = r_sd)

# Create the storage matrix for the 25-year, 1000-run simulation.
sim_matrix_1000 <- matrix(nrow = years_obj3 + 1, ncol = num_sims_obj3)
sim_matrix_1000[1, ] <- N0 # Set initial population

# Run the efficient, vectorized simulation loop for 25 years.
for (t in 1:years_obj3) {
  Nt <- sim_matrix_1000[t, ]
  sim_matrix_1000[t + 1, ] <- Nt + r_values_1000 * Nt * (1 - Nt / K)
}

# --- Part A: Plotting Trajectories with the Target Line ---
# I'll convert the final simulation results to a long data frame for plotting.
df_obj3_long <- as.data.frame(sim_matrix_1000) %>%
  mutate(Year = 0:years_obj3) %>%
  pivot_longer(
    cols = -Year,
    names_to = "SimulationID",
    values_to = "Population"
  )

# This plot shows all 1,000 trajectories along with a dashed red line
# [cite_start]indicating the conservation target. [cite: 25]
plot_obj3_trajectories <- ggplot(data = df_obj3_long, aes(x = Year, y = Population, group = SimulationID)) +
  geom_line(color = "lightblue", alpha = 0.2) +
  geom_hline(yintercept = target_pop, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "1,000 Population Projections vs. Conservation Target",
    subtitle = "Simulations run for 25 years",
    x = "Year",
    y = "Population Size"
  ) +
  theme_bw()

print(plot_obj3_trajectories)

# --- Parts B & C: Histogram and Calculating the Success Rate ---
# To answer the main question, I'll look at the population distribution in the final year.

# First, I'll extract the population sizes from the very last year (year 25).
final_populations <- sim_matrix_1000[years_obj3 + 1, ]

# Next, I'll calculate the fraction of simulations that met or exceeded the target.
num_success <- sum(final_populations >= target_pop)
fraction_success <- num_success / num_sims_obj3

# Now, I'll create the histogram of final population sizes.
# I'm also adding a vertical line for the target and an annotation with
plot_obj3_histogram <- ggplot(data = data.frame(FinalPop = final_populations), aes(x = FinalPop)) +
  geom_histogram(binwidth = 20, fill = "#006D2C", color = "white", alpha = 0.9) +
  geom_vline(xintercept = target_pop, linetype = "dashed", color = "red", size = 1) +
  # I'm using annotate() to place the text on the plot. I chose coordinates
  # that place it in an empty area of the plot for readability.
  annotate(
    "text",
    x = 550, y = 120, # Manually chosen coordinates
    label = paste("Fraction reaching target:", round(fraction_success, 3)),
    size = 4.5,
    hjust = 0 # Left-align the text
  ) +
  labs(
    title = "Distribution of Hellbender Population Size at Year 25",
    subtitle = paste("Based on", num_sims_obj3, "simulations."),
    x = "Final Population Size",
    y = "Frequency (Number of Simulations)"
  ) +
  theme_bw()

print(plot_obj3_histogram)

# Finally, I'll print the exact fraction to the console.
print(paste("The fraction of simulations reaching the target population of", target_pop, "is:", fraction_success))

# Saving the final, most informative plot as a PNG file.
ggsave("hellbender_population_histogram.png", plot = plot_obj3_histogram, width = 8, height = 6)