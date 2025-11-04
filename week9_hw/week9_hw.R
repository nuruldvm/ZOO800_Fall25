# Load necessary libraries
library(ggplot2)
library(dplyr)

# --- Part A: Generate Data ---

# Set a random seed for reproducibility
set.seed(42) 

# Define constants
n <- 100  # Number of observations
alpha <- 5  # Our made-up intercept 
beta <- 3   # Our made-up slope 

# 1. Generate 100 observations of x from a uniform distribution 
x <- runif(n, min = 0, max = 10)

# 2. Generate data for sigma = 1 
epsilon_1 <- rnorm(n, mean = 0, sd = 1) 
y_1 <- alpha + beta * x + epsilon_1 
data_1 <- data.frame(x = x, y = y_1, sigma = "sigma = 1")

# 3. Generate data for sigma = 10 
epsilon_10 <- rnorm(n, mean = 0, sd = 10)
y_10 <- alpha + beta * x + epsilon_10 
data_10 <- data.frame(x = x, y = y_10, sigma = "sigma = 10")

# 4. Generate data for sigma = 25 
epsilon_25 <- rnorm(n, mean = 0, sd = 25) 
y_25 <- alpha + beta * x + epsilon_25 
data_25 <- data.frame(x = x, y = y_25, sigma = "sigma = 25")

# 5. Combine into one long data frame for plotting
all_data <- rbind(data_1, data_10, data_25)

# Optional: Convert 'sigma' to a factor to ensure the plot order
all_data$sigma <- factor(all_data$sigma, levels = c("sigma = 1", "sigma = 10", "sigma = 25"))

# lets inspect the first few rows of the combined data
print(head(all_data))

# --- Part B: Create the Plot ---

ggplot(all_data, aes(x = x, y = y)) +
  geom_point(alpha = 0.7) +  # Scatter plot with some transparency
  # Use facet_wrap to create separate panels for each sigma 
  # Setting nrow = 1 puts them all in a single row 
  facet_wrap(~ sigma, nrow = 1) +
  labs(
    title = "Effect of Observation Error (σ) on y vs. x Relationship",
    x = "Predictor Variable (x)",
    y = "Response Variable (y)"
  ) +
  theme_bw() # A clean black-and-white theme

# --- Objective 1, Part C: Analysis ---

# As the observation error (sigma) increases, the ability to visually
# detect the underlying linear relationship between y and x decreases significantly.
# Here is the analysis for each sigma level:
# - At sigma = 1: The "noise" (error) is very small. The data points
#   form a tight, clear line, making the positive linear relationship
#   unmistakable. The "signal" (the true relationship) is much
#   stronger than the "noise".
#
# - At sigma = 10: The noise is larger. The points are much more
#   scattered, forming a "cloud" rather than a distinct line.
#   A general positive trend is still visible, but the relationship
#   is much less precise.
#
# - At sigma = 25: The noise is very large and completely overwhelms
#   the signal. The plot resembles a random cloud of points with no
#   clear pattern. It is nearly impossible to visually detect the
#   true underlying relationship.
# In summary, as sigma increases, the clarity of the linear relationship
# between y and x diminishes, illustrating how high observation error
# can obscure true patterns in data.

# --- Objective 2: Coin Flip Simulation ---

# --- 1. Set up Simulation Parameters ---

set.seed(42) # For reproducible results

n_sims <- 100         # "number of times out of 100" 
max_flips <- 20      # "for 1 to 20 coin flips" 
alpha_level <- 0.05  # "significantly unfair (alpha < 0.05)" 
p_null <- 0.5        # The reference value we are testing against 

# Probabilities to test (covers Part A and B)
true_probs <- c(0.55, 0.60, 0.65) # 

# --- 2. Create a Helper Function ---
# This function runs the 100 simulations for a *single* scenario.

run_simulation <- function(n_flips, true_p) {
  
  # Keep track of how many times we find a "significant" result
  significant_count <- 0
  
  # Run 100 simulations
  for (i in 1:n_sims) {
    
    # Simulate n_flips coin tosses and count the number of heads
    # rbinom(1, size, prob) is a perfect Bernoulli trial simulator
    n_heads <- rbinom(1, size = n_flips, prob = true_p)
    
    # Test if the result is significantly GREATER than 0.5
    # We use a one-sided binomial test
    test_result <- binom.test(
      x = n_heads,          # Number of "successes" (heads)
      n = n_flips,          # Number of trials (flips)
      p = p_null,           # The null hypothesis p=0.5
      alternative = "greater" # Test if p > 0.5 
    )
    
    # Check if the p-value is below our alpha
    if (test_result$p.value < alpha_level) {
      significant_count <- significant_count + 1
    }
  }
  
  # Return the probability of detecting unfairness (our "power")
  return(significant_count / n_sims)
}

# --- 3. Run the Full Simulation for All Scenarios ---

# We'll create a data frame with every combination of flips and probabilities
# This is a clean way to store all the parameters we need to test.
params <- expand.grid(
  n_flips = 1:max_flips,
  true_p = true_probs
)

# Run the simulation for every row in our 'params' data frame
# We use 'rowwise()' to apply the function to each row individually
# This might take a few seconds to run
results_df <- params %>%
  rowwise() %>%
  mutate(power = run_simulation(n_flips, true_p)) %>%
  ungroup() # Always good practice to ungroup after

# --- 4. Plot the Results (Part A & B) ---

# Convert true_p to a factor for better plotting
results_df$true_p <- as.factor(results_df$true_p)

ggplot(results_df, aes(x = n_flips, y = power, color = true_p)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  # Add a horizontal line at 80% power, a common target in power analysis
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey40") +
  annotate("text", x = 3, y = 0.82, label = "80% Power", color = "grey40") +
  
  # Add clear labels
  labs(
    title = "Statistical Power to Detect an Unfair Coin",
    subtitle = "Based on 100 simulations per point",
    x = "Number of Coin Flips (Sample Size)",
    y = "Probability of Detecting Unfairness (Power)",
    color = "True p(heads)"
  ) +
  
  # Clean up the axes
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 20, by = 2)) +
  theme_minimal()