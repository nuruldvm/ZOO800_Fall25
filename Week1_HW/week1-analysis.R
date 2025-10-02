# Quantitative Methods in Ecology and Evolution, Homework 1
# Nurul Islam
# September 8, 2025
# Modeling and visualizing logistic population growth

# --------------------------------------------------------------------------
# Problem Setup
# --------------------------------------------------------------------------
# Parameters for the logistic growth model
K <- 1000      # Carrying capacity
r <- 0.0005    # Intrinsic growth rate
N0 <- 1        # Initial population size
start_year <- 1950
end_year <- 2025

# --------------------------------------------------------------------------
# Objective 1: Calculate Population Size (Nt) Over Time
# --------------------------------------------------------------------------

# This revised function is more self-contained and calculates Nt based on time `t`.
calculate_Nt <- function(K, N0, r, t) {
  return(K / (1 + ((K - N0) / N0) * exp(-r * K * t)))
}

# Create a data frame to hold all results. This is more flexible than a matrix.
years <- seq(start_year, end_year)
time_elapsed <- years - start_year # Calculate the time vector `t`

pop_growth_df <- data.frame(
  year = years,
  time = time_elapsed,
  # Calculate population with the base growth rate `r`
  population_r1 = calculate_Nt(K, N0, r, time_elapsed),
  # Calculate population with double the growth rate `r`
  population_r2 = calculate_Nt(K, N0, r * 2, time_elapsed)
)

# --------------------------------------------------------------------------
# Objective 2 & 3: Plot and Save the Results
# --------------------------------------------------------------------------
# Using the ggplot2 package is the modern standard for plotting in R.
# If you don't have it, run: install.packages("ggplot2")
library(ggplot2)

# Filter the data frame for plotting (years <= 2000)
plot_data <- pop_growth_df[pop_growth_df$year <= 2000, ]

# Create the plot object
pop_growth_plot <- ggplot(data = plot_data, aes(x = year)) +
  geom_line(aes(y = population_r1, color = "r = 0.0005"), linewidth = 1.2) +
  geom_line(aes(y = population_r2, color = "r = 0.0010"), linewidth = 1.2) +
  labs(
    title = "Population Growth Models",
    x = "Year",
    y = "Population Size",
    color = "Growth Rate" # This renames the legend title
  ) +
  scale_color_manual(values = c("r = 0.0005" = "blue", "r = 0.0010" = "gold")) +
  theme_minimal(base_size = 14)

# Display the plot in RStudio
print(pop_growth_plot)

# --- Saving the Plot ---
# First, ensure the 'Figures' directory exists to avoid errors.
if (!dir.exists("Figures")) {
  dir.create("Figures")
}

# ggsave is a simpler way to save ggplot plots.
ggsave(filename = "Figures/ggplot_pop_growth_models.png",
       plot = pop_growth_plot,
       width = 8,
       height = 6,
       dpi = 300)