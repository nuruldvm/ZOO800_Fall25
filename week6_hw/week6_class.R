#Zoo800_class_excercise
#comparing flipper length by sex and species 
#Nurul Islam
#class assignment (exercise)
#date: 
#required packages for the class excercise 

#install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
summary(penguins)


# First lets create a boxplot comparing male vs female flipper length for each species of penguins
#using ggplot

flipper_plot <- ggplot(data = penguins, 
                       aes(x = species, 
                           y = flipper_length_mm, 
                           fill = sex)) +
  geom_boxplot() +
  labs(title = "Flipper Length Comparison by Sex Across Penguin Species",
       x = "Penguin Species",
       y = "Flipper Length (mm)",
       fill = "Sex") +
  theme_bw(base_size = 14)

flipper_plot

# mm I am seeing NA in both Gentoo and Adelie speices which I dont want to see. I will drop the NA 
#from the datasets
# i wanna try to violin plot, which is cool looking

library(tidyverse)
library(palmerpenguins)

# Remove penguins with unknown sex (optional, avoids gray "NA" legend)
penguins_clean <- subset(penguins, !is.na(sex))

# Create violin plot
flipper_violin <- ggplot(data = penguins_clean,
                         aes(x = species,
                             y = flipper_length_mm,
                             fill = sex)) +
  geom_violin(trim = FALSE, alpha = 0.8) +                # main violin shape
  geom_boxplot(width = 0.15, position = position_dodge(0.9),
               fill = "white", color = "black") +         # overlay boxplot for medians
  labs(title = "Distribution of Flipper Length by Sex Across Penguin Species",
       x = "Penguin Species",
       y = "Flipper Length (mm)",
       fill = "Sex") +
  theme_bw(base_size = 14) +                              # clean theme for publication
  scale_fill_brewer(palette = "Set2")                     # color palette for clarity

# Display
flipper_violin

# Save high-resolution PNG
ggsave("penguin_flipper_violin.png", flipper_violin,
       width = 7, height = 5, dpi = 300)

