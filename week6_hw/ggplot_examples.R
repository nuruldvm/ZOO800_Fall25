#Playing with ggplot2
#Olaf Jensen
#October 6, 2025
#ZOO 800

#load packages
library(palmerpenguins)
library(tidyverse)

#examine the penguins data set
summary(penguins)

#specify the data, aesthestics, and geometry of a simple plot of body_mass_g vs. flipper_length_mm
plot1 <- ggplot(data=penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
          geom_point()

plot1

#examine the ggplot object
plot1$data[1:4, ]

plot1$mapping

plot1$geom

plot1$layers

#add a variable to distinguish the different species by color
plot2 <- ggplot(data=penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point()

plot2

#use facet wrapping instead
plot3 <- plot2 + facet_wrap(~species, ncol = 3)

plot3

#add trend lines
plot4 <- plot3 + geom_smooth(method = "lm")

plot4

#explore other geometries
plot5 <- ggplot(data=penguins, mapping = aes(x = species, y = body_mass_g)) +
  geom_boxplot()

plot5

plot6 <- ggplot(data=penguins, mapping = aes(x = species, y = body_mass_g)) +
  geom_violin() + geom_jitter()

plot6

#explore other themes
plot7 <- plot5 + theme_bw()

plot7

#Change the plotting order of species: Gentoo, Adelie, Chinstrap
penguins$species <- factor(penguins$species, levels = c("Gentoo", "Adelie", "Chinstrap"))

plot8 <- ggplot(data=penguins, mapping = aes(x = species, y = body_mass_g)) +
  geom_boxplot() + theme_bw()

plot8

#Change the color palette
plot9 <- plot2 + scale_color_brewer(palette = "Set2")

plot9

