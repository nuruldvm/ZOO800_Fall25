#########################################################
# Zoology 800: Quantitative Methods in Biology
# Assignment: Week 5 - Data Importing, Wrangling, and Saving in R
# Name: Md Nurul Islam
# Date: October 4, 2025
#########################################################

#-------------------#
# Load/Install Packages
#-------------------#
#i tried to design a loop/ conditional chunk for ensuring if the package install it will load
#from the library; if not the code will install the packages. I search the google and substack
#a whole discussion on this. its cool to ensure the code portablity 

packages <- c("here", "writexl", "readxl", "dplyr", "ggplot2")

for(pkg in packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#-------------------#
# Ensure Output Folder Exists
#-------------------#
if(!dir.exists("Output")) dir.create("Output")


#-------------------#
# Problem 1: Import Datasets
#-------------------#
# Using robust project paths
fish_rds  <- readRDS(here("week5_hw", "data", "fish.rds"))
fish_csv  <- read.csv(here("week5_hw", "data", "fish.csv"))
fish_xlsx <- read_excel(here("week5_hw", "data", "fish.xlsx"))

# Show first five rows of each (prints in console)
head(fish_csv, 5)
head(fish_xlsx, 5)
head(fish_rds, 5)

#-------------------#
# Problem 2: Save One Dataset in Multiple Formats
#-------------------#
# Save fish_csv as CSV, XLSX, and RDS in Output folder
write.csv(fish_csv, "Output/fish.csv", row.names = FALSE)
write_xlsx(fish_csv, "Output/fish.xlsx")
saveRDS(fish_csv, "Output/fish.rds")

#-------------------#
# Problem 2: File Size Comparison
#-------------------#
# Compare file sizes in MB using file.info()
files <- c("Output/fish.csv", "Output/fish.xlsx", "Output/fish.rds")
sizes_bytes <- file.info(files)$size
sizes_mb <- sizes_bytes / (1024^2)

# Create a nice table
file_sizes <- data.frame(
  File = basename(files),
  Size_MB = round(sizes_mb, 4)
)
print(file_sizes)

# Based on the file size comparison:
# - The RDS file (fish.rds) is the most compact (0.0029 MB).
# - RDS is best for storage and use in R.
# - The CSV file (fish.csv) is a bit larger (0.0113 MB) but is widely compatible for sharing.
# - The XLSX file (fish.xlsx) is the largest (0.0132 MB) and is ideal for Excel workflows.
# In summary, RDS for compactness, CSV for universality, XLSX for spreadsheets.

#-------------------#
# Problem 3 – Wrangling Pipeline with dplyr
#-------------------#

library(dplyr)
library(ggplot2)

# End-to-end wrangling: filter, create, and summarise
fish_output <- fish_csv %>%
  filter(Species %in% c("Walleye", "Yellow Perch", "Smallmouth Bass"),
         Lake %in% c("Erie", "Michigan")) %>%
  select(Species, Lake, Year, Length_cm, Weight_g) %>%
  mutate(
    Length_mm = Length_cm * 10,
    Length_group = cut(Length_mm,
                       breaks = c(-Inf, 200, 400, 600, Inf),
                       labels = c("<=200", "200-400", "400-600", ">600"))
  )

# Count by species and length group
length_counts <- fish_output %>%
  count(Species, Length_group)

print(length_counts)

# Group by species × year, then summarise
summary_output <- fish_output %>%
  group_by(Species, Year) %>%
  summarise(
    mean_weight = mean(Weight_g, na.rm = TRUE),
    median_weight = median(Weight_g, na.rm = TRUE),
    sample_size = n(),
    .groups = "drop"
  )

print(summary_output)

# Optional quick plot
ggplot(summary_output, aes(x = Year, y = mean_weight, color = Species)) +
  geom_line() +
  labs(title = "Temporal Change in Mean Weight for Each Species",
       x = "Year", y = "Mean Weight (g)")

# Export result
write.csv(summary_output, "Output/fish_summary.csv", row.names = FALSE)

#######
# problem 4: multifile
#####
git clone https://github.com/xucamel/Quantitative_course_week5.git

