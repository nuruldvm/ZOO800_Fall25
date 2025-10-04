packages <- c("here", "writexl", "readxl")

for(pkg in packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
# Read files using robust, reproducible paths
fish_rds    <- readRDS(here("week5_hw", "data", "fish.rds"))
fish_csv    <- read.csv(here("week5_hw", "data", "fish.csv"))
fish_xlsx   <- read_excel(here("week5_hw", "data", "fish.xlsx"))

# Show the first 5 rows of each
head(fish_csv, 5)
head(fish_xlsx, 5)
head(fish_rds, 5)
# Save as CSV
write.csv(fish_csv, "Output/fish.csv", row.names = FALSE)

# Save as Excel
library(writexl)
write_xlsx(fish_csv, "Output/fish.xlsx")

# Save as RDS
saveRDS(fish_csv, "Output/fish.rds")
