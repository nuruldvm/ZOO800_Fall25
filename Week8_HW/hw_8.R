# =======================================================================
# Quantitative Methods in Ecology & Evolution — Homework 8
# Title: Remote query to GBIF + Bias-aware EDA for Odocoileus virginianus
# Authors: Nurul Islam 
# Date: 2025-10-28
#
# Objectives
# 1) Remotely query GBIF for White-tailed Deer in Wisconsin.
# 2) Perform bias-aware EDA:
#    - Temporal imbalance (histogram by year)
#    - Spatial imbalance (top counties table)
#    - Data-quality outliers (positional uncertainty)
#
# =======================================================================
  library(rgbif)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(stringr)
  library(purrr)
# -------------------------------
# 0) Parameters for the gbif query
# -------------------------------
sp_name    <- "Odocoileus virginianus"
country    <- "US"
state_name <- "Wisconsin"

year_min   <- 1800
year_max   <- 2025

# Homework-scale cap: we don’t need the full universe
cap_n      <- 100000
page_size  <- 10000   # rgbif supports paging

# -------------------------------
# 1) Resolve taxonomy → taxonKey
# -------------------------------
bb <- name_backbone(name = sp_name)
stopifnot(!is.null(bb$usageKey))
taxon_key <- bb$usageKey

# -------------------------------
# 2) Probe, then paged retrieval
# -------------------------------
probe <- occ_search(
  taxonKey = taxon_key,
  country  = country,
  stateProvince = state_name,
  hasCoordinate = TRUE,
  hasGeospatialIssue = FALSE,
  year = paste0(year_min, ",", year_max),
  limit = 0
)

total_available <- probe$meta$count %||% 0L
if (total_available == 0) {
  stop("No matching records on GBIF after filters. Check species/state/year window.")
}

to_get <- min(total_available, cap_n)
starts <- seq(0, to_get - 1, by = page_size)

message(sprintf("GBIF has %s records (after server-side filters). Targeting %s.",
                format(total_available, big.mark = ","),
                format(to_get, big.mark = ",")))

pull_page <- function(start_idx) {
  occ_search(
    taxonKey = taxon_key,
    country  = country,
    stateProvince = state_name,
    hasCoordinate = TRUE,
    hasGeospatialIssue = FALSE,
    year = paste0(year_min, ",", year_max),
    limit = min(page_size, to_get - start_idx),
    start = start_idx
  )$data
}

# Safe paging: collect successes, report failures
safepull <- purrr::safely(pull_page, otherwise = NULL)
pages    <- purrr::map(starts, safepull)

errs <- purrr::keep(pages, ~ !is.null(.x$error))
if (length(errs)) {
  message(sprintf("Warning: %d page(s) failed; proceeding with remaining pages.", length(errs)))
}

deer_data_raw <- pages |>
  purrr::map("result") |>
  purrr::compact() |>
  purrr::list_rbind()

if (!nrow(deer_data_raw)) stop("Paging succeeded but assembled 0 rows.")

message(sprintf("Retrieved %s rows.", format(nrow(deer_data_raw), big.mark = ",")))

# -------------------------------
# 3) Light cleaning & core fields
# -------------------------------
deer_data <- deer_data_raw %>%
  select(
    key, scientificName, kingdom, phylum, class, order, family, genus, species,
    countryCode, stateProvince, county,
    year, month, day, eventDate,
    decimalLatitude, decimalLongitude,
    coordinateUncertaintyInMeters
  ) %>%
  # reinforce state filter to avoid odd capitalization variants
  filter(stateProvince %in% c(state_name, toupper(state_name), tolower(state_name)))

# Quick sanity check for empty result post-clean
if (nrow(deer_data) == 0) {
  stop("No rows remain after basic field selection/state reinforcement.")
}

# -------------------------------
# 4) FIGURE 1 — Temporal imbalance
# -------------------------------
deer_data_time <- deer_data %>%
  filter(!is.na(year), year >= year_min, year <= year_max)

year_counts <- deer_data_time %>%
  count(year, name = "n")

y_max   <- if (nrow(year_counts)) max(year_counts$n, na.rm = TRUE) else 10
annot_y <- y_max * 0.92

p1 <- ggplot(deer_data_time, aes(x = year)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Figure 1. White-tailed Deer occurrences in Wisconsin by year",
    x = "Year of observation", y = "Number of records"
  ) +
  geom_vline(xintercept = 1980, linetype = "dashed") +
  annotate("text", x = 1960, y = annot_y, label = "Pre-1980\nmuseum/historical",
           hjust = 0, vjust = 1) +
  annotate("text", x = 2000, y = annot_y, label = "Post-1980\nmodern surge",
           hjust = 0, vjust = 1) +
  theme_minimal(base_size = 12)

print(p1)

# -------------------------------
# 5) TABLE 2 — Spatial imbalance (top counties)
# -------------------------------
county_counts <- deer_data %>%
  filter(!is.na(county), str_trim(county) != "") %>%
  mutate(county = str_to_title(county)) %>%
  count(county, sort = TRUE, name = "Record_Count") %>%
  slice_head(n = 10)

message("Table 2. Top 10 Wisconsin counties by GBIF occurrences:")
print(county_counts)

# -------------------------------
# 6) FIGURE 3 — Positional uncertainty outliers (log10)
# -------------------------------
deer_data_unc <- deer_data %>%
  filter(!is.na(coordinateUncertaintyInMeters),
         coordinateUncertaintyInMeters > 0)

p3 <- ggplot(deer_data_unc, aes(x = 1, y = log10(coordinateUncertaintyInMeters))) +
  geom_boxplot(width = 0.2) +
  labs(
    title = "Figure 3. Distribution of positional uncertainty (log10 meters)",
    x = NULL, y = "log10(Coordinate Uncertainty, m)"
  ) +
  geom_hline(yintercept = 4, linetype = "dashed") +  # 10^4 = 10 km
  annotate("text", x = 1.2, y = 4.15, label = "> 10 km uncertainty", hjust = 0) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

print(p3)

# -------------------------------
# 7) Programmatic diagnostics
# -------------------------------
diag <- list(
  n_rows_total      = nrow(deer_data),
  n_years_present   = n_distinct(deer_data_time$year),
  pct_post2000      = round(mean(deer_data_time$year >= 2000) * 100, 1),
  n_counties        = n_distinct(na.omit(deer_data$county)),
  top_county        = county_counts$county[1] %||% NA_character_,
  top_county_share  = round(((county_counts$Record_Count[1] %||% 0) / nrow(deer_data)) * 100, 1),
  pct_unc_gt10km    = round(mean(deer_data_unc$coordinateUncertaintyInMeters > 1e4) * 100, 1)
)

message("Diagnostics (for write-up):")
print(diag)
# =======================end of the assignment =========================