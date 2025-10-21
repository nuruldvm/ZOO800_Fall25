# ZOO 800 HW7: Study Site Map (Dane, Grant, Iowa Counties, WI)
# Nurul Islam
# date: system date

# =======================
# 0) Packages & Setup
# =======================
library(tidyverse)   # ggplot2, readr, dplyr, stringr, tidyr (all loaded)
library(maps)        # map data backend
library(patchwork)   # inset_element + layout
library(here)        # stable paths
library(ggrepel)     # collision-free labels

# Define study counties (lowercase required for map_data)
STUDY_COUNTIES_FULL <- c("iowa", "grant", "dane") 

# =======================
# 1) Data (Capture Sites)
# =======================

# CORRECT PATH: This uses the 'here' package to reliably find the file
# located in the 'week7_hw' subdirectory of your project root.
data_path <- here::here("week7_hw", "deer_capture_locations.csv")
deer_sites_raw <- read_csv(data_path, show_col_types = FALSE)

# Standardize names and select coords
deer_sites <- deer_sites_raw %>%
  rename_with(tolower) %>% # 'Lat' -> 'lat', 'Long' -> 'long'
  select(lat, long) %>%
  drop_na(lat, long) %>%
  mutate(site_type = "Capture Site")

# =======================
# 2) Basemaps & Labels
# =======================
us_map <- map_data("state")
wisconsin_map <- us_map %>% filter(region == "wisconsin")

wi_county_map <- map_data("county", region = "wisconsin") %>%
  mutate(subregion = tolower(subregion)) %>%
  mutate(is_study_county = subregion %in% STUDY_COUNTIES_FULL)

# Label positions (bbox midpoints) + nice text
county_labels <- wi_county_map %>%
  filter(is_study_county) %>%
  group_by(subregion) %>%
  # Use the standard 'linewidth' aesthetic instead of the older 'size' for lines
  summarise(
    long = mean(range(long), na.rm = TRUE),
    lat  = mean(range(lat),  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = str_to_title(subregion)) %>%
  # --- FIX: Nudge Iowa County label slightly to prevent point collision ---
  # Increased nudge (Nudge West and slightly North) for better visibility
  mutate(
    long = case_when(
      subregion == "iowa" ~ long - 0.15, # Further Nudge west
      TRUE ~ long
    ),
    lat = case_when(
      subregion == "iowa" ~ lat + 0.05, # Nudge North
      TRUE ~ lat
    )
  )
# -------------------------------------------------------------------------

# =======================
# 3) Map B: Wisconsin detail
# =======================
map_b <- ggplot(wi_county_map, aes(long, lat)) +
  
  # 1) All counties (background)
  geom_polygon(aes(group = group), fill = "gray95", color = "gray70", 
               linewidth = 0.2) +
  
  # 2) Study counties (highlighted)
  geom_polygon(data = wi_county_map %>% filter(is_study_county),
               aes(group = group),
               fill = "#0072B2", color = "black", 
               linewidth = 0.6) +
  
  # 3) County labels (using ggrepel for collision-free placement)
  geom_label_repel(
    data = county_labels,
    mapping = aes(x = long, y = lat, label = label),
    inherit.aes = FALSE,
    size = 4, fontface = "bold", # Increased size slightly for visibility
    color = "white", fill = "#005b96",
    label.size = 0,
    seed = 42,
    min.segment.length = Inf, # No leader lines
    box.padding = 0.15,
    point.padding = 0.15,
    max.overlaps = Inf
  ) +
  
  # 4) POINTS ON TOP (bordered for visibility)
  geom_point(
    data = deer_sites,
    mapping = aes(x = long, y = lat),
    inherit.aes = FALSE,
    shape = 21, fill = "gold", color = "black", # High visibility gold point with black border
    size = 3.2, stroke = 0.35
  ) +
  
  # 5) State border (final outline)
  geom_polygon(data = wisconsin_map, aes(group = group),
               fill = NA, color = "black", 
               linewidth = 1.0) +
  
  # 6) Final Theme
  coord_fixed(ratio = 1.3) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

# =======================
# 4) Map A: US inset
# =======================
map_a <- ggplot() +
  # US Base Map (Gray)
  geom_polygon(data = us_map, aes(long, lat, group = group),
               fill = "gray90", color = "gray60",
               linewidth = 0.2) +
  # Highlight Wisconsin (Red)
  geom_polygon(data = wisconsin_map, aes(long, lat, group = group),
               fill = "red", color = "black") +
  
  # Final Theme
  coord_fixed(ratio = 1.3, xlim = c(-125, -70), ylim = c(25, 50), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "aliceblue", color = NA),
    plot.margin = margin(1, 1, 1, 1, "mm"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

# =======================
# 5) Compose with inset & Save
# =======================
final_map_figure <- map_b +
  inset_element(map_a, left = 0.70, right = 0.98, bottom = 0.70, top = 0.98, align_to = "panel")

print(final_map_figure)

# Save output
out_png <- here::here("deer_study_sites_final.png")
ggsave(filename = out_png, plot = final_map_figure, width = 10, height = 8, dpi = 300)
message("Saved: ", out_png)