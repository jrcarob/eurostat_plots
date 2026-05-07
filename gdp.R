# ──────────────────────────────────────────────────────────────────[...]
# Replicate Eurostat NUTS2 GDP per capita PPS map in R
# EU=100, same bins & colours as the provided image
# Requires: giscoR, ggplot2, sf, dplyr, readr/stringr, forcats
# Data file: download from Eurostat → nama_10r_2gdp → PPS % of EU27_2020
# File name example: nama_10r_2gdppc__custom_XXXXXX.tsv.gz  (or nama_10r_2gdp.tsv.gz)
# ──────────────────────────────────────────────────────────────────[...]

library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)
library(giscoR)     # high-quality Europe NUTS maps
library(readr)
library(stringr)
library(forcats)
library(countrycode) # optional – helps with country names if needed

# ─── 1. Download NUTS 2021 or 2024 geometry (giscoR uses latest available) ───────
# resolution = 10 for publication quality, 20 or 60 faster
nuts2 <- gisco_get_nuts(
  resolution = "10",          # 1:1M scale
  nuts_level = "2",
  year = "2021",              # NUTS 2021 geometry
  epsg = 3035,                # LAEA – good for Europe
  cache = TRUE
)

# ─── 2. Read Eurostat data (adjust filename) ────────────────────────────────────
# Download → https://ec.europa.eu/eurostat/databrowser/product/view/nama_10r_2gdp
# Choose: geo × unit × time → PPS_HAB_EU27_2020 (or PPS per inhab % EU27_2020 avg)
# Download .tsv.gz (script will automatically use the most recent year available)

file_path <- "nama_10r_2gdpc.tsv.gz"   # ← change to your downloaded file name

# Read with more robust NA handling
raw <- read_tsv(file_path, 
                na = c(":", " ", ""), 
                trim_ws = TRUE,
                show_col_types = FALSE)

# ─── DATA CLEANING PIPELINE ─────────────────────────────────────────────────────
# The Eurostat TSV format has:
# - First column with geo code (e.g., "FR10", "DE11")
# - Subsequent columns with year × unit combinations (e.g., "2024 PPS_HAB_EU27_2020")

# Rename first column for clarity
colnames(raw)[1] <- "geo_time"

# Parse the geo_time column which contains both geo code and metadata
df <- raw |>
  # Extract geo code (2-letter country + 1-2 digit NUTS code)
  mutate(
    geo = str_extract(geo_time, "^[A-Z]{2}[A-Z0-9]{1,2}"),
    .before = 1
  ) |>
  # Remove the original compound column
  select(-geo_time) |>
  # Convert all data columns to character BEFORE pivoting to handle mixed types
  # (some columns may be numeric, others character with missing markers like ":")
  mutate(across(-geo, as.character)) |>
  # Pivot from wide (one column per time period) to long format
  pivot_longer(
    cols = -geo, 
    names_to = "year_unit", 
    values_to = "value"
  ) |>
  # Extract year from column name (finds first 4-digit sequence)
  mutate(
    year = str_extract(year_unit, "\\d{4}"),
    unit = str_remove(year_unit, "\\d{4}"),
    # Clean up whitespace in unit column
    unit = str_trim(unit)
  ) |>
  # Convert year to numeric for comparison
  mutate(year_numeric = as.numeric(year)) |>
  # Filter for the most recent year available in the dataset
  filter(year_numeric == max(year_numeric, na.rm = TRUE)) |>
  # Remove missing values and invalid entries
  filter(
    !is.na(value),
    value != ":",
    value != "",
    !is.na(year_numeric)
  ) |>
  # Convert value to numeric
  mutate(value = as.numeric(value)) |>
  # Keep only geo and value columns
  select(geo, value) |>
  # Remove rows where geo extraction failed (rows with country totals or invalid codes)
  filter(!is.na(geo), str_length(geo) >= 3, !is.na(value)) |>
  # Remove duplicates if any
  distinct(geo, .keep_all = TRUE)

# Rename value column to match map legend
df <- df |> rename(pps_eu100 = value)

# Quick check
max_year <- max(as.numeric(str_extract(raw |> 
  mutate(
    geo = str_extract(geo_time, "^[A-Z]{2}[A-Z0-9]{1,2}"),
    .before = 1
  ) |>
  select(-geo_time) |>
  mutate(across(-geo, as.character)) |>
  pivot_longer(cols = -geo, names_to = "year_unit", values_to = "value") |>
  pull(year_unit), "\\d{4}")), na.rm = TRUE)

cat("Data loaded successfully!\n")
cat("Most recent year in dataset:", max_year, "\n")
cat("Number of NUTS2 regions with data:", nrow(df), "\n")
cat("Summary of PPS values:\n")
print(summary(df$pps_eu100))

# Verify join will work
cat("\nSample of geo codes in data:", head(df$geo, 10), "\n")
cat("Sample of NUTS_ID in geometry:", head(nuts2$NUTS_ID, 10), "\n")

# ─── 3. Define the same bins & colours as the original map ───────────────────────
# From legend:
# ≥125        dark blue
# 100–<125    light blue
# 75–<100     very light pink/purple
# 50–<75      medium purple
# <50         dark purple
# NA          grey

breaks <- c(-Inf, 50, 75, 100, 125, Inf)
labels <- c("< 50", "50 – <75", "75 – <100", "100 – <125", "≥ 125")

pal <- c(
  "< 50"       = "#4a1486",   # dark purple
  "50 – <75"   = "#7b1fa2",   # medium purple
  "75 – <100"  = "#ba68c8",   # light pink-purple
  "100 – <125" = "#90caf9",   # light blue
  "≥ 125"      = "#0d47a1",   # dark blue
  "Data not available" = "grey75"  # grey for missing
)

# ─── 4. Classify data into bins ──────────────────────────────────────────────────
df <- df |>
  mutate(
    cat = cut(
      pps_eu100,
      breaks = breaks,
      labels = labels,
      right = FALSE,
      include.lowest = TRUE
    )
  )

# ─── 5. Join data to geometry and handle missing values ────────────────────────────
nuts2 <- nuts2 |>
  left_join(df, by = c("NUTS_ID" = "geo")) |>
  # Replace NA categories with "Data not available" as a string first
  mutate(
    cat = if_else(is.na(cat), "Data not available", as.character(cat)),
    # Convert to factor with all levels including "Data not available"
    cat = factor(cat, levels = names(pal))
  )

# ─── 6. Plot ────────────────────────────────────────────────────────────[...]
ggplot() +
  # background (sea-ish)
  geom_sf(
    data = gisco_get_countries(resolution = "10", epsg = 3035),
    fill = "#e0f7fa", 
    color = NA
  ) +   # very light cyan
  # main choropleth
  geom_sf(
    data = nuts2,
    aes(fill = cat),
    color = "white", 
    linewidth = 0.12
  ) +
  scale_fill_manual(
    values = pal,
    name = "PPS (EU=100)",
    drop = FALSE
  ) +
  coord_sf(
    xlim = c(1000000, 6500000), 
    ylim = c(1000000, 5500000),
    expand = FALSE
  ) +   # crop roughly to Europe
  theme_void(base_size = 11) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 1),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = paste("GDP per capita by NUTS 2 regions,", max_year),
    subtitle = "(in purchasing power standards (PPS), EU=100)",
    caption = "Source: Eurostat – nama_10r_2gdp • Cartography: giscoR / EuroGeographics © • Reproduction with kind permission"
  )

# Optional: save high-res
# ggsave(paste0("gdp_per_capita_nuts2_", max_year, "_pps.png"), width = 12, height = 10, dpi = 320)
