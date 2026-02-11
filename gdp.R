# ────────────────────────────────────────────────────────────────────────
# Replicate Eurostat NUTS2 GDP per capita PPS map (2024) in R
# EU=100, same bins & colours as the provided image
# Requires: giscoR, ggplot2, sf, dplyr, readr/stringr
# Data file: download from Eurostat → nama_10r_2gdp → filter 2024 → PPS % of EU27_2020
# File name example: nama_10r_2gdppc__custom_XXXXXX.tsv.gz  (or nama_10r_2gdp.tsv.gz)
# ────────────────────────────────────────────────────────────────────────

library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)
library(giscoR)     # high-quality Europe NUTS maps
library(readr)
library(stringr)
library(countrycode) # optional – helps with country names if needed

# ─── 1. Download NUTS 2021 or 2024 geometry (giscoR uses latest available) ───────
# resolution = 10 for publication quality, 20 or 60 faster
nuts2 <- gisco_get_nuts(
  resolution = "10",          # 1:1M scale
  nuts_level = "2",
  year = "2021",              # 2024 data usually still uses NUTS 2021 geometry
  epsg = 3035,                # LAEA – good for Europe
  cache = TRUE
)

# ─── 2. Read Eurostat data (adjust filename) ────────────────────────────────────
# Download → https://ec.europa.eu/eurostat/databrowser/product/view/nama_10r_2gdp
# Choose: geo × unit × time → PPS_HAB_EU27_2020 (or PPS per inhab % EU27_2020 avg)
# Filter year 2024 → download .tsv.gz

file_path <- "nama_10r_2gdpc.tsv.gz"   # ← change to your downloaded file name

raw <- read_tsv(file_path, na = c(":", ":", " ", ""), trim_ws = TRUE,
                show_col_types = FALSE)

# Clean column names & pivot (Eurostat TSV has ugly first column)
colnames(raw)[1] <- "geo_time"

df <- raw |>
  mutate(geo = str_extract(geo_time, "^[A-Z]{2}[A-Z0-9]{1,2}$"),
         .before = 1) |>
  select(-geo_time) |>
  pivot_longer(cols = -geo, names_to = "year_unit", values_to = "value") |>
  mutate(year = str_extract(year_unit, "\\d{4}"),
         unit = str_remove(year_unit, "\\d{4}")) |>
  filter(year == "2024", !is.na(value), value != ":") |>
  mutate(value = as.numeric(value)) |>
  select(geo, value) |>
  filter(str_length(geo) >= 3)          # remove country totals if present

# Rename value column to match map legend
df <- df |> rename(pps_eu100 = value)

# Quick check
summary(df$pps_eu100)

# ─── 3. Define the same bins & colours as the original map ───────────────────────
# From legend:
# ≥125        dark blue
# 105–<125    light blue
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
  "≥ 125"      = "#0d47a1"    # dark blue
)

# Classify
df <- df |>
  mutate(
    cat = cut(pps_eu100,
              breaks = breaks,
              labels = labels,
              right = FALSE,
              include.lowest = TRUE),
    cat = forcats::fct_rev(cat)   # so legend goes high → low
  )

# ─── 4. Join data to geometry ───────────────────────────────────────────────────
nuts2 <- nuts2 |>
  left_join(df, by = c("NUTS_ID" = "geo")) |>
  mutate(cat = replace(cat, is.na(cat), "Data not available"))

# Add "Data not available" to palette
pal["Data not available"] <- "grey75"

# ─── 5. Plot – try to match style closely ───────────────────────────────────────
ggplot() +
  # background (sea-ish)
  geom_sf(data = gisco_get_countries(resolution = "10", epsg = 3035),
          fill = "#e0f7fa", color = NA) +   # very light cyan
  # main choropleth
  geom_sf(data = nuts2,
          aes(fill = cat),
          color = "white", linewidth = 0.12) +
  scale_fill_manual(values = pal,
                    name = "PPS (EU=100)",
                    drop = FALSE,
                    guide = guide_legend(reverse = TRUE)) +
  coord_sf(xlim = c(1000000, 6500000), ylim = c(1000000, 5500000),
           expand = FALSE) +   # crop roughly to Europe
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
    title = "GDP per capita by NUTS 2 regions, 2024",
    subtitle = "(in purchasing power standards (PPS), EU=100)",
    caption = "Source: Eurostat – nama_10r_2gdp (2024 data) • Cartography: giscoR / EuroGeographics © • Reproduction with kind permission"
  )

# Optional: save high-res
# ggsave("gdp_per_capita_nuts2_2024_pps.png", width = 12, height = 10, dpi = 320)