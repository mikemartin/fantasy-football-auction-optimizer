# -------------------------------------------------------------------------------------------
# Step 1: Scrape season projections with ffanalytics
#
# Scrapes QB/RB/WR/TE season projections from all available sources, averages the raw
# stat columns across sources, and writes them to data/projections_raw_<season>.csv.
#
# The RAW stats are saved (not fantasy points) so scoring rules can change without a
# re-scrape. K and DEF are never scraped: the league treats them as $1 players.
#
# Run from the repo root:  Rscript scripts/01_scrape_projections.R
# -------------------------------------------------------------------------------------------

library(ffanalytics)
library(dplyr)
library(tidyr)

source("R/league_config.R")

scrape <- scrape_data(
  pos = c("QB", "RB", "WR", "TE"),
  season = league$season,
  week = 0
)

all_pos <- bind_rows(lapply(scrape, as_tibble))

if (nrow(all_pos) == 0 || !"data_src" %in% names(all_pos)) {
  stop(
    "No projection source returned any data. Every scrape failed - most likely the ",
    "network is blocking the projection sites (run this script somewhere with open ",
    "internet access), or every source changed its page format."
  )
}

# Report which sources responded and which stats each carries, so a gap in a stat the
# scoring needs is visible instead of silently averaged around.
src_summary <- all_pos %>%
  group_by(data_src) %>%
  summarise(
    players = n_distinct(id),
    has_pass_yds = "pass_yds" %in% names(all_pos) && any(!is.na(pass_yds)),
    has_rec_yds = "rec_yds" %in% names(all_pos) && any(!is.na(rec_yds)),
    .groups = "drop"
  )
message("Sources scraped:")
print(src_summary)

# One identity row per player: name/team/pos from the first source that lists them.
identities <- all_pos %>%
  filter(!is.na(player)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, player, team, pos)

# Average every numeric stat column across sources. Site-computed points and metadata
# are dropped; scoring is done under league rules in step 2.
non_stat_cols <- c(
  "id", "player", "team", "pos", "data_src", "src_id",
  "games", "bye", "site_pts", "site_fppg"
)

projections <- all_pos %>%
  select(-any_of(setdiff(non_stat_cols, "id"))) %>%
  group_by(id) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(where(is.numeric), \(x) ifelse(is.nan(x), NA_real_, x))) %>%
  inner_join(identities, by = "id") %>%
  relocate(player, team, pos, .after = id)

out_path <- sprintf("data/projections_raw_%d.csv", league$season)
write.csv(projections, out_path, row.names = FALSE)
message("Wrote ", nrow(projections), " players to ", out_path)
