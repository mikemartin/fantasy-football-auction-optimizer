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

# --- Basis check: are all sources counting the same games? ------------------------------
#
# Sources do not agree on what a "season" projection covers. Before week 1 they all publish
# full-season totals. Once games are played, some switch to remaining games only (FanDuel
# announces this in its scrape message) while others keep publishing full-season totals that
# already include the weeks behind us. Averaging the two bases together silently deflates
# every player who happens to be covered by the remaining-games source, and the deflation
# grows every week - so it is measured here rather than left to corrupt the sheet.
#
# The measurement: score each source on its own, take the median over players that EVERY
# source covers, and compare. A source on a remaining-games basis reads low by roughly the
# share of the season already played; genuine disagreement between sources sits under a few
# percent. The lowest-scale cluster is treated as the reference, because mid-season decisions
# want remaining-season value, and the fuller sources are scaled down to match it.
source("R/scoring.R")

# A true basis gap is large and predictable: with w of 17 weeks already played, a
# full-season source reads (17-w)/17 below a remaining-games one - about 12% by week 2,
# and growing. Ordinary forecaster optimism sits well inside that; ESPN and FantasyPros
# have run ~7% high all season without ever being on a different basis. So the gap is
# only called when a source sits closer to the full-season ratio than to parity.
#
# Detection always runs and always reports. RESCALING DOES NOT RUN BY DEFAULT: the
# evidence separating "different basis" from "more optimistic forecaster" is thin, and
# quietly rewriting one source's numbers to match another is exactly the kind of hidden
# transformation this pipeline is meant not to make. Set PROJECTION_BASIS_RESCALE=1 to
# apply it, having read data/source_basis_<season>.csv and decided it is warranted.
weeks_played <- tryCatch(max(0, as.integer(ffanalytics:::get_scrape_week()) - 1),
                         error = function(e) 0)
full_season_ratio <- (17 - weeks_played) / 17
scale_tolerance <- (1 + full_season_ratio) / 2
do_rescale <- identical(Sys.getenv("PROJECTION_BASIS_RESCALE"), "1")
message(sprintf("Basis check: %d week(s) played, a full-season source would read %.3f; ",
                weeks_played, full_season_ratio),
        sprintf("flagging any source below %.3f. Rescaling is %s.",
                scale_tolerance, ifelse(do_rescale, "ON", "OFF (report only)")))

src_pts <- lapply(split(all_pos, all_pos$data_src), function(d) {
  d <- as_tibble(d) %>% filter(pos %in% c("QB", "RB", "WR", "TE"))
  pts <- tryCatch(suppressWarnings(score_players(d, league))$points,
                  error = function(e) rep(NA_real_, nrow(d)))
  tibble(id = d$id, pts = pts)
})
common_ids <- Reduce(intersect, lapply(src_pts, function(x) x$id[is.finite(x$pts) & x$pts > 0]))

if (length(common_ids) < 20) {
  warning("Only ", length(common_ids), " players are covered by every source, too few to ",
          "check whether the sources count the same games. Projections are averaged as-is; ",
          "treat cross-source comparisons with caution.")
  scale_factor <- setNames(rep(1, length(src_pts)), names(src_pts))
  src_median <- setNames(rep(NA_real_, length(src_pts)), names(src_pts))
} else {
  src_median <- vapply(src_pts, function(x) median(x$pts[x$id %in% common_ids]), numeric(1))
  # Scale everyone to the SHORTEST basis: that is the remaining-season view we actually want.
  scale_factor <- min(src_median) / src_median
}

basis_gap <- any(scale_factor < scale_tolerance)
# `flagged` means the source sits far enough below the shortest basis to look like a
# different one; `rescale_applied` says whether anything was actually done about it. The
# two are not the same, and the file is the audit trail, so it must not imply otherwise.
src_report <- tibble(
  data_src = names(src_median),
  median_pts = round(unname(src_median), 1),
  scale_factor = round(unname(scale_factor), 3),
  flagged = unname(scale_factor) < scale_tolerance,
  rescale_applied = do_rescale & unname(scale_factor) < scale_tolerance
)
message("Source basis check (median points over ", length(common_ids), " common players):")
print(as.data.frame(src_report), row.names = FALSE)
write.csv(src_report, sprintf("data/source_basis_%d.csv", league$season), row.names = FALSE)

if (basis_gap && !do_rescale) {
  warning("Sources may not be counting the same games: ",
          paste(sprintf("%s=%.0f", src_report$data_src, src_report$median_pts), collapse = ", "),
          ". Flagged but NOT corrected - the averaged sheet still mixes them. Review ",
          "data/source_basis_", league$season, ".csv and re-run with ",
          "PROJECTION_BASIS_RESCALE=1 if the gap is a real basis difference.")
}

if (basis_gap && do_rescale) {
  warning("Sources disagree on how many games a season projection covers: ",
          paste(sprintf("%s=%.0f", src_report$data_src, src_report$median_pts), collapse = ", "),
          ". The fuller sources have been scaled down to the shortest basis so the average is ",
          "on one footing. See data/source_basis_", league$season, ".csv.")

  # Scale counting stats only. Rates and per-game columns describe a game, not a season, so
  # multiplying them would corrupt stats that are already on a common footing.
  count_cols <- setdiff(
    names(all_pos)[vapply(all_pos, is.numeric, logical(1))],
    c("id", "src_id", "games", "bye", "site_pts", "site_fppg",
      grep("_avg$|_rate$|_g$|_pct$", names(all_pos), value = TRUE))
  )
  all_pos <- all_pos %>%
    mutate(across(all_of(count_cols), \(x) x * unname(scale_factor[data_src])))
  message("Rescaled ", length(count_cols), " counting-stat columns for ",
          sum(src_report$rescale_applied), " of ", nrow(src_report), " sources.")
} else if (!basis_gap) {
  message("All sources are on the same basis; no rescaling needed.")
}

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
  # Some sources label a handful of players FB or other off-positions; the league
  # models QB/RB/WR/TE only, and fullbacks project near zero points anyway.
  filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
  relocate(player, team, pos, .after = id)

# Stamp when this scrape ran, so the value sheet can show its own age.
projections$scraped_at <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")

out_path <- sprintf("data/projections_raw_%d.csv", league$season)
write.csv(projections, out_path, row.names = FALSE)
message("Wrote ", nrow(projections), " players to ", out_path)
