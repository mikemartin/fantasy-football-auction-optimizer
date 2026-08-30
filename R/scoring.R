# -------------------------------------------------------------------------------------------
# Scoring
#
# Turns a table of raw projected stats (one row per player) into projected fantasy points
# under the rules in R/league_config.R. Expects the columns written by
# scripts/01_scrape_projections.R; refuses to run if a stat the rules need is missing,
# rather than silently scoring it as zero.
# -------------------------------------------------------------------------------------------

library(dplyr)

# Stats the scoring rules require. `fumbles_lost`, `rec`, and two-point conversions are
# deliberately absent: this league awards no points for any of them.
required_stats <- c(
  "pass_yds", "pass_tds", "pass_int",
  "rush_yds", "rush_tds",
  "rec_yds", "rec_tds"
)

score_players <- function(stats, league) {
  missing_cols <- setdiff(required_stats, names(stats))
  if (length(missing_cols) > 0) {
    stop(
      "Projection data is missing stats the scoring rules need: ",
      paste(missing_cols, collapse = ", "),
      ". Re-run scripts/01_scrape_projections.R or check the scrape sources."
    )
  }

  # A missing value for a stat a player never accrues (a WR's passing yards) is a true
  # zero. A position-wide gap would be a data problem, so report those before coalescing.
  for (col in c("pass_yds", "pass_tds", "pass_int")) {
    qb_missing <- sum(is.na(stats[[col]][stats$pos == "QB"]))
    if (qb_missing > 0) {
      warning(qb_missing, " QBs have no projected ", col, "; they will score 0 for it.")
    }
  }
  for (col in c("rec_yds", "rec_tds")) {
    skill_missing <- sum(is.na(stats[[col]][stats$pos %in% c("RB", "WR", "TE")]))
    if (skill_missing > 0) {
      warning(skill_missing, " RB/WR/TEs have no projected ", col,
              "; they will score 0 for it.")
    }
  }

  s <- league$scoring

  # Effective per-interception penalty: the flat INT points plus the expected pick-six
  # penalty (pick_six_rate of INTs are returned for a TD, each costing pick_six more).
  int_pts <- s$pass_int + league$pick_six_rate * s$pick_six

  # Two-point conversions: only some sources project them, either split by play type
  # (pass_two_pts / rush_two_pts / rec_two_pts) or combined (two_pts). All types score
  # the same here, so take the larger of the two estimates per player. If no source
  # supplied any of these columns, say so - never silently score them as zero.
  two_pt_cols <- intersect(c("pass_two_pts", "rush_two_pts", "rec_two_pts"), names(stats))
  has_combined <- "two_pts" %in% names(stats)
  if (length(two_pt_cols) == 0 && !has_combined) {
    warning(
      "No projection source supplied two-point conversion projections ",
      "(two_pts / pass_two_pts / rush_two_pts / rec_two_pts); the ", s$two_pt,
      "-point rule cannot be applied and 2PCs will contribute 0 points."
    )
    stats$two_pt_proj <- 0
  } else {
    split_sum <- if (length(two_pt_cols) > 0) {
      rowSums(as.matrix(stats[, two_pt_cols, drop = FALSE]), na.rm = TRUE)
    } else 0
    combined <- if (has_combined) coalesce(stats$two_pts, 0) else 0
    stats$two_pt_proj <- pmax(split_sum, combined)
  }

  stats %>%
    mutate(
      # First downs are not published by any projection source; derive them from
      # projected yards at FTN regression rates (per-position for receiving, a single
      # 0.0508/yd rate for rushing).
      rec_first_downs = coalesce(rec_yds, 0) * unname(league$rec_fd_rates[pos]),
      rush_first_downs = coalesce(rush_yds, 0) * league$rush_fd_rate,
      points =
        coalesce(pass_tds, 0)  * s$pass_td +
        coalesce(pass_int, 0)  * int_pts +
        coalesce(pass_yds, 0)  * s$pass_yd +
        coalesce(rush_yds, 0)  * s$rush_yd +
        coalesce(rush_tds, 0)  * s$rush_td +
        coalesce(rec_yds, 0)   * s$rec_yd +
        coalesce(rec_tds, 0)   * s$rec_td +
        rec_first_downs        * s$rec_first_down +
        rush_first_downs       * s$rush_first_down +
        two_pt_proj            * s$two_pt,
      points = round(points, 1)
    )
}
