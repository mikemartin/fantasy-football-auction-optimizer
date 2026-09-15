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

# For stats some sources publish combined and others split by play type: take the
# larger of the combined column and the sum of split columns (both estimate the same
# total, and the source means are computed independently). Warns - never silently
# zeroes - when no source supplied the stat at all.
combined_or_split <- function(stats, combined, split, what) {
  split_cols <- intersect(split, names(stats))
  has_combined <- combined %in% names(stats)
  if (length(split_cols) == 0 && !has_combined) {
    warning("No projection source supplied ", what,
            "; it cannot be applied and will contribute 0 points.")
    return(rep(0, nrow(stats)))
  }
  split_sum <- if (length(split_cols) > 0) {
    rowSums(as.matrix(stats[, split_cols, drop = FALSE]), na.rm = TRUE)
  } else 0
  combined_v <- if (has_combined) coalesce(stats[[combined]], 0) else 0
  pmax(split_sum, combined_v)
}

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

  # Two-point conversions: all types score the same here, so combined and split
  # source columns are interchangeable estimates of the same total.
  stats$two_pt_proj <- combined_or_split(
    stats, combined = "two_pts",
    split = c("pass_two_pts", "rush_two_pts", "rec_two_pts"),
    what = sprintf("two-point conversion projections (the %g-point rule)", s$two_pt)
  )

  # Fumbles lost: most sources publish a combined fumbles_lost; a couple split it by
  # play type instead.
  stats$fumbles_lost_proj <- combined_or_split(
    stats, combined = "fumbles_lost",
    split = c("rush_fumbles_lost", "rec_fumbles_lost", "sack_fumbles_lost",
              "rushing_fumbles_lost", "receiving_fumbles_lost"),
    what = sprintf("fumbles-lost projections (the %g-point rule)", s$fumble_lost)
  )

  # Return TDs ("special teams player TD"). Fumble-recovery TDs for offensive players
  # are projected by no source and are left unscored - see README.
  if ("return_tds" %in% names(stats)) {
    stats$return_tds_proj <- coalesce(stats$return_tds, 0)
  } else {
    warning("No projection source supplied return-TD projections; the ", s$return_td,
            "-point special-teams TD rule cannot be applied and will contribute 0 points.")
    stats$return_tds_proj <- 0
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
        two_pt_proj            * s$two_pt +
        fumbles_lost_proj      * s$fumble_lost +
        return_tds_proj        * s$return_td,
      points = round(points, 1)
    )
}
