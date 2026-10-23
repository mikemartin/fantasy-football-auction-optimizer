# -------------------------------------------------------------------------------------------
# Valuation
#
# Converts projected points into auction dollars with value-based pricing:
#   1. Replacement level per position from the league's starter demand (league_config.R).
#   2. Points above replacement (PAR) for every player.
#   3. $1 per modeled starter slot, plus the marginal dollar pool distributed in
#      proportion to PAR.
#
# Also provides tiers (per-position clustering on points) and apply_inflation(), the
# mid-draft rescaling helper.
# -------------------------------------------------------------------------------------------

library(dplyr)

# Replacement level: points of the player at the configured replacement rank, per position.
replacement_points <- function(scored, league) {
  unknown <- setdiff(unique(scored$pos), names(league$replacement_rank))
  if (length(unknown) > 0) {
    stop("Positions with no replacement rank in league_config.R: ",
         paste(unknown, collapse = ", "),
         ". Filter them out or add ranks before valuing.")
  }
  scored %>%
    group_by(pos) %>%
    arrange(desc(points), .by_group = TRUE) %>%
    summarise(
      replacement_pts = points[min(league$replacement_rank[first(pos)], n())],
      .groups = "drop"
    )
}

# Dollar values: $1 floor for anyone above replacement, marginal pool split by PAR share.
add_values <- function(scored, league) {
  repl <- replacement_points(scored, league)

  valued <- scored %>%
    left_join(repl, by = "pos") %>%
    mutate(par = pmax(points - replacement_pts, 0))

  total_par <- sum(valued$par)

  valued %>%
    mutate(
      value_raw = ifelse(par > 0, 1 + league$marginal_pool * par / total_par, 1),
      value = pmax(1, round(value_raw))
    ) %>%
    arrange(desc(value_raw), desc(points))
}

# Tiers: cluster on projected points within each position, top players only (down to a
# bit past replacement). Uses largest point-gaps as tier breaks so results are
# deterministic and explainable: a tier ends where the drop to the next player is big.
add_tiers <- function(valued, league, players_per_tier = 5, max_tiers = 8) {
  valued %>%
    group_by(pos) %>%
    arrange(desc(points), .by_group = TRUE) %>%
    mutate(tier = {
      n_tiered <- min(n(), league$replacement_rank[first(pos)] + 5)
      k <- min(max_tiers, max(2, ceiling(n_tiered / players_per_tier)))
      drops <- -diff(points[seq_len(n_tiered)])
      breaks <- sort(order(drops, decreasing = TRUE)[seq_len(k - 1)])
      tier_of <- findInterval(seq_len(n_tiered) - 1, breaks) + 1
      c(tier_of, rep(NA_integer_, n() - n_tiered))
    }) %>%
    ungroup()
}

# ---------------------------------------------------------------------------------------
# Mid-draft inflation helper
#
# As players sell above or below sheet value, the remaining pool changes. Re-run this
# with what has actually happened and it rescales the marginal value of every remaining
# player so the sheet again sums to the dollars still available for starters.
#
#   sheet         - the value sheet from add_values() (data/value_sheet_<season>.csv works)
#   dollars_spent - total dollars spent so far on modeled players (ignore $1 K/DEF/bench)
#   players_gone  - EITHER a character vector of drafted player names (exact match,
#                   case-insensitive) OR a single number n, which approximates the board
#                   by assuming the top n players by value are gone.
#
# Returns the remaining players with a new `value` column and prints the inflation rate.
# ---------------------------------------------------------------------------------------
apply_inflation <- function(sheet, dollars_spent, players_gone, league) {
  if (is.numeric(players_gone) && length(players_gone) == 1) {
    gone <- sheet %>% arrange(desc(value_raw)) %>% slice_head(n = players_gone)
  } else {
    matched <- tolower(sheet$player) %in% tolower(players_gone)
    unmatched <- players_gone[!tolower(players_gone) %in% tolower(sheet$player)]
    if (length(unmatched) > 0) {
      warning("Not on the sheet (check spelling): ", paste(unmatched, collapse = ", "))
    }
    gone <- sheet[matched, ]
  }

  remaining <- anti_join(sheet, gone, by = "player")

  # Marginal dollars: each drafted modeled player consumed $1 of floor money; the rest
  # of the spend came out of the marginal pool.
  marginal_spent    <- dollars_spent - nrow(gone)
  marginal_left     <- league$marginal_pool - marginal_spent
  sheet_marginal    <- sum(remaining$value_raw - 1)

  inflation <- marginal_left / sheet_marginal
  message(sprintf(
    "Spent $%d on %d players (sheet said $%d). Inflation factor on remaining value: %.2f",
    dollars_spent, nrow(gone), sum(gone$value), inflation
  ))

  remaining %>%
    mutate(
      value_raw = 1 + (value_raw - 1) * inflation,
      value = pmax(1, round(value_raw))
    ) %>%
    arrange(desc(value_raw))
}
