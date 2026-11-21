# -------------------------------------------------------------------------------------------
# LP roster optimizer (secondary tool)
#
# Maximizes projected starter points subject to budget and lineup constraints, buying
# players at their value-sheet prices. A live auction never honours sheet prices, so this
# is NOT a draft plan - it is a pre-draft sanity check on how a $200 budget could be
# allocated across positions (e.g. "how much should the QB slots absorb in superflex?").
#
# Lineup: 1 QB, 2 RB, 2 WR, 1 TE, 1 FLEX (W/R/T), 1 SUPERFLEX (Q/W/R/T) = 8 modeled
# starters. Encoded as count bounds, which are exactly the rosters that can legally fill
# those slots: two open slots beyond the base positions, at most one of which (the
# superflex) can take a second QB.
#   QB in [1, 2],  RB in [2, 4],  WR in [2, 4],  TE in [1, 3],  total = 8
# -------------------------------------------------------------------------------------------

library(Rglpk)
library(dplyr)

optimize_auction_roster <- function(sheet, league,
                                    budget = NULL,
                                    pos_limits = tribble(
                                      ~pos, ~min, ~max,
                                      "QB", 1, 2,
                                      "RB", 2, 4,
                                      "WR", 2, 4,
                                      "TE", 1, 3
                                    )) {
  n_starters <- sum(pos_limits$min) + 2  # base slots + flex + superflex

  # Budget for modeled starters: $200 minus $1 for each bench/K/DEF spot.
  if (is.null(budget)) {
    budget <- league$budget - (league$roster_size - n_starters)
  }

  data <- sheet %>% select(player, team, pos, points, value)
  n <- nrow(data)
  if (n == 0) stop("Empty value sheet.")

  pos_mat <- sapply(pos_limits$pos, function(p) as.integer(data$pos == p))

  mat <- rbind(
    data$value,      # total cost
    rep(1, n),       # roster count
    t(pos_mat),      # position maxima
    -t(pos_mat)      # position minima
  )

  sol <- Rglpk_solve_LP(
    obj = data$points,
    mat = mat,
    dir = c("<=", "==", rep("<=", nrow(pos_limits)), rep("<=", nrow(pos_limits))),
    rhs = c(budget, n_starters, pos_limits$max, -pos_limits$min),
    types = rep("B", n),
    max = TRUE
  )
  if (sol$status != 0) stop("No feasible roster under these constraints.")

  roster <- data[sol$solution == 1, ] %>% arrange(desc(points))

  list(
    roster = roster,
    totals = summarise(roster,
      total_points = sum(points),
      total_cost = sum(value),
      budget = budget
    ),
    by_position = roster %>%
      group_by(pos) %>%
      summarise(n = n(), spent = sum(value), points = sum(points), .groups = "drop")
  )
}
