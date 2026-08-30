# -------------------------------------------------------------------------------------------
# League configuration
#
# Every league-specific number lives here. The scoring, valuation, and optimizer code
# read from this list only, so a rule change is a one-line edit.
# -------------------------------------------------------------------------------------------

league <- list(
  season       = 2026,
  n_teams      = 10,
  budget       = 200,
  roster_size  = 16,

  # Starting lineup. K and DEF are treated as $1 players and never modeled.
  starters = c(QB = 1, RB = 2, WR = 2, TE = 1, FLEX = 1, SUPERFLEX = 1, K = 1, DEF = 1),

  # Scoring rules, matched to the league app's Scoring Settings screens (2026-08-30):
  # passing yards 0.04/yd confirmed; pick six is -5 on top of the -2 interception,
  # applied as expected value below.
  scoring = list(
    pass_td       = 5,
    pass_int      = -2,
    pick_six      = -5,      # additional to the INT penalty
    pass_yd       = 0.04,
    rush_yd       = 0.1,
    rec_yd        = 0.1,
    rush_td       = 6,
    rec_td        = 6,
    reception     = 0,
    rec_first_down  = 1,
    rush_first_down = 0.75,
    two_pt        = 2,       # same for passing, rushing, and receiving conversions
    fumble_lost   = -2,
    return_td     = 6        # "special teams player TD" (kick/punt return TDs)
  ),

  # Projection sources publish neither pick sixes nor receiving first downs, so both
  # are derived (see R/scoring.R):
  #
  # - pick_six_rate: share of interceptions returned for a touchdown. Roughly 1 in 13
  #   NFL interceptions is a pick six, so each projected INT carries an expected extra
  #   penalty of 0.075 * -5 = -0.375 points.
  # - rec_fd_rates: receiving first downs per receiving yard, from FTN's regression
  #   work. A QB's receiving yards (trick plays, a few yards a season) use the WR rate;
  #   the choice is worth < 0.1 points.
  # - rush_fd_rate: rushing first downs per rushing yard, from the same FTN article
  #   (y = 0.0508x, R = 0.934). FTN publishes a single rate, not per-position.
  pick_six_rate = 0.075,
  rec_fd_rates  = c(RB = 0.045, WR = 0.048, TE = 0.050, QB = 0.048),
  rush_fd_rate  = 0.0508,

  # Replacement ranks: the positional rank at which a player is freely available at $1.
  # Derived from starter demand across 10 teams:
  #   QB: 10 QB slots + ~9 of 10 superflex slots go QB, plus superflex teams rostering a
  #       spare QB pushes true replacement to ~QB22 (not QB12 as in a 1-QB league).
  #   RB: 20 base + ~4-5 of the 10 flex slots        -> ~RB25
  #   WR: 20 base + ~4-5 flex                        -> ~WR25
  #   TE: 10 base + ~1 flex                          -> ~TE11
  replacement_rank = c(QB = 22, RB = 25, WR = 25, TE = 11)
)

# Dollar pool arithmetic ---------------------------------------------------------------

# Slots filled for $1 and excluded from the value model: 6 bench + K + DEF per team.
league$n_dollar_slots <- (6 + 1 + 1) * league$n_teams                      # 80

# Modeled starter slots (QB/RB/WR/TE incl. flex + superflex): 8 per team.
league$n_valued_slots <- sum(league$starters[c("QB", "RB", "WR", "TE", "FLEX", "SUPERFLEX")]) *
  league$n_teams                                                           # 80

# Dollars available for modeled starters: 10 x $200 minus $1 per bench/K/DEF slot.
league$value_pool <- league$n_teams * league$budget - league$n_dollar_slots  # 1920

# Every modeled starter costs at least $1; the rest is distributed in proportion to
# points above replacement.
league$marginal_pool <- league$value_pool - league$n_valued_slots            # 1840
