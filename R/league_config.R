# -------------------------------------------------------------------------------------------
# League configuration
#
# Every league-specific number lives here. The scoring, valuation, and optimizer code
# read from this list only, so a rule change is a one-line edit.
# -------------------------------------------------------------------------------------------

league <- list(
  season       = 2026,

  # Sleeper league id, from the web app URL (sleeper.com/leagues/<id>). The league's own
  # read-only API is the source of truth for rosters, results, waivers and free agents -
  # see scripts/00_fetch_league.R. Everything it returns beats reconstructing the league
  # from screenshots, which is how a dm98 roster ended up implying 18 players in a 16-man
  # league.
  sleeper_id   = "1388110539288236032",

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

  # Team defence scoring, from the league's TEAM DEFENSE settings screen (2026-09-21).
  #
  # Two things about this table are unusual and drive the whole streaming strategy:
  #
  # 1. POINTS ALLOWED HAS ONLY TWO TIERS - a shutout pays 6, conceding 35+ costs 3, and
  #    every score between 1 and 34 pays exactly nothing. Most leagues run a six-step
  #    ladder where points allowed is the dominant swing; here it is close to irrelevant,
  #    because roughly seven games in eight land in the dead zone.
  # 2. The scoring is built on PRESSURE VOLUME - sacks, tackles for loss, three-and-outs
  #    and fourth-down stops. That rewards a defence that is good, more than one that
  #    happens to draw a weak opponent.
  def_scoring = list(
    dst_int          = 2,
    dst_fum_rec      = 2,
    dst_safety       = 2,
    blocked_kick     = 1,
    dst_td           = 6,
    tackle_for_loss  = 0.5,
    dst_sacks        = 1,
    pts_allowed_0    = 6,
    pts_allowed_35up = -3,
    three_and_out    = 0.5,
    fourth_down_stop = 1,
    st_td            = 6,
    st_fum_rec       = 2
  ),

  # Kicker scoring, from the KICKING settings screen (2026-09-21).
  k_scoring = list(
    pat_made       = 1,
    fg_made        = 3,
    pts_per_fg_yard_over_30 = 0.1
  ),

  # Spread of a team's points allowed around its projection, used to turn the two
  # points-allowed tiers into an expected value instead of applying a step function to
  # an average. NFL team scoring has a standard deviation near 10 points a game.
  pts_allowed_sd = 9.5,

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

# Your roster (edit as you make moves) - used by scripts/04_weekly_sheet.R to mark
# and rank your players in the weekly start/sit output. K and DEF are not scraped.
league$my_roster <- c(
  "Jordan Love", "Bryce Young", "Geno Smith",
  "Jonathan Taylor", "Christian McCaffrey", "MarShawn Lloyd",
  "Kaelon Black",
  "Jaxon Smith-Njigba", "Garrett Wilson", "Courtland Sutton", "Xavier Worthy",
  "Stefon Diggs", "Deebo Samuel",
  "Travis Kelce"
)
