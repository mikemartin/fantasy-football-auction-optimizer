# -------------------------------------------------------------------------------------------
# Team defence and kicker scoring
#
# Separate from R/scoring.R because these positions are scored from entirely different stat
# columns, and because the projection sources cover them far less completely than they cover
# QB/RB/WR/TE. Every category the sources cannot supply is reported by name rather than
# quietly contributing zero.
# -------------------------------------------------------------------------------------------

# Categories in the league's defence scoring that NO projection source publishes. Listed
# here so the gap is named in the output instead of being inferred from a low total.
DST_UNPROJECTED <- c(
  tackle_for_loss  = "tackles for loss",
  three_and_out    = "three-and-outs",
  fourth_down_stop = "fourth-down stops",
  blocked_kick     = "blocked kicks",
  st_fum_rec       = "special-teams fumble recoveries"
)

# Expected value of the two points-allowed tiers.
#
# Applying the tiers directly to a projected points-allowed figure would be wrong: a
# projection of 21.3 is an average, and the tiers only pay at the extremes, so a step
# function on the mean returns zero for every defence in the league. What matters is the
# CHANCE of each extreme. Points allowed is treated as normal around the projection with
# the league's configured spread, which turns both tiers into a small but real expected
# value that separates defences facing weak offences from the rest.
pts_allowed_ev <- function(projected_pa, league) {
  sd <- league$pts_allowed_sd
  s <- league$def_scoring
  # A shutout is exactly zero points, so the continuous approximation uses the mass below
  # half a point rather than the density at zero.
  p_shutout <- pnorm(0.5, mean = projected_pa, sd = sd)
  p_blowout <- pnorm(35, mean = projected_pa, sd = sd, lower.tail = FALSE)
  s$pts_allowed_0 * p_shutout + s$pts_allowed_35up * p_blowout
}

# Score team defences. `stats` is the averaged projection frame for pos == "DST".
score_dst <- function(stats, league) {
  s <- league$def_scoring
  get <- function(col) if (col %in% names(stats)) tidyr::replace_na(stats[[col]], 0) else NULL

  supplied <- c("dst_int", "dst_fum_rec", "dst_safety", "dst_td", "dst_sacks",
                "dst_ret_tds", "dst_pts_allowed")
  missing <- supplied[!supplied %in% names(stats)]
  if (length(missing) > 0) {
    warning("No source supplied these defence stats: ", paste(missing, collapse = ", "),
            ". They contribute 0 points.")
  }
  if ("dst_pts_allowed" %in% names(stats)) {
    pa_ev <- pts_allowed_ev(tidyr::replace_na(stats$dst_pts_allowed, 21), league)
  } else {
    warning("No source supplied projected points allowed; both points-allowed tiers ",
            "(shutout +", s$pts_allowed_0, ", 35+ ", s$pts_allowed_35up,
            ") contribute 0 points.")
    pa_ev <- 0
  }

  zero <- rep(0, nrow(stats))
  pts <-
    (get("dst_int")     %||% zero) * s$dst_int +
    (get("dst_fum_rec") %||% zero) * s$dst_fum_rec +
    (get("dst_safety")  %||% zero) * s$dst_safety +
    (get("dst_td")      %||% zero) * s$dst_td +
    (get("dst_sacks")   %||% zero) * s$dst_sacks +
    (get("dst_ret_tds") %||% zero) * s$st_td +
    pa_ev

  warning("Defence scoring is PARTIAL: no source projects ",
          paste(unname(DST_UNPROJECTED), collapse = ", "),
          ". Against observed week 2 results those categories were worth roughly a third ",
          "of a defence's total, so these numbers rank defences but understate them.")

  stats$points <- pts
  stats$points_note <- "partial - excludes TFL, 3-and-out, 4th-down stop, blocked kick, ST fum rec"
  stats
}

# Score kickers. Field-goal distance is the one place the league rewards precision, and no
# source projects made-field-goal DISTANCE, so the per-yard bonus is applied at a league
# average field-goal length rather than pretended to be known.
AVG_FG_YARDS <- 38   # NFL average made field goal sits just under 40 yards

score_k <- function(stats, league) {
  s <- league$k_scoring
  get <- function(col) if (col %in% names(stats)) tidyr::replace_na(stats[[col]], 0) else NULL
  zero <- rep(0, nrow(stats))

  fg <- get("fg") %||% get("fg_made") %||% zero
  xp <- get("xp") %||% get("pat_made") %||% zero
  if (all(fg == 0)) warning("No source supplied projected field goals; kicker scores are 0.")

  bonus_per_fg <- max(0, AVG_FG_YARDS - 30) * s$pts_per_fg_yard_over_30
  stats$points <- fg * (s$fg_made + bonus_per_fg) + xp * s$pat_made
  stats$points_note <- sprintf("per-yard FG bonus applied at a %d-yard league average", AVG_FG_YARDS)
  stats
}

`%||%` <- function(a, b) if (is.null(a)) b else a
