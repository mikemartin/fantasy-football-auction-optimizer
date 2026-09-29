# -------------------------------------------------------------------------------------------
# Step 8: Dual-scorecard trade search
#
# Scores every candidate trade TWICE: once on our projections, and once on Sleeper's - the
# numbers the other nine managers are assumed to be reading (see scripts/07_market_arbitrage.R).
#
# A trade only gets sent if it clears BOTH scorecards:
#   you_ours  > 0   we gain on the numbers we believe
#   them_mkt  > 0   they gain on the numbers they are looking at when they hit accept
#
# them_ours is also reported. It is the honesty column: where them_mkt is positive and
# them_ours is negative, the deal works because the market is mispricing someone, and it is
# worth knowing that rather than pretending both sides win on the same facts.
#
# FOCUS pins one player into every package (default MarShawn Lloyd, whom Sleeper prices 47%
# above our model). EXCLUDE keeps players out of the give side - use it for anyone already
# committed to a pending offer. PENDING_OUT/PENDING_IN re-base the search on the roster a
# trade already sent would leave us with. FOCUS=- searches every package.
#
# Run:  Rscript scripts/08_dual_scorecard.R
# -------------------------------------------------------------------------------------------

suppressMessages({library(dplyr); library(readr); library(stringr); library(tibble)})
source("R/league_config.R")
source("R/scoring.R")

season <- league$season
week <- as.integer(Sys.getenv("WEEK", NA))
if (is.na(week)) {
  avail <- list.files("data/league", pattern = "^sleeper_proj_wk\\d+\\.csv$")
  if (length(avail) == 0) stop("No data/league/sleeper_proj_wk*.csv - run scripts/00_fetch_league.R.")
  week <- max(as.integer(str_extract(avail, "\\d+")))
}
wk <- sprintf("%02d", week)

norm_name <- function(x) {
  x <- str_replace_all(x, "[.'`]", "")
  x <- str_replace_all(x, "\\s+(Jr|Sr|II|III|IV|V)$", "")
  str_squish(tolower(x))
}

# ---- the two scorecards -------------------------------------------------------------------
# Both sides are per-game rates for the same week, so they are directly comparable. Neither is
# a rest-of-season figure: bye weeks and schedule strength are outside this comparison.
ours <- read_csv(sprintf("data/weekly_wk%s_%d.csv", wk, season), show_col_types = FALSE) %>%
  mutate(key = norm_name(player)) %>%
  group_by(key) %>% slice_max(points, n = 1, with_ties = FALSE) %>% ungroup()

mkt <- read_csv(sprintf("data/league/sleeper_proj_wk%s.csv", wk), show_col_types = FALSE) %>%
  filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
  transmute(player, pos,
            pass_yds = pass_yd, pass_tds = pass_td, pass_int = pass_int,
            rush_yds = rush_yd, rush_tds = rush_td,
            rec_yds  = rec_yd,  rec_tds  = rec_td,
            fumbles_lost = fum_lost,
            pass_two_pts = pass_2pt, rush_two_pts = rush_2pt, rec_two_pts = rec_2pt)
mkt <- suppressWarnings(score_players(mkt, league)) %>%
  mutate(key = norm_name(player)) %>%
  group_by(key) %>% slice_max(points, n = 1, with_ties = FALSE) %>% ungroup()

source("R/rosters.R")   # rosters, unavailable

roster_csv <- read_csv("data/league/rosters.csv", show_col_types = FALSE) %>%
  mutate(key = norm_name(player))

players <- tibble(name = unique(unlist(lapply(rosters, `[[`, "players")))) %>%
  mutate(key = norm_name(name)) %>%
  left_join(roster_csv %>% select(key, pos), by = "key") %>%
  left_join(ours %>% select(key, ours = points), by = "key") %>%
  left_join(mkt  %>% select(key, mkt  = points), by = "key")

# Named, not silently zeroed: a player either scorecard cannot see is a player no trade
# should be justified by.
gap_ours <- players$name[is.na(players$ours)]
gap_mkt  <- players$name[is.na(players$mkt)]
if (length(gap_ours) > 0)
  warning("No projection of OURS for ", length(gap_ours), " rostered player(s), scored 0: ",
          paste(gap_ours, collapse = ", "))
if (length(gap_mkt) > 0)
  warning("No SLEEPER projection for ", length(gap_mkt), " rostered player(s), scored 0: ",
          paste(gap_mkt, collapse = ", "))

players <- players %>%
  mutate(ours = coalesce(ours, 0), mkt = coalesce(mkt, 0),
         # Injured players cannot be counted on by either side, on either scorecard.
         ours = ifelse(name %in% unavailable, 0, ours),
         mkt  = ifelse(name %in% unavailable, 0, mkt))

POS   <- setNames(players$pos,  players$name)
OURS  <- setNames(players$ours, players$name)
MKT   <- setNames(players$mkt,  players$name)

# Greedy most-restrictive-first fill. Valid because the slots nest: QB in SUPERFLEX, and
# RB/WR/TE in FLEX in SUPERFLEX, so no earlier slot can strand a player a later one needed.
lineup <- function(names, PV) {
  p <- PV[names]; s <- POS[names]
  o <- order(p, decreasing = TRUE); p <- p[o]; s <- s[o]
  used <- logical(length(p)); total <- 0
  for (spec in list(c("QB", 1), c("RB", 2), c("WR", 2), c("TE", 1))) {
    idx <- head(which(s == spec[1] & !used), as.integer(spec[2]))
    total <- total + sum(p[idx]); used[idx] <- TRUE
  }
  idx <- head(which(s %in% c("RB", "WR", "TE") & !used), 1)
  total <- total + sum(p[idx]); used[idx] <- TRUE
  idx <- head(which(!used), 1)
  total + sum(p[idx])
}

# Active roster room. Sleeper's IR slots sit outside the 16-man limit, so a team at 16 active
# can only take back as many players as it sends. Six of the ten, us included, are full.
room <- roster_csv %>% filter(slot != "IR") %>% count(manager, name = "active") %>%
  mutate(room = league$roster_size - active)
ROOM <- setNames(room$room, room$manager)

me <- league$my_manager
my_players <- rosters[[me]]$players

# FOCUS pins a player into every give-package - the piece we have decided to sell. EXCLUDE
# keeps players out of the give side entirely, which is how a second offer is kept from
# overlapping with one already sitting in front of another manager: two pending trades that
# share a player cannot both be accepted.
# PENDING_IN / PENDING_OUT re-bases the search on the roster we would have if a trade already
# sent is accepted, so the second offer is judged against the right starting lineup.
split_env <- function(v) {
  x <- str_squish(strsplit(Sys.getenv(v, ""), ",")[[1]])
  x[nzchar(x)]
}
pending_out <- split_env("PENDING_OUT")
pending_in  <- split_env("PENDING_IN")
if (length(pending_out) || length(pending_in)) {
  unknown <- setdiff(pending_in, players$name)
  if (length(unknown)) stop("PENDING_IN names nobody on any roster: ", paste(unknown, collapse = ", "))
  my_players <- c(setdiff(my_players, pending_out), pending_in)
  # The counterparty's roster has to move too, or the search will cheerfully offer to buy
  # the same player twice.
  for (t in setdiff(names(rosters), me)) {
    r <- rosters[[t]]$players
    if (any(pending_in %in% r)) r <- c(setdiff(r, pending_in), pending_out)
    rosters[[t]]$players <- r
  }
  message("Re-based on a pending trade: out ", paste(pending_out, collapse = ", "),
          " | in ", paste(pending_in, collapse = ", "))
}

FOCUS   <- Sys.getenv("FOCUS", "MarShawn Lloyd")
exclude <- split_env("EXCLUDE")
focused <- nzchar(FOCUS) && Sys.getenv("FOCUS", "MarShawn Lloyd") != "-"
if (focused && !FOCUS %in% my_players) stop(FOCUS, " is not on our roster.")
if (length(exclude)) message("Excluded from every give-package: ", paste(exclude, collapse = ", "))

combos <- function(v, k) {
  if (k == 0) return(list(character(0)))
  if (k == 1) return(as.list(v))
  apply(combn(v, k), 2, identity, simplify = FALSE)
}

base_me_ours <- lineup(my_players, OURS)
res <- list()

for (team in setdiff(names(rosters), me)) {
  them <- rosters[[team]]$players
  base_them_mkt  <- lineup(them, MKT)
  base_them_ours <- lineup(them, OURS)

  others <- setdiff(my_players, c(FOCUS, exclude))
  give_sets <- if (focused) {
    c(list(FOCUS),
      lapply(combos(others, 1), \(x) c(FOCUS, x)),
      lapply(combos(others, 2), \(x) c(FOCUS, x)))
  } else {
    pool <- setdiff(my_players, exclude)
    c(combos(pool, 1), combos(pool, 2))
  }

  for (give in give_sets) {
    ng <- length(give)
    # Roster limits do most of the pruning, and they are real, not a shortcut. Sleeper caps
    # active rosters at 16 and we are full, so we can never take back more than we send.
    # Six of the ten teams are equally full, which rules out every uneven shape against them.
    # Cap the return package at two. A 3-for-3 is searchable but not sendable: every extra
    # body on each side is another way for the other manager to find a reason to decline.
    for (nr in seq_len(min(ng, 2L))) {
      if (ng - nr > ROOM[[team]]) next
      for (get in combos(them, nr)) {
        a2 <- c(setdiff(my_players, give), get)
        b2 <- c(setdiff(them, get), give)
        res[[length(res) + 1L]] <- list(
          partner   = team,
          give      = paste(give, collapse = " + "),
          get       = paste(get,  collapse = " + "),
          you_ours  = lineup(a2, OURS) - base_me_ours,
          them_mkt  = lineup(b2, MKT)  - base_them_mkt,
          them_ours = lineup(b2, OURS) - base_them_ours,
          shape     = sprintf("%d:%d", ng, nr))
      }
    }
  }
}

res <- bind_rows(res) %>% mutate(across(c(you_ours, them_mkt, them_ours), \(x) round(x, 1)))
cat(sprintf("\nWeek %d. Searched %d packages%s. Our baseline lineup: %.1f pts.\n",
            week, nrow(res), if (focused) paste(" containing", FOCUS) else "", base_me_ours))

# Many give-packages produce an identical outcome because most of our bench never starts.
# Keep the one that costs the least real value for each (partner, target, result).
cost <- function(g) sum(OURS[strsplit(g, " \\+ ")[[1]]])
best <- res %>% mutate(c = vapply(give, cost, numeric(1))) %>%
  group_by(partner, get, you_ours, them_mkt) %>%
  slice_min(c, n = 1, with_ties = FALSE) %>% ungroup() %>% select(-c)

# The scenario goes in the filename too: running the same FOCUS with and without a pending
# trade is the normal way to use this, and the two answers must not overwrite each other.
tag <- paste0(if (focused) paste0("_", tolower(gsub("[^A-Za-z]", "", FOCUS))) else "",
              if (length(pending_out) || length(pending_in)) "_pending" else "")
out <- sprintf("data/dual_scorecard_wk%s_%d%s.csv", wk, season, tag)
write_csv(best %>% arrange(desc(you_ours)), out)

cat("\n=== CLEARS BOTH SCORECARDS (we gain on ours, they gain on Sleeper's) ===\n")
clear <- best %>% filter(you_ours > 0, them_mkt > 0) %>% arrange(desc(you_ours))
if (nrow(clear) == 0) cat("  none\n") else
  print(as.data.frame(head(clear, 15)), row.names = FALSE)

cat("\n=== BEST PER PARTNER, clearing both ===\n")
pp <- clear %>% group_by(partner) %>% slice_max(you_ours, n = 1, with_ties = FALSE) %>%
  ungroup() %>% arrange(desc(you_ours))
if (nrow(pp) == 0) cat("  none\n") else print(as.data.frame(pp), row.names = FALSE)

cat("\n=== TRUE WIN-WIN (they gain on BOTH scorecards - no mispricing needed) ===\n")
tw <- best %>% filter(you_ours > 0, them_mkt > 0, them_ours > 0) %>%
  arrange(desc(you_ours)) %>% head(10)
if (nrow(tw) == 0) cat("  none\n") else print(as.data.frame(tw), row.names = FALSE)

cat("\nWritten: ", out, "\n", sep = "")
