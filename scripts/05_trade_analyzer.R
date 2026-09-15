# -------------------------------------------------------------------------------------------
# Step 5: Trade analyzer
#
# Scores a proposed trade from BOTH sides by rebuilding each team's best legal starting
# lineup before and after. A trade only gets accepted if the other manager also gains, so
# the counterparty column is the one that tells you whether to bother sending it.
#
# Lineup: 1 QB, 2 RB, 2 WR, 1 TE, 1 FLEX (RB/WR/TE), 1 SUPERFLEX (QB/RB/WR/TE).
# Slots are nested (QB and FLEX both sit inside SUPERFLEX), so filling the most restrictive
# slot first with the best eligible player is provably optimal - no search needed.
#
# Run:  Rscript scripts/05_trade_analyzer.R
# -------------------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(stringr)

source("R/league_config.R")
source("R/rosters.R")

ros <- read_csv(sprintf("data/value_sheet_%d.csv", league$season), show_col_types = FALSE)

norm <- function(x) tolower(str_squish(gsub("\\s+(Jr|Sr|II|III|IV)\\.?$", "", x)))
ros <- ros %>% mutate(k = norm(player))

# Points for a set of players. Anyone OUT/IR counts as zero; anyone the projections do not
# cover is reported so a silent zero never props up a trade.
value_of <- function(names) {
  tibble(k = norm(names), name = names) %>%
    left_join(ros %>% select(k, pos, pts = points), by = "k") %>%
    mutate(pts = ifelse(name %in% unavailable, 0, pts))
}

# Best legal lineup: fill restrictive slots first, then FLEX, then SUPERFLEX.
best_lineup <- function(pool) {
  take <- function(p, eligible, n) {
    picked <- p %>% filter(pos %in% eligible) %>% arrange(desc(pts)) %>% slice_head(n = n)
    list(picked = picked, rest = anti_join(p, picked, by = "name"))
  }
  out <- list(); p <- pool %>% filter(!is.na(pts))
  for (spec in list(c("QB", 1), c("RB", 2), c("WR", 2), c("TE", 1))) {
    r <- take(p, spec[1], as.integer(spec[2])); out <- c(out, list(r$picked)); p <- r$rest
  }
  r <- take(p, c("RB", "WR", "TE"), 1); out <- c(out, list(r$picked)); p <- r$rest
  r <- take(p, c("QB", "RB", "WR", "TE"), 1); out <- c(out, list(r$picked))
  bind_rows(out)
}

lineup_total <- function(names) sum(best_lineup(value_of(names))$pts)

# Score one trade. `give` leaves team_a for team_b; `get` comes back the other way.
score_trade <- function(team_a, team_b, give, get, label) {
  a <- rosters[[team_a]]$players; b <- rosters[[team_b]]$players
  missing <- value_of(c(give, get)) %>% filter(is.na(pts), !name %in% unavailable)
  if (nrow(missing) > 0) {
    warning("No projection for: ", paste(missing$name, collapse = ", "))
  }
  a2 <- c(setdiff(a, give), get)
  b2 <- c(setdiff(b, get), give)
  tibble(
    trade = label, partner = team_b,
    you_before = lineup_total(a), you_after = lineup_total(a2),
    you_gain = lineup_total(a2) - lineup_total(a),
    them_before = lineup_total(b), them_after = lineup_total(b2),
    them_gain = lineup_total(b2) - lineup_total(b),
    partner_roster = ifelse(rosters[[team_b]]$complete, "full", "STARTERS ONLY")
  )
}

# --- Candidate trades -------------------------------------------------------------------

candidates <- bind_rows(
  score_trade("themikemartin", "Rowdy17",   c("MarShawn Lloyd", "Stefon Diggs"), "Chris Olave",   "Lloyd + Diggs -> Olave"),
  score_trade("themikemartin", "Rowdy17",   c("MarShawn Lloyd", "Stefon Diggs"), "Nico Collins",  "Lloyd + Diggs -> Collins"),
  score_trade("themikemartin", "Rowdy17",   "MarShawn Lloyd",                    "Chris Olave",   "Lloyd -> Olave"),
  score_trade("themikemartin", "Wenzo3030", c("MarShawn Lloyd", "Stefon Diggs"), "Drake London",  "Lloyd + Diggs -> London"),
  score_trade("themikemartin", "Wenzo3030", c("MarShawn Lloyd", "Stefon Diggs"), "Davante Adams", "Lloyd + Diggs -> Adams"),
  score_trade("themikemartin", "Wenzo3030", "MarShawn Lloyd",                    "Davante Adams", "Lloyd -> Adams"),
  score_trade("themikemartin", "Wenzo3030", "MarShawn Lloyd",                    "DK Metcalf",    "Lloyd -> Metcalf"),
  score_trade("themikemartin", "jezz281",   c("MarShawn Lloyd", "Stefon Diggs"), "George Pickens","Lloyd + Diggs -> Pickens"),
  # Geno is your third QB and never starts - but in a superflex league he is a STARTER for
  # any team carrying only one. That asymmetry is the only place a genuine win-win lives.
  score_trade("themikemartin", "Rowdy17",   "Geno Smith",                        "Chris Olave",   "Geno -> Olave"),
  score_trade("themikemartin", "Rowdy17",   c("Geno Smith", "MarShawn Lloyd"),   "Nico Collins",  "Geno + Lloyd -> Collins"),
  score_trade("themikemartin", "jezz281",   c("Geno Smith", "MarShawn Lloyd"),   "George Pickens","Geno + Lloyd -> Pickens"),
  score_trade("themikemartin", "jezz281",   "Geno Smith",                        "Rashee Rice",   "Geno -> Rashee Rice")
)

cat(sprintf("\nYour current best lineup: %.0f pts (modeled starters, K/DEF excluded)\n\n",
            lineup_total(rosters$themikemartin$players)))

cat("=== TRADES, SCORED FROM BOTH SIDES ===\n")
print(as.data.frame(candidates %>%
  mutate(across(where(is.numeric), \(x) round(x, 1))) %>%
  select(trade, partner, you_gain, them_gain, partner_roster) %>%
  arrange(desc(you_gain))), row.names = FALSE)

cat("\nA trade needs you_gain > 0 AND them_gain > 0 to be worth sending.\n")
cat("Rows marked STARTERS ONLY: partner bench unseen, so their gain is overstated.\n")
