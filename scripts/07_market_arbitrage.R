# -------------------------------------------------------------------------------------------
# Market arbitrage: our projections vs Sleeper's
#
# Premise: every other manager in this league prices players off the numbers Sleeper shows
# them. Where Sleeper and this model disagree, the disagreement is tradeable REGARDLESS of
# which one is right - a player Sleeper rates below us is cheap to buy, and one Sleeper rates
# above us is expensive to hold and easy to spend.
#
# Method note that matters: this scores Sleeper's RAW STAT projections through this league's
# own scoring rules, using the same score_players() code path our own projections go through.
# Comparing Sleeper's displayed point totals against ours would measure two scoring systems as
# much as two forecasts.
#
# Inputs : data/league/sleeper_proj_wk<NN>.csv   (scripts/00_fetch_league.R)
#          data/weekly_wk<NN>_<season>.csv        (scripts/04_weekly_sheet.R)
#          data/league/rosters.csv                (scripts/00_fetch_league.R)
# Output : data/market_arbitrage_wk<NN>_<season>.csv, plus the tables below on the console.
# -------------------------------------------------------------------------------------------

suppressMessages({library(dplyr); library(readr); library(stringr)})
source("R/league_config.R")
source("R/scoring.R")

season <- league$season
week <- as.integer(Sys.getenv("WEEK", NA))
if (is.na(week)) {
  avail <- list.files("data/league", pattern = "^sleeper_proj_wk\\d+\\.csv$")
  if (length(avail) == 0) {
    stop("No data/league/sleeper_proj_wk*.csv. Run scripts/00_fetch_league.R first ",
         "(it needs the open internet - in practice, push to .run-fetch and let CI do it).")
  }
  week <- max(as.integer(str_extract(avail, "\\d+")))
}
wk <- sprintf("%02d", week)

sleeper_path <- sprintf("data/league/sleeper_proj_wk%s.csv", wk)
ours_path    <- sprintf("data/weekly_wk%s_%d.csv", wk, season)
for (p in c(sleeper_path, ours_path, "data/league/rosters.csv")) {
  if (!file.exists(p)) stop(p, " is missing.")
}

# Match on names, not ids: the two feeds carry different id spaces. Suffixes and punctuation
# are the only differences that actually occur between them.
norm_name <- function(x) {
  x <- str_replace_all(x, "[.'`]", "")
  x <- str_replace_all(x, "\\s+(Jr|Sr|II|III|IV|V)$", "")
  str_squish(tolower(x))
}

# Sleeper publishes no first-down projection, so the Sleeper side gets first downs derived
# from its own projected yards at the same FTN rates score_players() uses on ours. Any
# disagreement about first-down RATE is therefore invisible to this comparison - it measures
# disagreement about yards, touchdowns and volume only.
sleeper <- read_csv(sleeper_path, show_col_types = FALSE) %>%
  filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
  transmute(player, pos,
            pass_yds = pass_yd, pass_tds = pass_td, pass_int = pass_int,
            rush_yds = rush_yd, rush_tds = rush_td,
            rec_yds  = rec_yd,  rec_tds  = rec_td,
            fumbles_lost = fum_lost,
            pass_two_pts = pass_2pt, rush_two_pts = rush_2pt, rec_two_pts = rec_2pt)

# score_players() warns about return TDs, which neither feed publishes; that gap is already
# documented in the README and is identical on both sides of this comparison.
sleeper <- suppressWarnings(score_players(sleeper, league)) %>%
  rename(sleeper = points) %>%
  filter(sleeper > 0) %>%
  mutate(key = norm_name(player)) %>%
  group_by(key) %>% slice_max(sleeper, n = 1, with_ties = FALSE) %>% ungroup()

ours <- read_csv(ours_path, show_col_types = FALSE) %>%
  mutate(key = norm_name(player)) %>%
  group_by(key) %>% slice_max(points, n = 1, with_ties = FALSE) %>% ungroup()

owners <- read_csv("data/league/rosters.csv", show_col_types = FALSE) %>%
  mutate(key = norm_name(player)) %>% select(key, manager, slot, status)

unmatched <- nrow(ours) - sum(ours$key %in% sleeper$key)

arb <- ours %>%
  inner_join(select(sleeper, key, sleeper), by = "key") %>%
  left_join(owners, by = "key") %>%
  mutate(manager = coalesce(manager, "FREE"),
         gap = round(points - sleeper, 1),
         sleeper = round(sleeper, 1)) %>%
  select(player, pos, team, manager, slot, status, ours = points, sleeper, gap) %>%
  arrange(desc(gap))

out <- sprintf("data/market_arbitrage_wk%s_%d.csv", wk, season)
write_csv(arb, out)

cat(sprintf("\nWeek %d: %d players matched, %d of ours had no Sleeper projection.\n",
            week, nrow(arb), unmatched))
cat(sprintf("Mean gap %+.1f, correlation %.2f - the closer to 1, the less there is to exploit.\n\n",
            mean(arb$gap), cor(arb$ours, arb$sleeper)))

show <- function(title, d, cols) {
  cat("===", title, "===\n")
  print(as.data.frame(d %>% select(all_of(cols))), row.names = FALSE)
  cat("\n")
}

me <- league$my_manager
show(sprintf("%s - where the market disagrees with us", me),
     arb %>% filter(manager == me) %>% arrange(gap),
     c("player", "pos", "slot", "ours", "sleeper", "gap"))
show("BUY - rostered elsewhere, market rates them below us",
     arb %>% filter(!manager %in% c("FREE", me), ours >= 8) %>% head(12),
     c("player", "pos", "manager", "ours", "sleeper", "gap"))
show("SPEND - market rates them above us (good pieces to give up)",
     arb %>% filter(manager != "FREE", sleeper >= 8) %>% arrange(gap) %>% head(12),
     c("player", "pos", "manager", "ours", "sleeper", "gap"))
show("FREE AGENTS the market under-rates",
     arb %>% filter(manager == "FREE", ours >= 5) %>% head(12),
     c("player", "pos", "team", "ours", "sleeper", "gap"))

# Ranking only. The level is not meaningful: rostered players skew towards starters, where
# Sleeper reads high across the board, so every manager's total comes out negative.
cat("=== Whose roster the market over-prices most (ranking, not level) ===\n")
print(as.data.frame(
  arb %>% filter(manager != "FREE") %>% group_by(manager) %>%
    summarise(n = n(), ours = round(sum(ours), 1), sleeper = round(sum(sleeper), 1),
              per_player = round((sum(ours) - sum(sleeper)) / n(), 2)) %>%
    arrange(per_player)), row.names = FALSE)

cat("\nWritten: ", out, "\n", sep = "")
