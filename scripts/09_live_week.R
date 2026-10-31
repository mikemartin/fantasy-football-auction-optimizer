# -------------------------------------------------------------------------------------------
# Live week: banked points plus remaining projections for every matchup
#
# For each starter Sleeper has locked in, a player whose NFL team has already played scores
# his actual points; everyone else scores his projection. "Already played" is inferred from
# the results file: a team counts as played once any rostered player on it has a non-zero
# score. Override with PLAYED=TEAM,TEAM / NOTPLAYED=TEAM when a game is mid-way or a team's
# rostered players all scored zero.
#
# Projections: ours (data/weekly_wk<NN>) and Sleeper's raw stats rescored under this league's
# rules (data/league/sleeper_proj_wk<NN>), averaged where both exist. K and DEF use our
# streaming tables. Assumption: points still to come carry the usual per-team spread
# (SD 26.2 a week) scaled by the share of starters yet to play.
#
# Inputs : data/league/results_wk<NN>.csv, rosters.csv, sleeper_proj_wk<NN>.csv
#          data/weekly_wk<NN>_<season>.csv, data/streaming_{dst,k}_wk<NN>_<season>.csv
# Usage  : WEEK=8 Rscript scripts/09_live_week.R
# -------------------------------------------------------------------------------------------

suppressMessages({library(dplyr); library(readr); library(stringr)})
source("R/league_config.R")
source("R/scoring.R")

week <- as.integer(Sys.getenv("WEEK", NA))
if (is.na(week)) stop("Set WEEK, e.g. WEEK=8 Rscript scripts/09_live_week.R")
season <- league$season
me <- league$my_manager
SD_WEEK <- 26.2

norm <- function(x) { x <- str_replace_all(x, "[.'`]", ""); x <- str_replace_all(x, "\\s+(Jr|Sr|II|III|IV|V)$", ""); str_squish(tolower(x)) }
std <- function(t) recode(t, KCC = "KC", JAC = "JAX", LA = "LAR", WSH = "WAS")
one <- function(df) df %>% group_by(key) %>% slice_max(points, n = 1, with_ties = FALSE) %>% ungroup()

res  <- read_csv(sprintf("data/league/results_wk%02d.csv", week), show_col_types = FALSE)
ros  <- read_csv("data/league/rosters.csv", show_col_types = FALSE) %>% select(manager, player, nfl_team, status)
ours <- read_csv(sprintf("data/weekly_wk%02d_%d.csv", week, season), show_col_types = FALSE) %>% mutate(key = norm(player)) %>% one() %>% select(key, ours = points)
sl <- read_csv(sprintf("data/league/sleeper_proj_wk%02d.csv", week), show_col_types = FALSE) %>% filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
  transmute(player, pos, pass_yds = pass_yd, pass_tds = pass_td, pass_int = pass_int, rush_yds = rush_yd, rush_tds = rush_td,
            rec_yds = rec_yd, rec_tds = rec_td, fumbles_lost = fum_lost, pass_two_pts = pass_2pt, rush_two_pts = rush_2pt, rec_two_pts = rec_2pt)
sl <- suppressWarnings(score_players(sl, league)) %>% mutate(key = norm(player)) %>% one() %>% select(key, sleeper = points)
dst <- read_csv(sprintf("data/streaming_dst_wk%02d_%d.csv", week, season), show_col_types = FALSE) %>% transmute(nfl_team = std(team), dp = points)
kk  <- read_csv(sprintf("data/streaming_k_wk%02d_%d.csv", week, season), show_col_types = FALSE) %>% transmute(player, kp = points)

x <- res %>% left_join(ros, by = c("manager", "player")) %>% mutate(key = norm(player)) %>%
  left_join(ours, by = "key") %>% left_join(sl, by = "key") %>% left_join(dst, by = "nfl_team") %>% left_join(kk, by = "player") %>%
  mutate(proj = case_when(pos == "DEF" ~ dp, pos == "K" ~ kp, TRUE ~ rowMeans(cbind(ours, sleeper), na.rm = TRUE)),
         proj = ifelse(status %in% c("Out", "IR", "Sus", "PUP"), 0, coalesce(proj, 0)))

played <- unique(x$nfl_team[x$points != 0])
played <- union(played, strsplit(Sys.getenv("PLAYED", ""), ",")[[1]])
played <- setdiff(played, strsplit(Sys.getenv("NOTPLAYED", ""), ",")[[1]])
x <- x %>% mutate(state = ifelse(nfl_team %in% played, "played", "to play"), exp = ifelse(state == "played", points, proj))

st <- x %>% filter(started)
tot <- st %>% group_by(matchup_id, manager) %>%
  summarise(now = sum(points), exp = sum(exp), left = sum(state == "to play"), n = n(), .groups = "drop")
mu <- tot %>% arrange(matchup_id, desc(manager == me)) %>% group_by(matchup_id) %>%
  summarise(a = manager[1], b = manager[2], now_a = now[1], now_b = now[2], exp_a = exp[1], exp_b = exp[2],
            left_a = left[1], left_b = left[2],
            p_a = pnorm((exp[1] - exp[2]) / pmax(1, SD_WEEK * sqrt((left[1] / n[1] + left[2] / n[2])))), .groups = "drop")

options(width = 200)
cat("Week", week, "- NFL teams already played:", paste(sort(played), collapse = " "), "\n\n")
print(as.data.frame(mu %>% mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)
opp <- mu$b[mu$a == me]
for (m in c(me, opp)) {
  cat("\n==", m, "==\n")
  print(as.data.frame(st %>% filter(manager == m) %>% transmute(player, pos, team = nfl_team, status, state, points,
    ours = round(ours, 1), sleeper = round(sleeper, 1), proj = round(proj, 1), exp = round(exp, 1)) %>% arrange(state, desc(exp))), row.names = FALSE)
}
cat("\n== bench (", me, ") ==\n")
print(as.data.frame(x %>% filter(manager == me, !started) %>% transmute(player, pos, team = nfl_team, status, state, points, proj = round(proj, 1))), row.names = FALSE)
