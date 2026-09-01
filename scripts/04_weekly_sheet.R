# -------------------------------------------------------------------------------------------
# Step 4: Weekly start/sit sheet (in-season)
#
# Scrapes THIS WEEK's projections from every source that publishes weekly numbers,
# averages them, scores them under league rules, and writes:
#   - data/weekly_wk<NN>_<season>.csv       every player, weekly points, pos rank, mine flag
#   - output/weekly_wk<NN>_<season>.html    phone-sized start/sit + waiver report
#
# It also prints your best legal lineup (1 QB, 2 RB, 2 WR, 1 TE, flex, superflex) from
# league$my_roster - keep that list current in R/league_config.R as you make moves.
#
# Run:  Rscript scripts/04_weekly_sheet.R <week>
# Week may also come from a WEEK env var, or be auto-detected from the NFL calendar.
# -------------------------------------------------------------------------------------------

library(ffanalytics)
library(dplyr)
library(tidyr)
library(readr)

source("R/league_config.R")
source("R/scoring.R")

args <- commandArgs(trailingOnly = TRUE)
week <- suppressWarnings(as.integer(args[1]))
if (is.na(week)) week <- suppressWarnings(as.integer(Sys.getenv("WEEK")))
if (is.na(week)) {
  week <- tryCatch(as.integer(ffanalytics:::get_scrape_week()), error = function(e) NA)
}
if (is.na(week) || week < 1 || week > 18) {
  stop("Could not determine the NFL week - run as: Rscript scripts/04_weekly_sheet.R <week>")
}
message("Scraping week ", week, " projections for ", league$season)

scrape <- scrape_data(pos = c("QB", "RB", "WR", "TE"), season = league$season, week = week)
all_pos <- bind_rows(lapply(scrape, as_tibble))
if (nrow(all_pos) == 0 || !"data_src" %in% names(all_pos)) {
  stop("No projection source returned weekly data - network blocked, sources changed, ",
       "or week ", week, " projections are not published yet.")
}
message("Sources: ", paste(unique(all_pos$data_src), collapse = ", "))

identities <- all_pos %>%
  filter(!is.na(player)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, player, team, pos)

non_stat_cols <- c("id", "player", "team", "pos", "data_src", "src_id",
                   "games", "bye", "site_pts", "site_fppg")

projections <- all_pos %>%
  select(-any_of(setdiff(non_stat_cols, "id"))) %>%
  group_by(id) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(where(is.numeric), \(x) ifelse(is.nan(x), NA_real_, x))) %>%
  inner_join(identities, by = "id") %>%
  filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
  relocate(player, team, pos, .after = id)

weekly <- projections %>%
  score_players(league) %>%
  group_by(pos) %>%
  arrange(desc(points), .by_group = TRUE) %>%
  mutate(pos_rank = row_number()) %>%
  ungroup() %>%
  mutate(mine = player %in% league$my_roster) %>%
  arrange(desc(points)) %>%
  select(player, team, pos, pos_rank, points, mine)

csv_path <- sprintf("data/weekly_wk%02d_%d.csv", week, league$season)
write_csv(weekly, csv_path)
message("Wrote ", csv_path, " (", nrow(weekly), " players)")

# --- Best legal lineup from my roster --------------------------------------------------

mine <- weekly %>% filter(mine) %>% arrange(desc(points))
missing <- setdiff(league$my_roster, mine$player)
if (length(missing) > 0) {
  warning("On your roster but not in this week's projections (bye, injury, or name ",
          "mismatch): ", paste(missing, collapse = ", "))
}

pick <- function(pool, positions, n) {
  taken <- pool %>% filter(pos %in% positions) %>% slice_head(n = n)
  list(taken = taken, rest = anti_join(pool, taken, by = "player"))
}
p <- pick(mine, "QB", 1);                qb <- p$taken; pool <- p$rest
p <- pick(pool, "RB", 2);                rb <- p$taken; pool <- p$rest
p <- pick(pool, "WR", 2);                wr <- p$taken; pool <- p$rest
p <- pick(pool, "TE", 1);                te <- p$taken; pool <- p$rest
p <- pick(pool, c("RB", "WR", "TE"), 1); fx <- p$taken; pool <- p$rest
p <- pick(pool, c("QB", "RB", "WR", "TE"), 1); sf <- p$taken; bench <- p$rest

lineup <- bind_rows(
  qb %>% mutate(slot = "QB"), rb %>% mutate(slot = "RB"), wr %>% mutate(slot = "WR"),
  te %>% mutate(slot = "TE"), fx %>% mutate(slot = "FLEX"), sf %>% mutate(slot = "SFLX")
) %>% select(slot, player, team, pos, points)

cat(sprintf("\nBest lineup, week %d (%.1f pts + K/DEF):\n\n", week, sum(lineup$points)))
print(as.data.frame(lineup), row.names = FALSE)
cat("\nBench:\n")
print(as.data.frame(bench %>% select(player, pos, points)), row.names = FALSE)

# --- Waiver watch: best players NOT on your roster, by position ------------------------

waivers <- weekly %>%
  filter(!mine) %>%
  group_by(pos) %>%
  slice_head(n = 8) %>%
  ungroup()

# NOTE: this list still contains other teams' rosters - it is "best non-mine", not
# "confirmed free agents". Check availability in the app before planning around anyone.

# --- Phone-sized HTML report -----------------------------------------------------------

row_html <- function(r, badge = "") sprintf(
  '<tr><td class="s">%s</td><td class="p">%s</td><td class="m">%s %s%d</td><td class="v">%.1f</td></tr>',
  badge, r$player, r$team, r$pos, r$pos_rank, r$points)

lineup_rows <- paste(vapply(seq_len(nrow(lineup)), function(i) {
  r <- weekly %>% filter(player == lineup$player[i])
  row_html(r[1, ], lineup$slot[i])
}, character(1)), collapse = "")
bench_rows <- paste(vapply(seq_len(nrow(bench)), function(i)
  row_html(bench[i, ], "BN"), character(1)), collapse = "")
waiver_rows <- paste(vapply(seq_len(nrow(waivers)), function(i)
  row_html(waivers[i, ]), character(1)), collapse = "")

html <- paste0('<!DOCTYPE html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Week ', week, ' Sheet</title><style>
  body { font: 15px/1.4 -apple-system, Helvetica, Arial, sans-serif; margin: 0;
         background: #0b0e17; color: #e8eaf0; padding: 14px; }
  h1 { font-size: 17px; letter-spacing: 2px; color: #7ee2b8; margin: 4px 0 2px; }
  h2 { font-size: 13px; letter-spacing: 1.5px; text-transform: uppercase;
       color: #8a92a6; margin: 18px 0 4px; }
  .sub { color: #8a92a6; font-size: 12px; }
  table { border-collapse: collapse; width: 100%; }
  td { padding: 5px 4px; border-bottom: 1px solid #1c2130; }
  td.s { color: #7ee2b8; font-weight: 700; font-size: 11px; width: 40px; }
  td.m { color: #8a92a6; font-size: 12px; text-align: right; }
  td.v { font-weight: 700; text-align: right; width: 46px; }
</style></head><body>
<h1>&spades; WEEK ', week, '</h1>
<div class="sub">Multi-source projections scored under league rules &middot; scraped ',
format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"), '</div>
<h2>Best lineup &middot; ', sprintf("%.1f", sum(lineup$points)), ' pts + K/DEF</h2>
<table>', lineup_rows, '</table>
<h2>Bench</h2><table>', bench_rows, '</table>
<h2>Top non-roster players (check availability)</h2>
<table>', waiver_rows, '</table>
</body></html>')

dir.create("output", showWarnings = FALSE)
html_path <- sprintf("output/weekly_wk%02d_%d.html", week, league$season)
writeLines(html, html_path)
message("Wrote ", html_path)
