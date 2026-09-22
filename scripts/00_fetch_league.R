# -------------------------------------------------------------------------------------------
# Step 0: Pull the real league from Sleeper's read-only API
#
# Everything downstream used to be reconstructed from screenshots: rosters from chat
# notifications, results from match screens, free agency from whoever happened to be visible.
# That drifts - the dm98 roster ended up implying 18 players in a 16-man league - and it left
# five of ten teams permanently invisible.
#
# This replaces all of it. The API needs no key and only reads.
#
# Writes to data/league/:
#   managers.csv        one row per team: manager, record, points for/against, FAAB left
#   rosters.csv         one row per rostered player, with the team that owns them
#   free_agents.csv     every active NFL player at a modeled position that nobody rosters
#   results_wk<NN>.csv  actual points scored by every player in the league, per week
#   transactions.csv    every waiver, free agent add and trade, with the FAAB bid
#   settings.csv        roster slots, waiver type, playoff and trade-deadline weeks
#
# Run:  Rscript scripts/00_fetch_league.R
# The container's proxy blocks api.sleeper.app, so this runs in CI (see the workflow).
# -------------------------------------------------------------------------------------------

library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

source("R/league_config.R")

# purrr and recent base R both supply this, but the script leans on it heavily and a missing
# definition would fail deep inside a map_dfr with an unhelpful message.
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

BASE <- "https://api.sleeper.app/v1"
OUT  <- "data/league"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# A failed call is reported with the URL that failed rather than returning an empty frame
# that would quietly read as "the league has no transactions".
get_json <- function(path, allow_empty = FALSE) {
  url <- paste0(BASE, path)
  res <- tryCatch(fromJSON(url, simplifyVector = FALSE),
                  error = function(e) {
                    stop("Sleeper API call failed: ", url, "\n  ", conditionMessage(e))
                  })
  if (length(res) == 0 && !allow_empty) {
    warning("Sleeper returned nothing for ", url)
  }
  res
}

message("Fetching league ", league$sleeper_id)

state    <- get_json("/state/nfl")
this_week <- as.integer(state$week %||% 1)
info     <- get_json(paste0("/league/", league$sleeper_id))
users    <- get_json(paste0("/league/", league$sleeper_id, "/users"))
rosters  <- get_json(paste0("/league/", league$sleeper_id, "/rosters"))

message("  ", info$name, " - ", info$total_rosters, " teams, season ", info$season,
        ", NFL week ", this_week)

# --- Player dictionary -------------------------------------------------------------------
# ~5MB and rarely changes, so it is cached between runs.
players_cache <- file.path(OUT, "players_nfl.rds")
if (file.exists(players_cache) &&
    difftime(Sys.time(), file.mtime(players_cache), units = "days") < 3) {
  players <- readRDS(players_cache)
  message("  using cached player dictionary (", length(players), " players)")
} else {
  message("  downloading player dictionary...")
  players <- get_json("/players/nfl")
  saveRDS(players, players_cache)
}

pl <- function(id, field, default = NA_character_) {
  p <- players[[id]]
  if (is.null(p) || is.null(p[[field]])) default else as.character(p[[field]])
}
player_name <- function(id) {
  nm <- pl(id, "full_name")
  # Team defences come back as the team abbreviation rather than a person.
  if (is.na(nm)) ifelse(nchar(id) <= 4, paste(id, "D/ST"), id) else nm
}

# --- Managers ------------------------------------------------------------------------------
user_by_id <- setNames(users, vapply(users, \(u) u$user_id, character(1)))

managers <- map_dfr(rosters, function(r) {
  u <- user_by_id[[r$owner_id %||% ""]]
  s <- r$settings
  tibble(
    roster_id   = r$roster_id,
    manager     = u$display_name %||% "(unknown)",
    team_name   = u$metadata$team_name %||% (u$display_name %||% "(unknown)"),
    wins        = s$wins %||% NA_integer_,
    losses      = s$losses %||% NA_integer_,
    ties        = s$ties %||% NA_integer_,
    points_for  = (s$fpts %||% 0) + (s$fpts_decimal %||% 0) / 100,
    points_against = (s$fpts_against %||% 0) + (s$fpts_against_decimal %||% 0) / 100,
    faab_used   = s$waiver_budget_used %||% NA_integer_,
    faab_left   = (info$settings$waiver_budget %||% NA_integer_) - (s$waiver_budget_used %||% 0)
  )
}) %>% arrange(desc(wins), desc(points_for))

write_csv(managers, file.path(OUT, "managers.csv"))
message("  managers.csv   ", nrow(managers), " teams")

# --- Rosters --------------------------------------------------------------------------------
manager_of <- setNames(managers$manager, managers$roster_id)

roster_rows <- map_dfr(rosters, function(r) {
  ids <- unlist(r$players %||% list())
  if (length(ids) == 0) return(tibble())
  starters <- unlist(r$starters %||% list())
  reserve  <- unlist(r$reserve  %||% list())
  tibble(
    manager = manager_of[[as.character(r$roster_id)]],
    player_id = ids,
    player = vapply(ids, player_name, character(1)),
    pos    = vapply(ids, \(i) pl(i, "position"), character(1)),
    nfl_team = vapply(ids, \(i) pl(i, "team"), character(1)),
    status = vapply(ids, \(i) pl(i, "injury_status", ""), character(1)),
    slot   = case_when(ids %in% reserve ~ "IR", ids %in% starters ~ "starter", TRUE ~ "bench")
  )
})
write_csv(roster_rows, file.path(OUT, "rosters.csv"))
message("  rosters.csv    ", nrow(roster_rows), " rostered players across ",
        n_distinct(roster_rows$manager), " teams")

# --- Free agents -----------------------------------------------------------------------------
# Anyone at a modeled position who is on an NFL roster and on no fantasy roster. This is the
# answer to "is he actually available", which screenshots could never give reliably.
rostered <- unique(roster_rows$player_id)
MODELED <- c("QB", "RB", "WR", "TE", "K", "DEF")
free_agents <- map_dfr(setdiff(names(players), rostered), function(id) {
  p <- players[[id]]
  if (is.null(p$position) || !(p$position %in% MODELED)) return(tibble())
  if (is.null(p$team) || is.na(p$team)) return(tibble())        # not on an NFL roster
  if (!isTRUE(p$active) && p$position != "DEF") return(tibble())
  tibble(player_id = id, player = player_name(id), pos = p$position,
         nfl_team = p$team, status = p$injury_status %||% "")
})
write_csv(free_agents, file.path(OUT, "free_agents.csv"))
message("  free_agents.csv ", nrow(free_agents), " available at modeled positions")

# --- Weekly results ---------------------------------------------------------------------------
# Actual points per player per week, for every team. This is what lets the projection model be
# calibrated against reality rather than against a screenshot of one roster.
for (wk in seq_len(max(1, this_week))) {
  mus <- tryCatch(get_json(paste0("/league/", league$sleeper_id, "/matchups/", wk),
                           allow_empty = TRUE),
                  error = function(e) NULL)
  if (is.null(mus) || length(mus) == 0) next
  rows <- map_dfr(mus, function(m) {
    pts <- m$players_points %||% list()
    if (length(pts) == 0) return(tibble())
    ids <- names(pts)
    tibble(
      week = wk,
      matchup_id = m$matchup_id %||% NA_integer_,
      manager = manager_of[[as.character(m$roster_id)]],
      player = vapply(ids, player_name, character(1)),
      pos    = vapply(ids, \(i) pl(i, "position"), character(1)),
      points = unlist(pts, use.names = FALSE),
      started = ids %in% unlist(m$starters %||% list())
    )
  })
  if (nrow(rows) == 0) next
  write_csv(rows, file.path(OUT, sprintf("results_wk%02d.csv", wk)))
  message(sprintf("  results_wk%02d.csv %d player-weeks", wk, nrow(rows)))
}

# --- Transactions ------------------------------------------------------------------------------
tx <- map_dfr(seq_len(max(1, this_week)), function(wk) {
  ts <- tryCatch(get_json(paste0("/league/", league$sleeper_id, "/transactions/", wk),
                          allow_empty = TRUE), error = function(e) NULL)
  if (is.null(ts) || length(ts) == 0) return(tibble())
  map_dfr(ts, function(t) {
    adds  <- names(t$adds  %||% list())
    drops <- names(t$drops %||% list())
    tibble(
      week = wk,
      # Epoch milliseconds; converted on read so the waiver cadence is visible.
      when_utc = as.POSIXct((t$status_updated %||% NA_real_) / 1000,
                            origin = "1970-01-01", tz = "UTC"),
      type = t$type %||% NA_character_,
      status = t$status %||% NA_character_,
      manager = paste(unique(na.omit(manager_of[as.character(unlist(t$roster_ids))])),
                      collapse = " + "),
      added   = paste(vapply(adds,  player_name, character(1)), collapse = ", "),
      dropped = paste(vapply(drops, player_name, character(1)), collapse = ", "),
      faab    = t$settings$waiver_bid %||% NA_integer_
    )
  })
})
if (nrow(tx) > 0) {
  write_csv(tx, file.path(OUT, "transactions.csv"))
  message("  transactions.csv ", nrow(tx), " moves")
}

# --- Settings -----------------------------------------------------------------------------------
s <- info$settings
settings <- tibble(
  name = info$name,
  season = info$season,
  teams = info$total_rosters,
  roster_slots = paste(unlist(info$roster_positions), collapse = " "),
  waiver_type = s$waiver_type %||% NA,
  waiver_budget = s$waiver_budget %||% NA,
  # 0 = Sunday ... 6 = Saturday. With waiver_clear_days, this is when claims process and
  # therefore when the bidding deadline falls.
  waiver_day_of_week = s$waiver_day_of_week %||% NA,
  waiver_clear_days = s$waiver_clear_days %||% NA,
  daily_waivers_hour = s$daily_waivers_hour %||% NA,
  playoff_teams = s$playoff_teams %||% NA,
  playoff_start_week = s$playoff_week_start %||% NA,
  trade_deadline = s$trade_deadline %||% NA
)
write_csv(settings, file.path(OUT, "settings.csv"))
message("  settings.csv")
cat("\n")
print(as.data.frame(settings), row.names = FALSE)
cat("\n=== STANDINGS ===\n")
print(as.data.frame(managers %>%
  select(manager, team_name, wins, losses, points_for, faab_left)), row.names = FALSE)

