# -------------------------------------------------------------------------------------------
# League rosters
#
# Loaded from data/league/rosters.csv, which scripts/00_fetch_league.R pulls straight from
# Sleeper. This replaces a hand-maintained list built from chat notifications, which covered
# only five of ten teams, drifted out of date, and could not see injuries at all.
#
# Exports, unchanged so the trade scripts keep working:
#   rosters      named list, one entry per manager, each with $players and $complete
#   unavailable  players who cannot be counted on - OUT, IR, PUP or suspended
# -------------------------------------------------------------------------------------------

local({
  path <- "data/league/rosters.csv"
  if (!file.exists(path)) {
    stop("data/league/rosters.csv is missing. Run scripts/00_fetch_league.R (it needs the ",
         "open internet, so in practice: push to .run-fetch and let CI do it).")
  }
  r <- readr::read_csv(path, show_col_types = FALSE)

  # K and DEF are $1 slots the value model never covers, so they are dropped here rather
  # than left to surface downstream as players with no projection.
  r <- dplyr::filter(r, pos %in% c("QB", "RB", "WR", "TE"))

  rosters <<- lapply(split(r, r$manager), function(d) {
    list(complete = TRUE, players = d$player)
  })

  # A player carrying any of these cannot be relied on for a trade or a lineup. Sleeper's
  # "Questionable" is deliberately excluded - those players mostly play.
  out <- dplyr::filter(r, status %in% c("Out", "IR", "PUP", "Sus", "NA") | slot == "IR")
  unavailable <<- unique(out$player)

  message(sprintf("Loaded %d teams, %d modeled players, %d unavailable (OUT/IR/PUP).",
                  length(rosters), nrow(r), length(unavailable)))
})
