# -------------------------------------------------------------------------------------------
# Step 6: Exhaustive trade search
#
# Enumerates EVERY 1-for-1, 2-for-1, 1-for-2 and 2-for-2 trade against every rival roster
# and scores each from both sides. Replaces guessing at candidates: if a deal exists that
# helps you without hurting them, this finds it.
#
# Run:  Rscript scripts/06_trade_search.R
# -------------------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(stringr)

source("R/league_config.R")
source("R/rosters.R")

ros <- read_csv(sprintf("data/value_sheet_%d.csv", league$season), show_col_types = FALSE)
norm <- function(x) tolower(str_squish(gsub("\\s+(Jr|Sr|II|III|IV)\\.?$", "", x)))
ros <- ros %>% mutate(k = norm(player))

# One lookup table for every player in the league, so the search works on indices.
all_names <- unique(unlist(lapply(rosters, `[[`, "players")))
tbl <- tibble(name = all_names, k = norm(all_names)) %>%
  left_join(ros %>% select(k, pos, pts = points), by = "k") %>%
  mutate(pts = ifelse(name %in% unavailable, 0, pts))
# Deep-bench players the projection sources never cover. Zeroed so the search still runs,
# but named so a trade is never justified by a player we cannot value.
if (any(is.na(tbl$pts))) {
  warning("No projection for ", sum(is.na(tbl$pts)), " rostered player(s), scored as 0: ",
          paste(tbl$name[is.na(tbl$pts)], collapse = ", "))
  tbl <- tbl %>% mutate(pos = ifelse(is.na(pos), "WR", pos), pts = ifelse(is.na(pts), 0, pts))
}
POS <- setNames(tbl$pos, tbl$name)
PTS <- setNames(tbl$pts, tbl$name)

# Fast best-legal-lineup: sort once, then fill QB, RB x2, WR x2, TE, FLEX, SUPERFLEX.
lineup_pts <- function(names) {
  p <- PTS[names]; s <- POS[names]
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

combos <- function(v, k) if (k == 1) as.list(v) else
  apply(combn(v, k), 2, identity, simplify = FALSE)

me <- rosters$themikemartin$players
base_me <- lineup_pts(me)
results <- list()

for (team in setdiff(names(rosters), "themikemartin")) {
  them <- rosters[[team]]$players
  base_them <- lineup_pts(them)
  for (ng in 1:2) for (nr in 1:2) {
    for (give in combos(me, ng)) for (get in combos(them, nr)) {
      a2 <- c(setdiff(me, give), get)
      b2 <- c(setdiff(them, get), give)
      results[[length(results) + 1]] <- list(
        partner = team,
        give = paste(give, collapse = " + "), get = paste(get, collapse = " + "),
        you = lineup_pts(a2) - base_me, them = lineup_pts(b2) - base_them,
        balanced = ng == nr)
    }
  }
}

res <- bind_rows(results) %>% mutate(across(c(you, them), \(x) round(x, 1)))

cat(sprintf("\nSearched %d trades. Your baseline: %.0f pts\n", nrow(res), base_me))

# Many different "give" packages produce the identical result, because most of your
# spare players are not in your lineup. Collapse to the CHEAPEST give for each target.
give_cost <- function(g) sum(PTS[strsplit(g, " \\+ ")[[1]]])
best <- res %>%
  mutate(cost = vapply(give, give_cost, numeric(1))) %>%
  group_by(partner, get, you, them) %>%
  slice_min(cost, n = 1, with_ties = FALSE) %>%
  ungroup()

cat("\n=== TRUE WIN-WIN (both sides gain) ===\n")
ww <- best %>% filter(you > 0, them > 0) %>% arrange(desc(you))
if (nrow(ww) == 0) cat("  none exist\n") else
  print(as.data.frame(ww %>% select(partner, give, get, you, them) %>% head(12)), row.names = FALSE)

cat("\n=== SENDABLE (you gain, costs them < 5 pts) ===\n")
print(as.data.frame(best %>% filter(you > 0, them > -5) %>% arrange(desc(you)) %>%
  select(partner, give, get, you, them, balanced) %>% head(12)), row.names = FALSE)

cat("\n=== BEST PER PARTNER (costs them < 15 pts) ===\n")
print(as.data.frame(best %>% filter(you > 0, them > -15) %>% group_by(partner) %>%
  slice_max(you, n = 2, with_ties = FALSE) %>% ungroup() %>%
  select(partner, give, get, you, them)), row.names = FALSE)
