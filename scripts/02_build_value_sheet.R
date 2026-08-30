# -------------------------------------------------------------------------------------------
# Step 2: Build the auction value sheet (MAIN OUTPUT)
#
# Reads the raw projections from step 1, scores them under league rules, converts points
# to dollars with value-based pricing, and writes:
#   - data/value_sheet_<season>.csv        full sheet, sorted by value
#   - output/value_sheet_<season>.html     printable one-page cheat sheet
#
# Run from the repo root:  Rscript scripts/02_build_value_sheet.R
#
# Mid-draft, reload the sheet and rescale with apply_inflation() - see README.
# -------------------------------------------------------------------------------------------

library(dplyr)
library(readr)

source("R/league_config.R")
source("R/scoring.R")
source("R/valuation.R")

raw_path <- sprintf("data/projections_raw_%d.csv", league$season)
if (!file.exists(raw_path)) {
  stop(raw_path, " not found - run scripts/01_scrape_projections.R first.")
}

sheet <- read_csv(raw_path, show_col_types = FALSE) %>%
  score_players(league) %>%
  add_values(league) %>%
  add_tiers(league) %>%
  select(player, team, pos, points, value, value_raw, tier, par)

csv_path <- sprintf("data/value_sheet_%d.csv", league$season)
write_csv(sheet, csv_path)
message("Wrote ", csv_path)

# Sanity check: sheet dollars should reconcile with the league's pool.
message(sprintf(
  "Pool check: $%d modeled pool; sheet values for above-replacement players sum to $%d.",
  league$value_pool, sum(sheet$value[sheet$par > 0])
))

# --- Printable one-page HTML sheet -----------------------------------------------------

pos_colors <- c(QB = "#c33", RB = "#171", WR = "#26c", TE = "#a50")

row_html <- function(r) {
  sprintf(
    '<tr><td class="v">$%d</td><td class="p">%s</td><td>%s</td><td class="pos" style="color:%s">%s%d</td><td class="pts">%.0f</td><td class="t">%s</td></tr>',
    r$value, r$player, r$team, pos_colors[r$pos], r$pos, r$pos_rank, r$points,
    ifelse(is.na(r$tier), "", r$tier)
  )
}

printable <- sheet %>%
  group_by(pos) %>%
  arrange(desc(points), .by_group = TRUE) %>%
  mutate(pos_rank = row_number()) %>%
  ungroup() %>%
  arrange(desc(value_raw), desc(points)) %>%
  filter(value > 1 | row_number() <= 140) %>%  # keep the sheet to one printed page
  slice_head(n = 160)

rows <- paste(vapply(seq_len(nrow(printable)),
                     function(i) row_html(printable[i, ]), character(1)),
              collapse = "\n")

html <- sprintf('<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>Auction Value Sheet %d</title>
<style>
  body { font: 8pt/1.25 -apple-system, Helvetica, Arial, sans-serif; margin: 12px; }
  h1 { font-size: 11pt; margin: 0 0 2px; }
  .sub { color: #555; font-size: 7pt; margin-bottom: 6px; }
  .cols { column-count: 4; column-gap: 14px; column-rule: 1px solid #ccc; }
  table { border-collapse: collapse; width: 100%%; }
  td { padding: 0.5px 3px; white-space: nowrap; border-bottom: 1px solid #eee; }
  td.v { font-weight: 700; text-align: right; width: 24px; }
  td.p { overflow: hidden; text-overflow: ellipsis; max-width: 105px; }
  td.pos { font-weight: 600; }
  td.pts, td.t { color: #777; text-align: right; }
  tr:nth-child(5n) td { border-bottom: 1px solid #bbb; } /* guide line every 5 rows */
  @media print { body { margin: 0; } .cols { column-count: 4; } }
</style></head><body>
<h1>Auction Value Sheet - %d</h1>
<div class="sub">10 teams &middot; $200 &middot; superflex &middot; $%d pool over %d starter slots &middot;
columns: $ / player / team / pos-rank / proj pts / tier &middot; generated %s</div>
<div class="cols"><table>%s</table></div>
</body></html>',
  league$season, league$season, league$value_pool, league$n_valued_slots,
  format(Sys.Date()), rows)

dir.create("output", showWarnings = FALSE)
html_path <- sprintf("output/value_sheet_%d.html", league$season)
writeLines(html, html_path)
message("Wrote ", html_path)

print(head(select(sheet, player, team, pos, points, value, tier), 25), n = 25)
