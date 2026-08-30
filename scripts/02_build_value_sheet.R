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
  arrange(desc(value_raw), desc(points)) %>%  # add_tiers regroups by position
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

# Plain strings (no sprintf) so CSS percent signs stay literal.
css <- '<style>
  body { font: 8pt/1.25 -apple-system, Helvetica, Arial, sans-serif; margin: 12px; }
  h1 { font-size: 11pt; margin: 0 0 2px; }
  h2 { font-size: 9.5pt; margin: 8px 0 3px; }
  .sub { color: #555; font-size: 7pt; margin-bottom: 4px; }
  .legend { font-size: 7.5pt; background: #f2f2f2; border: 1px solid #ccc;
            padding: 3px 6px; margin-bottom: 6px; }
  .cols { column-count: 4; column-gap: 14px; column-rule: 1px solid #ccc; }
  table { border-collapse: collapse; width: 100%; }
  td { padding: 0.5px 3px; white-space: nowrap; border-bottom: 1px solid #eee; }
  td.v { font-weight: 700; text-align: right; width: 24px; }
  td.p { overflow: hidden; text-overflow: ellipsis; max-width: 105px; }
  td.pos { font-weight: 600; }
  td.pts, td.t { color: #777; text-align: right; }
  tr:nth-child(5n) td { border-bottom: 1px solid #bbb; } /* guide line every 5 rows */
  .guide { page-break-before: always; font-size: 8.5pt; max-width: 7.5in; }
  .guide li { margin-bottom: 3px; }
  .guide .two { column-count: 2; column-gap: 20px; }
  .tracker { width: 100%; margin-top: 4px; }
  .tracker td, .tracker th { border: 1px solid #999; height: 15px; font-size: 8pt;
                             padding: 1px 4px; text-align: left; }
  .tracker th { background: #eee; }
  .tracker td.money { width: 40px; }
  @media print { body { margin: 0; } .cols { column-count: 4; } }
</style>'

legend <- paste0(
  '<div class="legend"><b>How to read a row:</b> ',
  '<b>$</b> = fair price under league scoring &mdash; your max bid; ',
  'below it is profit, a few dollars above only for the last player of a tier. ',
  '<span style="color:#c33"><b>QB7</b></span> = 7th-best at the position ',
  '(<span style="color:#c33"><b>QB</b></span>/<span style="color:#171"><b>RB</b></span>/',
  '<span style="color:#26c"><b>WR</b></span>/<span style="color:#a50"><b>TE</b></span>). ',
  '<b>pts</b> = projected season points. <b>Tier</b>: players in one tier are near ',
  'enough the same &mdash; buy the cheapest, pay up only when a tier is nearly empty. ',
  'Cross players off as they sell and jot what they went for. Full guide on page 2.</div>'
)

tracker_row <- function(slot) paste0(
  '<tr><td style="width:70px">', slot, '</td><td></td><td class="money">$</td>',
  '<td style="width:70px">Budget left:</td><td class="money">$</td></tr>'
)
tracker <- paste0(
  '<table class="tracker"><tr><th>Slot</th><th>Player</th><th>Paid</th>',
  '<th colspan="2">Running budget (start $200)</th></tr>',
  paste(vapply(c("QB", "RB", "RB", "WR", "WR", "TE", "FLEX", "SUPERFLEX",
                 "Bench", "Bench", "Bench", "Bench", "Bench", "Bench",
                 "K ($1)", "DEF ($1)"), tracker_row, character(1)), collapse = ""),
  '</table>'
)

guide <- paste0('<div class="guide">
<h1>First-Auction Guide</h1>
<div class="two">
<h2>How the auction works</h2>
<ul>
<li>Managers nominate players one at a time and everyone bids from the same $200.
The highest bid wins the player; that money is gone for good.</li>
<li>You must fill all 16 roster spots, so the most you can ever bid is
$200 minus $1 for every spot you still have to fill. Real ceiling on one
player: about $185.</li>
<li>The sheet prices assume every team pays $1 each for K, DEF and 6 bench spots,
leaving $192 for your 8 real starters (QB, 2 RB, 2 WR, TE, flex, superflex).</li>
<li>Nobody in the room honours a price list. The sheet is not a prediction of what
players will sell for &mdash; it is what each player is <i>worth</i>, so you know
when to keep bidding and when to let go.</li>
</ul>
<h2>Five rules</h2>
<ol>
<li>Early bids run high. Let the first handful of players go unless one falls to
sheet price &mdash; then bid.</li>
<li>Decide your stop price <i>before</i> the bidding starts, and never bid past it
on a player you only half-want.</li>
<li>Think in tiers, not names. While a tier is deep, hunt its cheapest member; when
it is down to its last player, pay full sheet price or $1-2 more.</li>
<li>This league starts ~2 QBs per team (superflex), so QBs are worth far more here
than on generic sheets. Leave with two QBs from the board&apos;s top 15 &mdash;
the $18-37 middle tier is the best value in the room.</li>
<li>Spend exactly $1 on your K and $1 on your DEF, at the end. Never more.</li>
</ol>
<h2>A sensible $200 plan</h2>
<ul>
<li>Two QBs &asymp; $55 total (one ~$31-37, one ~$19-24)</li>
<li>One anchor RB or WR &asymp; $60-90</li>
<li>Two more starters &asymp; $25-45 each</li>
<li>Remaining starters (WR/TE/flex) &asymp; $10-25 each</li>
<li>Bench $1-3 each + K/DEF at $1 = keep ~$10 back</li>
<li>The plan is a shape, not a script &mdash; if the room hands you a $70 player for
$50, take it and adjust.</li>
</ul>
<h2>Watching the room</h2>
<ul>
<li>Jot the sale price next to your printed value as players sell. A run of sales
above your numbers means money is leaving the room fast: your remaining targets get
cheaper later, but act sooner on must-haves. Sales below your numbers mean managers
are hoarding; mid-tier players will get expensive late.</li>
<li>Rough correction: for every $100 the room overpays in total, shave ~$1 off each
remaining top player&apos;s price.</li>
<li>Endgame: when most teams are down to $1-per-slot, a single spare dollar wins any
player. Count who can still bid.</li>
</ul>
</div>
<h2>Your roster &mdash; fill it in as you buy</h2>', tracker, '</div>')

html <- paste0('<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>Auction Value Sheet ', league$season, '</title>', css, '</head><body>
<h1>Auction Value Sheet - ', league$season, '</h1>
<div class="sub">10 teams &middot; $200 budget &middot; superflex &middot; $',
league$value_pool, ' pool over ', league$n_valued_slots,
' starter slots &middot; generated ', format(Sys.Date()), '</div>',
legend,
'<div class="cols"><table>', rows, '</table></div>',
guide,
'</body></html>')

dir.create("output", showWarnings = FALSE)
html_path <- sprintf("output/value_sheet_%d.html", league$season)
writeLines(html, html_path)
message("Wrote ", html_path)

print(head(select(sheet, player, team, pos, points, value, tier), 25), n = 25)
