# fantasy-football-auction-optimizer

Auction **value sheet** generator for a 10-team, $200, superflex league, built on
[ffanalytics](https://github.com/FantasyFootballAnalytics/ffanalytics) projections.

The main output is a per-player dollar value sheet (CSV + printable one-page HTML), not an
"optimal roster": a live auction never honours sheet prices, so the useful artifact is a
price for every player that you can re-inflate mid-draft. The LP optimizer is kept as a
secondary pre-draft sanity check on budget allocation.

## League settings (R/league_config.R)

- 10 teams, $200 budget, 16 roster spots
- Starters: 1 QB, 2 RB, 2 WR, 1 TE, 1 flex (W/R/T), 1 superflex (Q/W/R/T), 1 K, 1 DEF; 6 bench
- Scoring: 5/pass TD, −2/INT (plus expected pick-six penalty, see assumptions),
  0.04/pass yd, 0.1/rush & rec yd, 6/rush & rec TD, 0/reception,
  1 per **receiving** first down (rushing first downs score 0)
- K and DEF are $1 players and are never scraped or modeled

## Usage

```sh
Rscript scripts/01_scrape_projections.R   # scrape raw 2026 stat projections -> data/
Rscript scripts/02_build_value_sheet.R    # score, price, tier -> CSV + HTML sheet
Rscript scripts/03_lp_sanity_check.R      # optional: LP budget-shape check
```

Outputs:

- `data/value_sheet_2026.csv` — player, team, pos, projected points, dollar value, tier
- `output/value_sheet_2026.html` — one-page printable cheat sheet (4 columns, sorted by value)

### Mid-draft inflation

As players sell over or under sheet price, re-price the remaining board from an R console:

```r
source("R/league_config.R"); source("R/valuation.R")
sheet <- readr::read_csv("data/value_sheet_2026.csv")

# 45 players gone for a combined $612 — name them for exact repricing...
live <- apply_inflation(sheet, dollars_spent = 612,
                        players_gone = c("Bijan Robinson", "Ja'Marr Chase", ...),
                        league = league)

# ...or pass a count to approximate with the top-45 players by value:
live <- apply_inflation(sheet, dollars_spent = 612, players_gone = 45, league = league)
head(live, 30)
```

Count only dollars spent on QB/RB/WR/TE (skip $1 K/DEF buys, or include them and their
names/count — just be consistent between the two arguments).

## How the pricing works

1. **Replacement level** per position from starter demand. Superflex means ~20 QBs start
   across 10 teams, so QB replacement sits at ~QB22 rather than QB12. Defaults:
   QB22 / RB25 / WR25 / TE11 (`league$replacement_rank`, with the demand math in comments).
2. **Dollar pool** = 10 × $200 − $1 × 80 bench/K/DEF slots = **$1,920** across the 80
   modeled starter slots (8 per team).
3. Every modeled starter costs at least $1; the remaining $1,840 is distributed in
   proportion to points above replacement.
4. **Tiers** per position via largest projected-point gaps (deterministic, ~5 players/tier).

## Assumptions

All of these are editable in `R/league_config.R` unless noted.

- **Passing yards score 0.04/yd** — confirmed 2026-08-30.
- **Pick sixes**: no projection source publishes them, so the −5 penalty is applied as
  expected value: ~1 in 13 INTs is a pick six (rate 0.075), costing an extra
  0.075 × 5 ≈ 0.38 pts per projected INT on top of the −2. Confirmed 2026-08-30.
- **Receiving first downs**: not published by projection sources; derived from projected
  receiving yards at FTN regression rates — RB 4.5%, WR 4.8%, TE 5.0% per receiving yard.
  QB receiving yards (negligible) use the WR rate.
- **No points for fumbles, receptions, two-point conversions, return TDs, or any
  yardage/reception bonuses** — none appear in the league rules given, so scraped
  projections for them are deliberately unused.
- **Replacement ranks** assume the 10 flex slots split ~4.5 RB / ~4.5 WR / ~1 TE and
  ~9 of 10 superflex slots go to QBs, with superflex bench demand pushing QB replacement
  from QB21 to ~QB22.
- **Bench (except superflex QB demand above), K, and DEF carry $0 of marginal value** —
  every one of those 80 slots is priced at exactly $1.
- **Projections are the unweighted mean across whatever sources ffanalytics returns**
  for the season scrape; a player missing from a source is averaged over the sources
  that do project them. `scripts/01_scrape_projections.R` prints which sources responded.
- **Rounding**: displayed values are whole dollars (min $1), so the sheet total can
  drift a few dollars from $1,920; `value_raw` keeps the unrounded number and is what
  `apply_inflation()` rescales.
- The LP sanity check buys 8 starters with $192 ($200 − $1 × 8 bench/K/DEF) at sheet
  prices, with superflex encoded as QB∈[1,2], RB∈[2,4], WR∈[2,4], TE∈[1,3], total 8.

## Project structure

```
├── R/
│   ├── league_config.R      # every league setting and assumption
│   ├── scoring.R            # raw stats -> points under league rules
│   ├── valuation.R          # replacement level, dollars, tiers, apply_inflation()
│   └── optimizer.R          # LP roster optimizer (secondary)
├── scripts/
│   ├── 01_scrape_projections.R
│   ├── 02_build_value_sheet.R
│   └── 03_lp_sanity_check.R
├── data/                    # scraped raw stats + generated value sheet CSVs
└── output/                  # printable HTML sheet
```

## Requirements

```r
install.packages(c("tidyverse", "Rglpk", "remotes"))
remotes::install_github("FantasyFootballAnalytics/ffanalytics")
```
