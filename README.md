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
- Scoring (matched to the league app's Scoring Settings screens): 5/pass TD, −2/INT
  (plus expected pick-six penalty, see assumptions), 0.04/pass yd, 0.1/rush & rec yd,
  6/rush & rec TD, 0/reception, 1 per receiving first down, 0.75 per rushing first
  down, 2 per two-point conversion (any type), −2 per fumble lost, 6 per return TD
- K and DEF are $1 players and are never scraped or modeled

## Running it locally

Needs **R 4.1 or newer** (the code uses the `\(x)` lambda shorthand).

```sh
git clone https://github.com/mikemartin/fantasy-football-auction-optimizer.git
cd fantasy-football-auction-optimizer
git checkout claude/fantasy-auction-optimizer-o0fyp4
```

Install the packages once:

```r
install.packages(c(
  "dplyr", "tidyr", "readr", "stringr", "purrr", "tibble", "jsonlite",
  "glue", "rvest", "httr2", "readxl", "data.table", "rrapply", "remotes"
))
remotes::install_github("FantasyFootballAnalytics/ffanalytics", upgrade = "never")

# only needed for the optional LP check in scripts/03
install.packages("Rglpk")   # needs the GLPK system library:
                            #   macOS  brew install glpk
                            #   Debian sudo apt install libglpk-dev
```

Then run from the repo root, in this order:

```sh
Rscript scripts/00_fetch_league.R         # live league from Sleeper  -> data/league/
Rscript scripts/01_scrape_projections.R   # multi-source raw stats    -> data/projections_raw_2026.csv
Rscript scripts/02_build_value_sheet.R    # score + price             -> value sheet CSV + HTML

Rscript scripts/04_weekly_sheet.R 3       # this week's start/sit + defence streaming
Rscript scripts/06_trade_search.R         # every trade across all ten teams, both sides
Rscript scripts/05_trade_analyzer.R       # named candidate trades only
Rscript scripts/07_market_arbitrage.R     # where we disagree with Sleeper -> who to buy and sell
Rscript scripts/08_dual_scorecard.R       # trades scored on our numbers AND on Sleeper's
Rscript scripts/03_lp_sanity_check.R      # optional pre-draft budget check
```

Steps 00 and 01 are the only ones that need the internet. Everything else reads the
CSVs they write, so once you have those you can re-run the analysis offline as often
as you like — changing a scoring rule in `R/league_config.R` and re-running step 02
takes a second and needs no re-scrape.

Step 00 must run before 04, 05 or 06: `R/rosters.R` reads `data/league/rosters.csv`
and will stop with a clear message if it is missing.

### If a scrape fails

`01` prints which sources responded and which stats each one carries. Sources block
scrapers from time to time (FantasySharks and RTSports both return 403 as of week 3)
— the run continues on whatever answered, and the value sheet is built from those.
`data/source_basis_2026.csv` records each source's median and whether it looks like it
is counting a different number of games; see the source-basis note under Assumptions.

### In a sandbox where CRAN is blocked

Claude Code's containers, and some CI sandboxes, route outbound traffic through a proxy
that blocks CRAN and Posit Package Manager, so `install.packages()` fails. Ubuntu ships
the packages this project needs, and apt reaches them:

```sh
apt-get update
apt-get install -y --no-install-recommends \
  r-base-core r-cran-dplyr r-cran-readr r-cran-stringr \
  r-cran-tibble r-cran-tidyr r-cran-purrr r-cran-jsonlite
```

`--no-install-recommends` matters: the recommended set drags in graphics and video
libraries that often 404 against a stale index and fail the whole install. If apt reports
404s on unrelated packages, `apt-get update` first — the index, not the package, is the
problem.

That covers steps 02 and 04-06, which read the committed CSVs. Steps 00 and 01 need the
network anyway, so run those in CI.

**These containers are rebuilt periodically and the install does not survive.** If
`Rscript: command not found` appears mid-session, nothing is broken — reinstall with the
command above. ffanalytics is not packaged for apt, so a container that has been rebuilt
cannot run the scrape locally; that is what the workflows below are for.

### The GitHub Actions alternative

Three workflows do all of the above on a schedule and commit the results back to the
branch, which is useful if you would rather not install R at all:

| workflow | does | fires |
|---|---|---|
| `fetch-league.yml` | step 00 | Tuesdays after waivers, or push to `.run-fetch` |
| `build-value-sheet.yml` | steps 01–02 | on push, or manually |
| `weekly-sheet.yml` | step 04 | Fridays, or push a week number into `.run-weekly` |

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
- **First downs**: not published by projection sources; derived from projected yards at
  FTN regression rates — receiving: RB 4.5%, WR 4.8%, TE 5.0% per receiving yard (QB
  receiving yards, negligible, use the WR rate); rushing: 5.08% per rushing yard
  (FTN publishes a single rushing rate, not per-position; y = 0.0508x, R = 0.934).
- **Two-point conversions** (2 pts) use source projections where available — some
  sources publish them split by play type (`pass/rush/rec_two_pts`), some combined
  (`two_pts`); scoring takes the larger of the two estimates per player and warns
  loudly if no source supplied any of them.
- **Fumbles lost (−2) and return TDs (6)** use source projections where available
  (5 and 4 sources respectively map these columns), with the same warn-if-absent
  behavior as 2PCs. Fumbles score only when lost, matching the MISC screen.
- **Fumble-recovery TDs by offensive players (6 pts) are left unscored**: no
  projection source publishes them, and unlike pick sixes there is no sensible stat
  to derive an expected value from. League-wide only a handful happen per season, so
  the omission is worth well under 0.1 points per player.
- **The rest of the league prices players off Sleeper's projections.** This is an
  assumption about the other nine managers, not about football, and it is what
  `scripts/07_market_arbitrage.R` trades on. Three caveats sit underneath it:
  Sleeper's projection endpoint is undocumented and publishes raw stats, so the script
  scores them through this league's own rules rather than trusting any displayed total;
  Sleeper publishes no first-down projection, so its side of the comparison borrows the
  FTN-derived first downs above, which means a disagreement about first-down *rate* is
  invisible to the comparison; and a gap can be Sleeper being wrong or us being wrong —
  a stale depth chart on our side reads identically to a market error.
- **Trades are searched on a single week's per-game rates**, on both scorecards, because
  Sleeper's projection endpoint is weekly. Rest-of-season value, bye weeks, schedule
  strength and age curves are all outside `scripts/08_dual_scorecard.R`. A package that
  gains a point or two on one week is noise; treat only the large gaps as signal.
- **A player one feed prices at nothing and the other prices as a starter is treated as
  untradeable**, not as a bargain. Sleeper publishes a zero for a doubtful player while our
  model still carries his full projection, which makes any package containing him look like
  a steal for whoever is giving him up. `scripts/08_dual_scorecard.R` lists those packages
  separately rather than ranking them, and names the player.
- **Injured players score zero on both scorecards**, so the trade search is indifferent
  between receiving a hurt star and receiving a healthy bench body. That is wrong in
  reality and in our favour: when the search offers a choice of filler, ask for the
  injured name. The model cannot see the difference, so a human has to.
- **Active rosters cap at 16 and IR slots sit outside it**, so a team at 16 can never
  take back more players than it sends. Six of the ten are full, which is what rules out
  most uneven trade shapes before any projection is consulted.
- **No points for receptions, return yards, or any yardage/reception bonuses** —
  none appear in the league rules or any scoring screen.
- **Replacement ranks** assume the 10 flex slots split ~4.5 RB / ~4.5 WR / ~1 TE and
  ~9 of 10 superflex slots go to QBs, with superflex bench demand pushing QB replacement
  from QB21 to ~QB22.
- **Bench (except superflex QB demand above), K, and DEF carry $0 of marginal value** —
  every one of those 80 slots is priced at exactly $1.
- **Projections are the unweighted mean across whatever sources ffanalytics returns**
  for the season scrape; a player missing from a source is averaged over the sources
  that do project them. `scripts/01_scrape_projections.R` prints which sources responded.
- **Sources may count different numbers of games.** After week 1, some publish
  remaining-season projections while others keep publishing full-season totals that
  include games already played. `scripts/01` scores each source alone, compares medians
  over players every source covers, and writes `data/source_basis_<season>.csv`. A gap
  wide enough to look like a basis difference is reported but NOT corrected by default;
  set `PROJECTION_BASIS_RESCALE=1` to scale the fuller sources down to the shortest.
- **Team defence scoring is partial.** No source projects tackles for loss,
  three-and-outs, fourth-down stops, blocked kicks or special-teams fumble recoveries,
  and against week 2 results those were roughly a third of a defence's total. The
  streaming table ranks defences but understates them, and says so on the report.
- **Points allowed has only two tiers in this league** (6 for a shutout, −3 at 35+,
  nothing between), which is under 4 points of spread across the realistic range. The
  tiers are converted to an expected value over a normal centred on the projection
  rather than applied as a step function to an average, which would return zero for
  every defence.
- **Rounding**: displayed values are whole dollars (min $1), so the sheet total can
  drift a few dollars from $1,920; `value_raw` keeps the unrounded number and is what
  `apply_inflation()` rescales.
- The LP sanity check buys 8 starters with $192 ($200 − $1 × 8 bench/K/DEF) at sheet
  prices, with superflex encoded as QB∈[1,2], RB∈[2,4], WR∈[2,4], TE∈[1,3], total 8.

## Project structure

```
├── R/
│   ├── league_config.R      # every league setting, scoring rule and assumption
│   ├── scoring.R            # raw stats -> points under league rules
│   ├── scoring_dst.R        # team defence and kicker scoring
│   ├── valuation.R          # replacement level, dollars, tiers, apply_inflation()
│   ├── rosters.R            # loads the live league from data/league/rosters.csv
│   └── optimizer.R          # LP roster optimizer (secondary)
├── scripts/
│   ├── 00_fetch_league.R          # Sleeper API -> rosters, results, waivers, free agents
│   ├── 01_scrape_projections.R    # ffanalytics multi-source scrape
│   ├── 02_build_value_sheet.R     # season value sheet: CSV + printable HTML
│   ├── 03_lp_sanity_check.R       # optional LP budget-shape check
│   ├── 04_weekly_sheet.R          # in-season start/sit + defence streaming
│   ├── 05_trade_analyzer.R        # score named trades from both sides
│   ├── 06_trade_search.R          # exhaustive trade search across all ten teams
│   ├── 07_market_arbitrage.R      # our projections vs Sleeper's, per player and per team
│   └── 08_dual_scorecard.R        # trades that clear both our numbers and the market's
├── data/
│   ├── league/              # live league pulled from Sleeper (step 00)
│   └── *.csv                # scraped stats and generated value sheets
└── output/                  # printable HTML sheets
```
