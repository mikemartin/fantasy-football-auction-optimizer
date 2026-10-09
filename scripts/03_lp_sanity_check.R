# -------------------------------------------------------------------------------------------
# Step 3 (optional): LP sanity check on budget allocation
#
# Buys the points-maximizing legal starting lineup AT SHEET PRICES. A live auction never
# honours sheet prices, so read this only as a pre-draft check on budget shape - roughly
# how many dollars the optimizer wants at each position - not as a target roster.
#
# Run from the repo root:  Rscript scripts/03_lp_sanity_check.R
# -------------------------------------------------------------------------------------------

library(dplyr)
library(readr)

source("R/league_config.R")
source("R/optimizer.R")

sheet_path <- sprintf("data/value_sheet_%d.csv", league$season)
if (!file.exists(sheet_path)) {
  stop(sheet_path, " not found - run scripts/02_build_value_sheet.R first.")
}
sheet <- read_csv(sheet_path, show_col_types = FALSE)

result <- optimize_auction_roster(sheet, league)

cat("\nOptimal starters at sheet prices (1 QB, 2 RB, 2 WR, 1 TE, flex, superflex):\n\n")
print(as.data.frame(result$roster), row.names = FALSE)
cat("\nBudget shape:\n")
print(as.data.frame(result$by_position), row.names = FALSE)
print(as.data.frame(result$totals), row.names = FALSE)
