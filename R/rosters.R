# -------------------------------------------------------------------------------------------
# League rosters (update as trades and waivers happen)
#
# Used by scripts/05_trade_analyzer.R to score proposed trades from BOTH sides. Only the
# modeled positions matter (QB/RB/WR/TE) - K and DEF are $1 slots and never affect a trade.
#
# Rosters observed 2026-09-15 after week 1. Where a bench was not visible in the app, the
# roster is marked incomplete and the analyzer warns rather than pretending to know.
# -------------------------------------------------------------------------------------------

rosters <- list(

  # 2026-09-15: traded Jayden Daniels to dm98 for Jordan Love + Garrett Wilson.
  themikemartin = list(complete = TRUE, players = c(
    "Jordan Love", "Bryce Young", "Geno Smith",
    "Jonathan Taylor", "Christian McCaffrey", "MarShawn Lloyd", "Kaelon Black",
    "Jaxon Smith-Njigba", "Garrett Wilson", "Courtland Sutton", "Xavier Worthy",
    "Stefon Diggs", "Deebo Samuel",
    "Travis Kelce")),

  # Full roster seen 2026-09-15. THREE quarterbacks (Caleb, Lawrence, Stroud), four healthy
  # backs, five receivers - deep everywhere. James Conner on IR.
  Rowdy17 = list(complete = TRUE, players = c(
    "Caleb Williams", "Trevor Lawrence", "C.J. Stroud",
    "Saquon Barkley", "Javonte Williams", "Bucky Irving", "Blake Corum", "James Conner",
    "Nico Collins", "Chris Olave", "Tee Higgins", "Parker Washington", "Josh Downs",
    "Sam LaPorta", "Dalton Schultz")),

  # "Waxson Dart" - six receivers, three backs. Zay Flowers currently OUT.
  Wenzo3030 = list(complete = TRUE, players = c(
    "Josh Allen", "Drake Maye", "Cam Ward",
    "Kyren Williams", "Kenneth Walker III", "Chuba Hubbard",
    "Drake London", "Zay Flowers", "Davante Adams", "DK Metcalf", "Chris Godwin",
    "KC Concepcion",
    "Tyler Warren", "Mark Andrews")),

  # "Named you after the dog!" - seven running backs. A.J. Brown on IR.
  jezz281 = list(complete = TRUE, players = c(
    "Brock Purdy", "Jared Goff",
    "Travis Etienne", "Chase Brown", "Omarion Hampton", "Jacory Croskey-Merritt",
    "Kyle Monangai", "J.K. Dobbins", "Rhamondre Stevenson",
    "A.J. Brown", "George Pickens", "Rashee Rice",
    "Trey McBride")),

  # 2026-09-22: paid $31 for Brissett and added Drew Lock - FIVE quarterbacks now.
  # "Braudie sucks" (@dm98) - owns CeeDee Lamb. Three QBs, five backs, four receivers.
  dm98 = list(complete = TRUE, players = c(
    "Jaxson Dart", "Dak Prescott", "Jayden Daniels", "Jacoby Brissett", "Drew Lock",
    "James Cook", "Emmett Johnson", "Cam Skattebo", "Jeremiyah Love", "Tyjae Spears",
    "CeeDee Lamb", "Emeka Egbuka", "Matthew Golden", "Tre Tucker",
    "Harold Fannin", "George Kittle"))
)

# Players who cannot be counted on right now (OUT / IR). The analyzer zeroes them so a
# trade is never justified by a player who is not playing.
unavailable <- c("Zay Flowers", "A.J. Brown", "James Conner")
