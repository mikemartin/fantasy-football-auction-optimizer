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

  themikemartin = list(complete = TRUE, players = c(
    "Jayden Daniels", "Bryce Young", "Geno Smith",
    "Jonathan Taylor", "Christian McCaffrey", "MarShawn Lloyd", "Kaelon Black", "DJ Giddens",
    "Jaxon Smith-Njigba", "Courtland Sutton", "Xavier Worthy", "Stefon Diggs", "Deebo Samuel",
    "Travis Kelce")),

  # Owns Nico Collins AND Chris Olave. Bench not seen in the app - starters only, so their
  # side of any trade is indicative, not exact.
  Rowdy17 = list(complete = FALSE, players = c(
    "Caleb Williams",
    "Saquon Barkley", "Javonte Williams",
    "Nico Collins", "Tee Higgins", "Chris Olave",
    "Sam LaPorta")),

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
    "A.J. Brown", "George Pickens", "Rashee Rice", "Quentin Johnston",
    "Trey McBride"))
)

# Players who cannot be counted on right now (OUT / IR). The analyzer zeroes them so a
# trade is never justified by a player who is not playing.
unavailable <- c("Zay Flowers", "A.J. Brown")
