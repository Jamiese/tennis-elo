# function to get the elo by selecting the ranking of the player ie. the player
# with hightest elo will be 1, second will be 2 etc. 
# Surface=1 is Hard, surface=2 is clay and surface=3 is grass. Does not work yet
# because it does not recognize player names
get_elo <- function(ranking1, surface) {
  if (surface == 1) {
    pelo = elo$jsehard[ranking1]
  }
  else if (surface == 2) {
    pelo = elo$jseclay[ranking1]
  } else {
    pelo = elo$jsegrass[ranking1]
  }
  
  print(elo$Player[ranking1])
  print(pelo)
  return(pelo)
}


# Function to get the odds based off elos
get_odds <- function(elo1, elo2) {
  temp = 1 + 10^((elo2-elo1)/400)
  prob1 = round(1/temp, 5)
  prob2 = round(1 - prob1, 5)
  odds1 = round(1/prob1, 2)
  odds2 = round(1/prob2, 2)
  print("Player1 prob/odds = ")
  print(prob1)
  print(odds1)
  
  print("Player2 prob/odds = ")
  print(prob2)
  print(odds2)
  return(c(odds1, odds2))
}


kelly_bet <- function()



elo = read.csv("elo.csv", fileEncoding="UTF-8-BOM")
yelo = read.csv("yelo.csv", fileEncoding="UTF-8-BOM")

# Set surface values to numeric
elo$HardRaw = as.numeric(elo$HardRaw)
elo$ClayRaw = as.numeric(elo$ClayRaw)
elo$GrassRaw = as.numeric(elo$GrassRaw)


elo$yelo = 0
# Combines yelo into elo df
i=1
j=1
while (i < length(yelo$Player)) { 
  while (j < length(elo$Player)) {
    if (yelo$Player[i] == elo$Player[j]) {
      elo$yelo[j] = yelo$yElo[i]
      i=i+1
      j=1
    } else {
        j = j + 1
    }
    if (i >= length(yelo$Player)) {
      break
    }
  } 
  j=1
  i=i+1
}


# Create new myelo values based on elo surfaceelo and yelo
i = 1
while (i < length(elo$Player)) {
  if (elo$yelo[i] > 0) {
    elo$jsehard[i] = 0.4*elo$Elo[i] + 0.2*as.numeric(elo$HardRaw[i]) + 0.4*elo$yelo[i]
    elo$jseclay[i] = 0.4*elo$Elo[i] + 0.2*elo$ClayRaw[i] + 0.4*elo$yelo[i]
    elo$jsegrass[i] = 0.4*elo$Elo[i] + 0.2*elo$GrassRaw[i] + 0.4*elo$yelo[i]
  } else { 
    elo$jsehard[i] = 0.78*elo$Elo[i] + 0.2*elo$HardRaw[i]
    elo$jseclay[i] = 0.78*elo$Elo[i] + 0.2*elo$ClayRaw[i]
    elo$jsegrass[i] = 0.78*elo$Elo[i] + 0.2*elo$GrassRaw[i]
  }
  i = i + 1
}

# script to get elos and odds. To do this enter the rankings of the players 
# and the surface they are playing on with surface = 1 being hard, surface=2 being
# clay and surface =3 being grass
surface = 1
player1ranking = 260
player2ranking = 200
bankroll = 1000
betterodds = 0
player1elo = get_elo(player1ranking, surface)
player2elo = get_elo(player2ranking, surface)
odds = get_odds(player1elo, player2elo)
my.read = readline(prompt="Better odds online? 1=y, 0=n: ")
betterodds = as.integer(my.read)
if (betterodds == 1) {
  my.read = readline(prompt="Are online odds better for player1 or player2? (1/2)")
  whichplayer = as.integer(my.read)
  my.read = readline(prompt = "Enter the player's payout odds: ")
  b = as.double(my.read)
  if (whichplayer == 1) {
    f=odds[1]-(odds[2]/b)
  } else {
    f=odds[2]-(odds[1]/b)
  }
  print("You should bet: ")
  print(bankroll * f * 0.25)
}





