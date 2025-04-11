replaceName <- function(name) {
  if (name == "Juan Jose Barea" | name == "Jose Juan Barea" | name == "Jose Barea") {name <- "J.J. Barea"}
  else if (name == "Steph Curry") {name <- "Stephen Curry"}
  else if (name == "Mohamed Bamba") {name <- "Mo Bamba"}
  else if (name == "Marcus Morris Sr." | name == "Marcus Morris Sr") {name <- "Marcus Morris"}
  else if (name == "Robert Williams III") {name <- "Robert Williams"}
  else if (name == "Kyle O`Quinn") {name <- "Kyle O'Quinn"}
  else if (name == "PJ Tucker") {name <- "P.J. Tucker"}
  else if (name == "CJ McCollum") {name <- "C.J. McCollum"}
  else if (name == "JJ Redick") {name <- "J.J. Redick"}
  else if (name == "Nah'Shon Hyland") {name <- "Bones Hyland"}
  else if (name == "Cam Thomas") {name <- "Cameron Thomas"}
  else if (name == "Cam Payne") {name <- "Cameron Payne"}
  else if (name == "Brandon Boston Jr." | name == "Brandon Boston Jr") {name <- "Brandon Boston"}
  else if (name == "P.J. Dozier") {name <- "PJ Dozier"}
  else if (name == "Vít Krejčí") {name <- "Vit Krejci"}
  else if (name == "A.J. Griffin") {name <- "AJ Griffin"}
  else if (name == "Trey Murphy III") {name <- "Trey Murphy"}
  else if (name == "Nic Claxton") {name <- "Nicolas Claxton"}
  else if (name == "Luka Dončić") {name <- "Luka Doncic"}
  else if (name == "Nikola Jokić") {name <- "Nikola Jokic"}
  else if (name == "Jonas Valančiūnas") {name <- "Jonas Valanciunas"}
  else if (name == "Alexandre Sarr" | name == "Alex Sarr") {name <- "Alex Sarr"}
  else if (name == "Carlton Carrington") {name <- "Bub Carrington"}
  else if (name == "Ron Holland II") {name <- "Ron Holland"}
  else if (name == "Kel'El Ware") {name <- "Kel'el Ware"}
  else if (name == "Jabari Smith Jr." | name == "Jabari Smith Jr") {name <- "Jabari Smith"}
  else if (name == "CJ Elleby") {name <- "C.J. Elleby"}
  else if (name == "Tristan Da Silva") {name <- "Tristan da Silva"}
  else if (name == "David Duke Jr." | name == "David Duke Jr") {name <- "David Duke"}
  else if (name == "KJ Martin Jr." | name == "KJ Martin" | name == "K.J. Martin Jr." | name == "KJ Martin Jr" | name == "K.J. Martin Jr" | name == "Kenyon Martin") {name <- "Kenyon Martin Jr."}
  else if (name == "R.J. Hampton Jr." | name == "RJ Hampton" | name == "RJ Hampton Jr" | name == "R.J. Hampton Jr") {name <- "R.J. Hampton"}
  else if (name == "Russell Westbrook III") {name <- "Russell Westbrook"}
  else if (name == "Aleksej Pokuševski") {name <- "Aleksej Pokusevski"}
  else if (name == "Otto Porter Jr." | name == "Otto Porter Jr") {name <- "Otto Porter"}
  else if (name == "Vernon Carey Jr." | name == "Vernon Carey Jr") {name <- "Vernon Carey"}
  else if (name == "Bobby Portis Jr." | name == "Bobby Portis Jr") {name <- "Bobby Portis"}
  else if (name == "Kevin Knox II") {name <- "Kevin Knox"}
  else if (name == "Lonnie Walker IV") {name <- "Lonnie Walker"}
  else if (name == "Terence Davis II") {name <- "Terence Davis"}
  else if (name == "Kira Lewis Jr." | name == "Kira Lewis Jr") {name <- "Kira Lewis"}
  else if (name == "Kevin Porter Jr." | name == "Kevin Porter Jr") {name <- "Kevin Porter"}
  else if (name == "Michael Porter Jr." | name == "Michael Porter Jr") {name <- "Michael Porter"}
  else if (name == "Bruce Brown Jr." | name == "Bruce Brown Jr") {name <- "Bruce Brown"}
  else if (name == "Gary Trent Jr." | name == "Gary Trent Jr") {name <- "Gary Trent"}
  else if (name == "Frank Mason III") {name <- "Frank Mason"}
  else if (name == "Harry Giles III") {name <- "Harry Giles"}
  else if (name == "Deyonta Davis ") {name <- "Deyonta Davis"}
  else if (name == "Guillermo Hernangomez") {name <- "Willy Hernangomez"}
  else if (name == "Cristiano Da Silva Felicio") {name <- "Cristiano Felicio"}
  else if (name == "Nene" | name == "Nene Hilario " | name == "Nene ") {name <- "Nene Hilario"}
  else if (name == "Kelly Oubre Jr." | name == "Kelly Oubre Jr") {name <- "Kelly Oubre"}
  else if (name == "Wendell Carter Jr." | name == "Wendell Carter Jr") {name <- "Wendell Carter"}
  else if (name == "Ty Wallace") {name <- "Tyrone Wallace"}
  else if (name == "TyTy Washington Jr.") {name <- "TyTy Washington"}
  else if (name == "O.G. Anunoby") {name <- "OG Anunoby"}
  else if (name == "Frank Kaminsky III") {name <- "Frank Kaminsky"}
  else if (name == "Devonte' Graham") {name <- "Devonte Graham"}
  else if (name == "Sviatoslav Mykhailiuk") {name <- "Svi Mykhailiuk"}
  else if (name == "Troy Brown Jr." | name == "Troy Brown Jr") {name <- "Troy Brown"}
  else if (name == "Xavier Tillman Sr." | name == "Xavier Tillman Sr") {name <- "Xavier Tillman"}
  else if (name == "Andre Jackson Jr." | name == "Andre Jackson Jr") {name <- "Andre Jackson"}
  else if (name == "Herb Jones") {name <- "Herbert Jones"}
  else if (name == "Marvin Bagley III") {name <- "Marvin Bagley"}
  else if (name == "Ömer Yurtseven") {name <- "Omer Yurtseven"}
  else if (name == "Danuel House Jr.") {name <- "Danuel House"}
  else if (name == "Glenn Robinson III") {name <- "Glenn Robinson"}
  else if (name == "Fred Van Vleet" | name == "Fred Van") {name <- "Fred VanVleet"}
  else if (name == "Alperen Sengün" | name == "Alperen Sengün") {name <- "Alperen Sengun"}
  else if (name == "Greg Brown III") {name <- "Greg Brown"}
  else if (name == "G.G. Jackson" | name == "Gregory Jackson" | name == "GG Jackson II") {name <- "GG Jackson"}
  else if (name == "James Ennis III") {name <- "James Ennis"}
  else if (name == "Jacob Evans III") {name <- "Jacob Evans"}
  else if (name == "R.J. Barrett") {name <- "RJ Barrett"}
  else if (name == "Duane Washington Jr.") {name <- "Duane Washington"}
  else if (name == "Jaren Jackson Jr." | name == "Jaren Jackson Jr") {name <- "Jaren Jackson"}
  else if (name == "Wes Iwundu") {name <- "Wesley Iwundu"}
  else if (name == "Théo Maledon") {name <- "Theo Maledon"}
  else if (name == "Thomas Bryant ") {name <- "Thomas Bryant"}
  else if (name == "Jalen Brunson ") {name <- "Jalen Brunson"}
  else if (name == "Raulzinho Neto") {name <- "Raul Neto"}
  else if (name == "Taurean Waller-Prince") {name <- "Taurean Prince"}
  else if (name == "Tomas Satoransky") {name <- "Tomas Satoransky"}
  else if (name == "Moe Harkless") {name <- "Maurice Harkless"}
  else if (name == "Dennis Smith Jr." | name == "Dennis Smith," | name == "Dennis Smith Jr") {name <- "Dennis Smith"}
  else if (name == "Derrick Jones Jr." | name == "Derrick Jones Jr") {name <- "Derrick Jones"}
  else if (name == "Louis Williams") {name <- "Lou Williams"}
  #else if (name == "Herbert Jones") {name <- "Herb Jones"}
  else if (name == "Shaq Harrison") {name <- "Shaquille Harrison"}
  else if (name == "Cam Johnson") {name <- "Cameron Johnson"}
  else if (name == "Ishmael Smith") {name <- "Ish Smith"}
  else if (name == "Gary Payton II") {name <- "Gary Payton"}
  else if (name == "Patty Mills") {name <- "Patrick Mills"}
  else if (name == "Enes Freedom" | name == "Enes Kanter Freedom") {name <- "Enes Kanter"}
  else if (name == "Domas Sabonis") {name <- "Domantas Sabonis"}
  else if (name == "TJ Warren") {name <- "T.J. Warren"}
  else if (name == "J.T. Thor") {name <- "JT Thor"}
  else if (name == "Aleksandar Vezenkov") {name <- "Sasha Vezenkov"}
  else if (name == "PJ Washington" | name == "P.J. Washington Jr.") {name <- "P.J. Washington"}
  else if (name == "TJ Leaf") {name <- "T.J. Leaf"}
  else if (name == "Luc Richard Mbah a Moute") {name <- "Luc Mbah a Moute"}
  else if (name == "DAngelo Russell") {name <- "D'Angelo Russell"}
  else if (name == "Clint N`Dumba-Capela") {name <- "Clint Capela"}
  else if (name == "Brad Beal") {name <- "Bradley Beal"}
  else if (name == "Tim Hardaway Jr." | name == "Tim Hardaway Jr") {name <- "Tim Hardaway"}
  else if (name == "DeAndre' Bembry" | name == "Deandre' Bembry" | name == "Deandre Bembry") {name <- "DeAndre Bembry"}
  else if (name == "Wayne Selden") {name <- "Wayne Selden Jr."}
  else if (name == "DeAndre Ayton") {name <- "Deandre Ayton"}
  else if (name == "Larry Nance Jr." | name == "Larry Nance Jr") {name <- "Larry Nance"}
  else if (name == "E`Twaun Moore") {name <- "E'Twaun Moore"}
  else {name}
}

# function to calculate standard error from sample of means
calcSE <- function(x) {
  n <- length(x[!is.na(x)])
  if (n > 1) {
    y <- sd(x, na.rm = T)
  } else {
    y <- 0
  }
}

# function to calculate scoring bonuses on DraftKings
bonus <- function(x) {
  if (x$PTS >= 10 & x$TRB >= 10 & (x$AST >= 10 | x$BLK >= 10 | x$STL >= 10)) {
    y <- 4.5
  } else if (x$PTS >= 10 & (x$TRB >= 10 | x$AST >= 10 | x$BLK >= 10 | x$STL >= 10)) {
    y <- 1.5
  } else {
    y <- 0
  }
}

teamAbbr <- function(team) {
  if (team == "Atlanta" | team == "Atlanta Hawks") {team <- "ATL"}
  else if (team == "Boston" | team == "Boston Celtics") {team <- "BOS"}
  else if (team == "Brooklyn" | team == "Brooklyn Nets") {team <- "BKN"}
  else if (team == "Charlotte" | team == "Charlotte Hornets") {team <- "CHA"}
  else if (team == "Chicago" | team == "Chicago Bulls") {team <- "CHI"}
  else if (team == "Cleveland" | team == "Cleveland Cavaliers") {team <- "CLE"}
  else if (team == "Dallas" | team == "Dallas Mavericks") {team <- "DAL"}
  else if (team == "Denver" | team == "Denver Nuggets") {team <- "DEN"}
  else if (team == "Detroit" | team == "Detroit Pistons") {team <- "DET"}
  else if (team == "Golden State" | team == "Golden State Warriors" | team == "GSW") {team <- "GS"}
  else if (team == "Houston" | team == "Houston Rockets") {team <- "HOU"}
  else if (team == "Indiana" | team == "Indiana Pacers") {team <- "IND"}
  else if (team == "LA Clippers" | team == "Los Angeles Clippers") {team <- "LAC"}
  else if (team == "LA Lakers" | team == "Los Angeles Lakers") {team <- "LAL"}
  else if (team == "Memphis" | team == "Memphis Grizzlies") {team <- "MEM"}
  else if (team == "Miami" | team == "Miami Heat") {team <- "MIA"}
  else if (team == "Milwaukee" | team == "Milwaukee Bucks") {team <- "MIL"}
  else if (team == "Minnesota" | team == "Minnesota Timberwolves") {team <- "MIN"}
  else if (team == "New Orleans" | team == "New Orleans Pelicans") {team <- "NO"}
  else if (team == "New York" | team == "New York Knicks") {team <- "NY"}
  else if (team == "Oklahoma City" | team == "Oklahoma City Thunder") {team <- "OKC"}
  else if (team == "Orlando" | team == "Orlando Magic") {team <- "ORL"}
  else if (team == "Philadelphia" | team == "Philadelphia 76ers") {team <- "PHI"}
  else if (team == "Phoenix" | team == "Phoenix Suns") {team <- "PHO"}
  else if (team == "Portland" | team == "Portland Trail Blazers") {team <- "POR"}
  else if (team == "Sacramento" | team == "Sacramento Kings") {team <- "SAC"}
  else if (team == "San Antonio" | team == "San Antonio Spurs") {team <- "SA"}
  else if (team == "Toronto" | team == "Toronto Raptors") {team <- "TOR"}
  else if (team == "Utah" | team == "Utah Jazz") {team <- "UTA"}
  else if (team == "Washington" | team == "Washington Wizards") {team <- "WAS"}
}