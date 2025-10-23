# Clear console and workspace
cat("\014")
rm(list = ls())

# Install and load required packages
packages <- c(
  "XML", "RCurl", "stringr", "rjson", "plyr", "dplyr", "httr", 
  "jsonlite", "magrittr", "googlesheets4", "googledrive", "lubridate", "base64enc"
)

installed_pkgs <- installed.packages()[, "Package"]
to_install <- setdiff(packages, installed_pkgs)
if (length(to_install) > 0) install.packages(to_install)

lapply(packages, library, character.only = TRUE)

source("functions.R")

# Authenticate with Google Sheets
authenticate_google_sheets <- function() {
  json_key_base64 <- Sys.getenv("GCP_SHEETS_KEY_B64")
  json_key <- rawToChar(base64enc::base64decode(json_key_base64))
  temp_json_file <- tempfile(fileext = ".json")
  writeLines(json_key, temp_json_file)
  gs4_auth(path = temp_json_file)
}

# Normalize team names
normalize_teams <- function(df) {
  recode(df$Team,
         "GSW" = "GS", "NYK" = "NY", "PHO" = "PHX", 
         "NOR" = "NO", "NOP" = "NO", "SAN" = "SA", "SAS" = "SA")
}

# Extract and process slate data from API
get_clean_df <- function(endpoint) {
  # Fetch data from API
  res <- GET(endpoint, add_headers(Authorization = "FantasySixPack", `Content-Type` = "application/json"))
  data <- content(res, "parsed", simplifyVector = TRUE)
  slates <- data$slates
  
  # Find MAIN slate
  text_cols <- names(slates)[sapply(slates, is.character)]
  slate_col <- text_cols[which(sapply(text_cols, function(col) any(grepl("MAIN", slates[[col]], ignore.case = TRUE))))]
  
  if (length(slate_col) == 0) stop("No relevant column found containing slate names.")
  
  slate_index <- which(grepl("MAIN|ALL DAY|ALL", slates[[slate_col[1]]], ignore.case = TRUE))[1]
  if (is.na(slate_index)) {
    # Fallback: use slate with most players
    slate_lengths <- sapply(slates$info, nrow)
    slate_index <- which.max(slate_lengths)
    message(sprintf("MAIN slate not found. Using slate with %d players.", slate_lengths[slate_index]))
  }
  
  # Extract player data
  df <- data$slates$info[[slate_index]]
  names(df) <- c("Start Time", "Opp", "Player", "ID", "Pos", "Team", "Proj", "Salary", "Beta", "Value")
  
  # Process and filter data
  df <- df %>%
    mutate(
      Proj = round(as.numeric(Proj), 2),
      Salary = as.numeric(Salary),
      Value = round(as.numeric(Value), 1)
    ) %>%
    select(Player, Pos, Team, Opp, Salary, Proj, Value) %>%
    filter(!is.na(Proj) & Proj >= 5) %>%
    arrange(desc(Proj))
  
  # Normalize names
  df$Player <- sapply(df$Player, replaceName)
  df$Team <- normalize_teams(df)
  
  # Remove any NA values
  df <- df[!is.na(df$Player) & !is.na(df$Proj), ]
  
  # Reorder columns for output
  df <- df %>%
    select(Player, Proj, Salary, Value, Pos, Team, Opp)
  
  return(df)
}

# Main execution
main <- function() {
  authenticate_google_sheets()
  sheet_id <- "1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8"
  
  # Process FanDuel
  fd <- get_clean_df("https://bluecollardfs.com/api/nba_fanduel")
  sheet_write(fd, sheet = "FD NBA DFS", ss = sheet_id)
  
  # Process DraftKings
  dk <- get_clean_df("https://bluecollardfs.com/api/nba_draftkings")
  sheet_write(dk, sheet = "DK NBA DFS", ss = sheet_id)
  
  # Update timestamp
  now_et <- with_tz(Sys.time(), "America/New_York")
  range_write(ss = sheet_id, data = data.frame(Date = format(now_et, "%B %d, %Y")), 
              sheet = "NBA Update Time", range = "A2", col_names = FALSE)
  range_write(ss = sheet_id, data = data.frame(Time = format(now_et, "%I:%M %p ET")), 
              sheet = "NBA Update Time", range = "B2", col_names = FALSE)
}

main()
