# Clear console and workspace
cat("\014")
rm(list = ls())

# Install and load required packages
packages <- c(
  "XML", "RCurl", "stringr", "rjson", "plyr", "dplyr", "httr", 
  "jsonlite", "magrittr", "googlesheets4", "googledrive", "lubridate", "base64enc"
)

# Install any missing packages
installed_pkgs <- installed.packages()[, "Package"]
to_install <- setdiff(packages, installed_pkgs)
if (length(to_install) > 0) install.packages(to_install)

# Load all packages
lapply(packages, library, character.only = TRUE)

source("functions.R")

# Function to authenticate with Google Sheets using GCP service account key
authenticate_google_sheets <- function() {
  json_key_base64 <- Sys.getenv("GCP_SHEETS_KEY_B64")
  json_key <- rawToChar(base64enc::base64decode(json_key_base64))
  temp_json_file <- tempfile(fileext = ".json")
  writeLines(json_key, temp_json_file)
  gs4_auth(path = temp_json_file)
}

# Function to extract slate data from an API
extract_slate_data <- function(api_url, header_key) {
  response <- GET(api_url, add_headers(Authorization = header_key, `Content-Type` = "application/json"))
  data <- content(response, "parsed", simplifyVector = TRUE)
  slates <- data$slates
  
  if (length(slates) == 0) stop("No slates found in the API response.")
  
  # Try to find the 'MAIN' slate
  slate_names <- sapply(slates$info, function(s) s$name)
  main_index <- which(grepl("MAIN", slate_names, ignore.case = TRUE))
  
  # If found, check if it has players
  if (length(main_index) > 0) {
    main_slate <- slates$info[[main_index]]
    if (!is.null(main_slate$players) && length(main_slate$players) > 0) {
      return(main_slate)
    }
  }
  
  # Fallback: find slate with the most players
  slate_lengths <- sapply(slates$info, function(s) length(s$players))
  best_index <- which.max(slate_lengths)
  
  message(sprintf("MAIN slate was empty or not found. Using slate: %s with %d players.", 
                  slates$info[[best_index]]$name, slate_lengths[best_index]))
  
  return(slates$info[[best_index]])
}

# Function to process player data
process_player_data <- function(df) {
  names(df) <- c("Start Time", "Opp", "Player", "ID", "Pos", "Team", "Proj", "Salary", "Beta", "Value")
  
  df$Proj <- round(as.numeric(df$Proj), 2)
  df$Salary <- as.numeric(df$Salary)
  df$Value <- round(as.numeric(df$Value), 1)
  
  # Retain relevant columns
  df <- df[, c("Player", "Pos", "Team", "Opp", "Salary", "Proj", "Value")]
  
  # Remove NA values and filter for projected scores >= 5
  df <- df[!is.na(df$Proj) & df$Proj >= 5, ]
  
  # Arrange players by highest projections
  df <- arrange(df, desc(Proj))
  
  # Normalize player and team names
  df$Player <- sapply(df$Player, replaceName)
  df$Team <- recode(df$Team, "GSW" = "GS", "NYK" = "NY", "PHO" = "PHX", "NOR" = "NO", "NOP" = "NO", "SAN" = "SA", "SAS" = "SA")
  
  # Remove any remaining NA values
  df <- df[!is.na(df$Player) & !is.na(df$Proj), ]

  # Reorder columns to desired output format
  df <- df %>%
    select(Player, Proj, Salary, Value, Pos, Team, Opp)
  
  return(df)
}

# Function to write data to Google Sheets
write_to_sheets <- function(df, sheet_url, sheet_name) {
  sheet_write(df, sheet = sheet_name, ss = sheet_url)
}

# Function to update timestamp in "NBA Update Time" sheet
update_timestamp <- function(sheet_url_time) {
  update_time <- with_tz(Sys.time(), "America/New_York")
  formatted_date <- format(update_time, "%B %d, %Y")
  formatted_time <- format(update_time, "%I:%M %p ET")
  
  range_write(ss = sheet_url_time, data = data.frame(Date = formatted_date), sheet = "NBA Update Time", range = "A2", col_names = FALSE)
  range_write(ss = sheet_url_time, data = data.frame(Time = formatted_time), sheet = "NBA Update Time", range = "B2", col_names = FALSE)
}

# Main function to execute the entire workflow
main <- function() {
  authenticate_google_sheets()
  sheet_url_fd <- "https://docs.google.com/spreadsheets/d/1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8/edit#gid=0"
  sheet_url_time <- "https://docs.google.com/spreadsheets/d/1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8/edit#gid=1047637228"
  
  # Get FanDuel data
  fd_data <- extract_slate_data("https://bluecollardfs.com/api/nba_fanduel", "FantasySixPack")
  fd_processed <- process_player_data(fd_data)
  write_to_sheets(fd_processed, sheet_url_fd, "FD NBA DFS")
  
  # Get DraftKings data
  dk_data <- extract_slate_data("https://bluecollardfs.com/api/nba_draftkings", "FantasySixPack")
  dk_processed <- process_player_data(dk_data)
  write_to_sheets(dk_processed, sheet_url_fd, "DK NBA DFS")
  
  # Update timestamp
  update_timestamp(sheet_url_time)
}

# Run the main function
main()
