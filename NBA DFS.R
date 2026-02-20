# Clear console and workspace
cat("\014")
rm(list = ls())

# Install and load required packages
packages <- c(
  "XML", "RCurl", "stringr", "rjson", "plyr", "dplyr", "httr", 
  "jsonlite", "magrittr", "googlesheets4", "googledrive", "lubridate", "base64enc"
)

#installed_pkgs <- installed.packages()[, "Package"]
#to_install <- setdiff(packages, installed_pkgs)
#if (length(to_install) > 0) install.packages(to_install)

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
  slate_index <- NULL
  
  # First, try to find a slate with MAIN, ALL DAY, or ALL in the name
  text_cols <- names(slates)[sapply(slates, is.character)]
  if (length(text_cols) > 0) {
    for (col in text_cols) {
      matches <- which(grepl("MAIN|ALL DAY|ALL", slates[[col]], ignore.case = TRUE))
      if (length(matches) > 0) {
        slate_index <- matches[1]
        message(sprintf("Found slate: '%s'", slates[[col]][slate_index]))
        break
      }
    }
  }
  
  # Fallback: use slate with most players
  if (is.null(slate_index) || is.na(slate_index)) {
    if (!is.null(slates$info) && length(slates$info) > 0) {
      slate_lengths <- sapply(slates$info, function(x) if(is.data.frame(x)) nrow(x) else 0)
      slate_index <- which.max(slate_lengths)
      message(sprintf("MAIN slate not found. Using slate with most players (%d players).", slate_lengths[slate_index]))
    } else {
      stop("No slates with player data found in API response")
    }
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

# Process a single site with error handling
process_site <- function(site_name, endpoint, sheet_name, sheet_id) {
  tryCatch({
    message(sprintf("Processing %s...", site_name))
    df <- get_clean_df(endpoint)
    sheet_write(df, sheet = sheet_name, ss = sheet_id)
    message(sprintf("✓ %s updated successfully (%d players)", site_name, nrow(df)))
    return(TRUE)
  }, error = function(e) {
    message(sprintf("✗ %s failed: %s", site_name, e$message))
    return(FALSE)
  })
}

# Main execution
main <- function() {
  authenticate_google_sheets()
  sheet_id <- "1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8"
  
  # Track success status
  results <- list()
  
  # Process DraftKings first (usually more reliable)
  results$dk <- process_site(
    site_name = "DraftKings",
    endpoint = "https://bluecollardfs.com/api/nba_draftkings",
    sheet_name = "DK NBA DFS",
    sheet_id = sheet_id
  )
  
  # Process FanDuel (independent of DraftKings result)
  results$fd <- process_site(
    site_name = "FanDuel",
    endpoint = "https://bluecollardfs.com/api/nba_fanduel",
    sheet_name = "FD NBA DFS",
    sheet_id = sheet_id
  )
  
  # Update timestamp only if at least one site succeeded
  if (results$dk || results$fd) {
    tryCatch({
      now_et <- with_tz(Sys.time(), "America/New_York")
      range_write(ss = sheet_id, data = data.frame(Date = format(now_et, "%B %d, %Y")), 
                  sheet = "NBA Update Time", range = "A2", col_names = FALSE)
      range_write(ss = sheet_id, data = data.frame(Time = format(now_et, "%I:%M %p ET")), 
                  sheet = "NBA Update Time", range = "B2", col_names = FALSE)
      message("✓ Timestamp updated")
    }, error = function(e) {
      message(sprintf("✗ Timestamp update failed: %s", e$message))
    })
  }
  
  # Summary message
  message("\n=== Update Summary ===")
  message(sprintf("DraftKings: %s", ifelse(results$dk, "SUCCESS", "FAILED")))
  message(sprintf("FanDuel: %s", ifelse(results$fd, "SUCCESS", "FAILED")))
  
  # Return exit code: 0 if at least one succeeded, warning if both failed
  if (!results$dk && !results$fd) {
    warning("Both DraftKings and FanDuel updates failed")
    return(invisible(FALSE))
  }
  
  return(invisible(TRUE))
}

# Run main with top-level error handling for GitHub Actions
tryCatch({
  main()
}, error = function(e) {
  message(sprintf("\n✗ Critical error: %s", e$message))
  # Don't stop() or quit() - just let it exit normally with code 0
})


