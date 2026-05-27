# Clear console and workspace
cat("\014")
rm(list = ls())

packages <- c(
  "XML", "RCurl", "stringr", "rjson", "plyr", "dplyr", "httr", 
  "jsonlite", "magrittr", "googlesheets4", "googledrive", "lubridate", "base64enc"
)
lapply(packages, library, character.only = TRUE)

source("functions.R")

authenticate_google_sheets <- function() {
  json_key_base64 <- Sys.getenv("GCP_SHEETS_KEY_B64")
  json_key <- rawToChar(base64enc::base64decode(json_key_base64))
  temp_json_file <- tempfile(fileext = ".json")
  writeLines(json_key, temp_json_file)
  gs4_auth(path = temp_json_file)
}

normalize_teams <- function(df) {
  recode(df$Team,
         "GSW" = "GS", "NYK" = "NY", "PHO" = "PHX", 
         "NOR" = "NO", "NOP" = "NO", "SAN" = "SA", "SAS" = "SA")
}

get_clean_df <- function(endpoint) {
  api_key <- paste0("ApiKey ", Sys.getenv("BCDFS_API_KEY"))
  res <- GET(endpoint, add_headers(Authorization = api_key, `Content-Type` = "application/json"))
  data <- content(res, "parsed", simplifyVector = TRUE)
  slates <- data$slates
  
  slate_index <- NULL
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
  
  if (is.null(slate_index) || is.na(slate_index)) {
    if (!is.null(slates$info) && length(slates$info) > 0) {
      slate_lengths <- sapply(slates$info, function(x) if(is.data.frame(x)) nrow(x) else 0)
      slate_index <- which.max(slate_lengths)
      message(sprintf("MAIN slate not found. Using slate with most players (%d players).", slate_lengths[slate_index]))
    } else {
      stop("No slates with player data found in API response")
    }
  }
  
  df <- data$slates$info[[slate_index]]
  names(df) <- c("Start Time", "Opp", "Player", "ID", "Pos", "Team", "Proj", "Salary", "Beta", "Value")
  
  df <- df %>%
    mutate(
      Proj = round(as.numeric(Proj), 2),
      Salary = as.numeric(Salary),
      Value = round(as.numeric(Value), 1)
    ) %>%
    select(Player, Pos, Team, Opp, Salary, Proj, Value) %>%
    filter(!is.na(Proj) & Proj >= 5) %>%
    arrange(desc(Proj))
  
  df$Player <- sapply(df$Player, replaceName)
  df$Team <- normalize_teams(df)
  df <- df[!is.na(df$Player) & !is.na(df$Proj), ]
  df <- df %>% select(Player, Proj, Salary, Value, Pos, Team, Opp)
  
  return(df)
}

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

main <- function() {
  authenticate_google_sheets()
  sheet_id <- "1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8"
  results <- list()
  
  results$dk <- process_site("DraftKings", "https://bluecollardfs.com/api/nba_draftkings", "DK NBA DFS", sheet_id)
  results$fd <- process_site("FanDuel",    "https://bluecollardfs.com/api/nba_fanduel",    "FD NBA DFS", sheet_id)
  
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
  
  message("\n=== Update Summary ===")
  message(sprintf("DraftKings: %s", ifelse(results$dk, "SUCCESS", "FAILED")))
  message(sprintf("FanDuel: %s",    ifelse(results$fd, "SUCCESS", "FAILED")))
  
  if (!results$dk && !results$fd) {
    warning("Both DraftKings and FanDuel updates failed")
    return(invisible(FALSE))
  }
  return(invisible(TRUE))
}

tryCatch({
  main()
}, error = function(e) {
  message(sprintf("\n✗ Critical error: %s", e$message))
})
