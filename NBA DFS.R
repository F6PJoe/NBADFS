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

normalize_teams <- function(teams) {
  recode(teams,
         "GSW" = "GS", "NYK" = "NY", "PHO" = "PHX", 
         "NOR" = "NO", "NOP" = "NO", "SAN" = "SA", "SAS" = "SA")
}

get_clean_df <- function(endpoint) {
  api_key <- paste0("ApiKey ", Sys.getenv("BCDFS_API_KEY"))
  res <- GET(endpoint, add_headers(Authorization = api_key, `Content-Type` = "application/json"))
  data <- content(res, "parsed", simplifyVector = TRUE)
  slates <- data$slates
  
  # Only classic slates; showdown slates carry CPT/UTIL salaries in roster_slots
  classic <- which(slates$slate_type == "classic")
  if (length(classic) == 0) classic <- seq_len(nrow(slates))
  
  # Prefer the MAIN slate, then ALL DAY, then the classic slate with the most players
  slate_index <- NULL
  for (pattern in c("MAIN", "ALL DAY")) {
    matches <- classic[grepl(pattern, slates$slate[classic], ignore.case = TRUE)]
    if (length(matches) > 0) {
      slate_index <- matches[1]
      message(sprintf("Found slate: '%s'", slates$slate[slate_index]))
      break
    }
  }
  
  if (is.null(slate_index)) {
    if (!is.null(slates$info) && length(slates$info) > 0) {
      slate_lengths <- sapply(slates$info[classic], function(x) if(is.data.frame(x)) nrow(x) else 0)
      slate_index <- classic[which.max(slate_lengths)]
      message(sprintf("MAIN slate not found. Using slate with most players (%d players).", max(slate_lengths)))
    } else {
      stop("No slates with player data found in API response")
    }
  }
  
  # Select fields by name -- the API's field order is not stable
  info <- data$slates$info[[slate_index]]
  required <- c("name", "position", "team", "opponent", "salary", "projection", "value")
  missing <- setdiff(required, names(info))
  if (length(missing) > 0) {
    stop(sprintf("API response missing fields: %s (got: %s)",
                 paste(missing, collapse = ", "), paste(names(info), collapse = ", ")))
  }
  
  df <- data.frame(
    Player = info$name,
    Pos    = info$position,
    Team   = info$team,
    Opp    = info$opponent,
    Salary = as.numeric(info$salary),
    Proj   = round(as.numeric(info$projection), 2),
    Value  = round(as.numeric(info$value), 1),
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(Proj) & Proj >= 5) %>%
    arrange(desc(Proj))
  
  # FanDuel sends accented names (Luka Dončić); match DraftKings' plain spelling
  df$Player <- stringi::stri_trans_general(df$Player, "Latin-ASCII")
  df$Player <- sapply(df$Player, replaceName)
  df$Team <- normalize_teams(df$Team)
  df$Opp  <- normalize_teams(df$Opp)
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
