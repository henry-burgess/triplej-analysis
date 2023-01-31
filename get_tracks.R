library(httr)
library(stringr)
library(dplyr)
library(tibble)

# Query components
BASE <- "http://music.abcradio.net.au/api/v1/plays/search.json"
RESULT_LIMIT <- 100
RESULT_PAGE <- 0
STATION <- "triplej"
RESULT_FROM <- "2022-06-30T00:00:00.000Z"
RESULT_TO <- "2022-07-30T00:00:00.000Z"

# Generate initial query
query <- str_c(c(BASE, "?from=", RESULT_FROM, "&limit=", RESULT_LIMIT, "&offset=", 0, "&page=", RESULT_PAGE, "&station=", STATION, "&to=", RESULT_TO), collapse = "")

# Execute query, looping over limit until total number of tracks received
response <- GET(query, accept_json())
parsed <- content(response, as="parsed", type="application/json")
track_count <- parsed$total
print(str_c(track_count, "tracks", sep = " "))

# Tibble to store all tracks
all_tracks <- tibble(artist = "", title = "")

counter <- 0
while (counter < track_count) {
  offset <- counter
  completion <- str_c(round(offset / track_count * 100), "%")
  print(str_c("Retrieved", offset, "tracks,", completion, "complete", sep = " "))
  
  # Create new query using updated offset
  query <- str_c(c(BASE, "?from=", RESULT_FROM, "&limit=", RESULT_LIMIT, "&offset=", offset, "&page=", RESULT_PAGE, "&station=", STATION, "&to=", RESULT_TO), collapse = "")
  response <- GET(query, accept_json())
  parsed <- content(response, as="parsed", type="application/json")
  response_tracks <- parsed[["items"]]
  
  # Collate all data into the one data frame
  for (i in 1:length(response_tracks)) {
    track_data <- response_tracks[[i]]$recording
    all_tracks <- all_tracks %>%
      add_row(artist = track_data$artists[[1]]$name, title = track_data$title)
  }

  counter <- counter + RESULT_LIMIT
}

# Create top artist summary
top_artists <- all_tracks %>%
                  group_by(artist) %>%
                  summarise(n = n()) %>%
                  arrange(desc(n))
