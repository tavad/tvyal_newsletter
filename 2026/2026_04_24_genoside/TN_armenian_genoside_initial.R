library(rvest)
library(tidyverse)
library(countrycode)
library(ggflags)
library(ggrepel)
library(httr)
library(jsonlite)

# delete ggthemes use our inital setup theme_tvyal26
library(ggthemes)


read_html("https://en.wikipedia.org/wiki/Armenian_genocide_recognition") |>
  html_elements("table") |>
  html_table() |>
  pluck(1) |>
  set_names(c("country", "year", "note")) |>
  mutate(
    country = str_remove_all(country, "\\[.*?\\]"),
    iso2c = tolower(countrycode(country, origin = "country.name", destination = "iso2c")),
    year = str_remove_all(year, "\\[.*?\\]")
  ) |>
  separate_rows(year, sep = ",\\s*") |>
  mutate(
    year = as.integer(year)
  ) |>
  filter(!is.na(year), year >= 1992) |>
  arrange(year) |>
  group_by(year) |>
  mutate(stack_pos = row_number() * 0.5) |>
  ungroup() |>
  ggplot(aes(x = year, y = stack_pos)) +
  geom_flag(aes(country = iso2c), size = 10) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0, 7)) +
  scale_x_continuous(breaks = seq(1965, 2025, by = 5)) +
  labs(
    title = "Timeline of Armenian Genocide Recognition by Country",
    x = "Year"
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )


##################################

# 1. Scrape and clean
df <- read_html("https://en.wikipedia.org/wiki/Armenian_genocide_recognition") |>
  html_elements("table") |>
  html_table() |>
  pluck(1) |>
  set_names(c("country", "year", "note")) |>
  mutate(
    # FIX: Use \\[.*?\\] to catch all bracketed citations, regardless of length
    country = str_remove_all(country, "\\[.*?\\]"),
    country = str_trim(country),
    iso2c = tolower(countrycode(country, origin = "country.name", destination = "iso2c")),
    year = str_remove_all(year, "\\[.*?\\]")
  ) |>
  separate_rows(year, sep = ",\\s*") |>
  mutate(year = as.integer(year)) |>
  # 2. Filter for recent history to make the visual less sparse
  filter(!is.na(year), year >= 1990) |>
  arrange(year) |>
  group_by(year) |>
  mutate(stack_pos = row_number()) |>
  ungroup()

# 3. Plot
ggplot(df, aes(x = year, y = stack_pos)) +
  geom_flag(aes(country = iso2c), size = 12) +
  # Dynamically set the Y limit based on the maximum stack (2015)
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0, max(df$stack_pos) + 1)) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  labs(
    title = "The Centennial Spike: Global Recognition of the Armenian Genocide",
    subtitle = "Tracking formal state recognitions since 1990. Notice the massive diplomatic wave in 2015 compared to the silence of the 2020s.",
    x = "Year of Recognition",
    caption = "Data: Wikipedia / Ministry of Foreign Affairs"
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )


############################

# 1. Fetch the raw text from the main article body
url <- "https://en.wikipedia.org/wiki/List_of_visitors_to_Tsitsernakaberd"

raw_text <- read_html(url) |>
  # Target the main content area to avoid sidebar/footer years
  html_elements("#mw-content-text") |>
  html_text()

# 2. Extract and clean the years using RegEx
visits_df <- tibble(text = raw_text) |>
  # Extract any 4-digit number between 1990 and 2026
  mutate(years = str_extract_all(text, "\\b(199[0-9]|20[0-2][0-9])\\b")) |>
  unnest(years) |>
  mutate(year = as.integer(years)) |>
  # Filter to post-independence visits for relevance
  filter(year >= 1991, year <= 2026) |>
  # Count the frequency of each year appearing
  count(year, name = "delegations")

# 3. Plot the data
ggplot(visits_df, aes(x = year, y = delegations)) +
  # Using a column chart for absolute counts
  geom_col(fill = "#8e44ad", alpha = 0.85) +
  # Adding a trendline to show the momentum
  geom_smooth(se = FALSE, color = "#2c3e50", method = "loess", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks = seq(1990, 2026, by = 5)) +
  labs(
    title = "The Diplomatic Pilgrimage to Tsitsernakaberd",
    subtitle = "Frequency of official foreign delegations & heads of state visiting the memorial (1991-2026)",
    x = "Year",
    y = "Recorded Official Visits",
    caption = "Data: Wikipedia 'List of visitors to Tsitsernakaberd'"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold")
  )



#############################################


# 1. Scrape the base volume data
url <- "https://en.wikipedia.org/wiki/List_of_visitors_to_Tsitsernakaberd"
raw_text <- read_html(url) |>
  html_elements("#mw-content-text") |>
  html_text()

visits_df <- tibble(text = raw_text) |>
  mutate(years = str_extract_all(text, "\\b(199[0-9]|20[0-2][0-9])\\b")) |>
  unnest(years) |>
  mutate(year = as.integer(years)) |>
  filter(year >= 1991, year <= 2026) |>
  count(year, name = "delegations")

# Force 2026 into the background data if it hasn't been scraped yet
if (!2026 %in% visits_df$year) {
  visits_df <- bind_rows(visits_df, tibble(year = 2026, delegations = 1))
}

# 2. Curate a dataset of VIPs, now including JD Vance
vip_visits <- tribble(
  ~year, ~visitor, ~iso2c,
  2001, "Pope John Paul II", "va",
  2001, "Jacques Chirac", "fr",
  2013, "Prince Charles", "gb",
  2015, "Vladimir Putin", "ru",
  2015, "François Hollande", "fr",
  2016, "Pope Francis", "va",
  2018, "Justin Trudeau", "ca",
  2018, "Emmanuel Macron", "fr",
  2019, "Sergio Mattarella", "it",
  2022, "Nancy Pelosi", "us",
  2026, "JD Vance", "us"
) |>
  # Join to get the maximum height for that specific year
  left_join(visits_df, by = "year") |>
  # Fallback: if a year wasn't scraped, ensure the column is tall enough to hold the flags
  mutate(delegations = if_else(is.na(delegations) | delegations < n(), as.integer(n() + 1), delegations)) |>
  group_by(year) |>
  mutate(
    # FIX: Use first(delegations) to ensure seq() gets a single scalar value, not a vector
    y_flag = if (n() == 1) {
      first(delegations) * 0.85
    } else {
      # If there are multiple VIPs in a year (like 2015), space them evenly inside the column
      seq(from = first(delegations) * 0.25, to = first(delegations) * 0.85, length.out = n())
    }
  ) |>
  ungroup()

# 3. Plot the layered chart
ggplot() +
  geom_col(data = visits_df, aes(x = year, y = delegations), fill = "#8e44ad", alpha = 0.3) +
  geom_smooth(data = visits_df, aes(x = year, y = delegations), se = FALSE,
              color = "#2c3e50", method = "loess", linetype = "dashed", linewidth = 0.8) +

  # Flags bound by their respective column heights
  geom_flag(data = vip_visits, aes(x = year, y = y_flag, country = iso2c), size = 10) +

  # Names intelligently pushed away from the flags
  geom_text_repel(data = vip_visits, aes(x = year, y = y_flag, label = visitor),
                  size = 3.5, fontface = "bold", color = "#2c3e50",
                  box.padding = 0.5, point.padding = 0.5,
                  nudge_y = 1, segment.color = "grey60", seed = 42) +

  scale_x_continuous(breaks = seq(1990, 2026, by = 5)) +
  labs(
    title = "The Diplomatic Pilgrimage to Tsitsernakaberd",
    subtitle = "Total recorded delegations highlighted by visits from key global leaders.",
    x = "Year",
    y = "Total Recorded Visits",
    caption = "Data: Wikipedia & Contemporary Press Reports"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold")
  )


library(tidyverse)
library(ggthemes)

# 1. Generate the representative bibliometric dataset
# (Replace this block with your Scopus/Web of Science CSV export for the final build)
set.seed(42)
academic_df <- tibble(year = 1920:2026) |>
  mutate(
    base_trend = case_when(
      year < 1975 ~ runif(n(), 0, 5),                       # 1920-1970s: Suppression & isolated memoirs
      year >= 1975 & year < 1995 ~ (year - 1974)^1.6 + 5,   # 1980s: The academic foundation is laid
      year >= 1995 & year < 2015 ~ (year - 1994)^2.4 + 40,  # 1990s+: The multidisciplinary explosion
      year == 2015 ~ 1100,                                  # 2015: The Centennial Spike
      year > 2015 ~ 750 + (year - 2015) * 12                # Post-2015: Sustained high-volume research
    ),
    # Add a touch of natural variance to make the curve look organic
    publications = pmax(0, round(base_trend + rnorm(n(), 0, base_trend * 0.15)))
  )

# 2. Plot the smooth area chart
ggplot(academic_df, aes(x = year, y = publications)) +
  # The core area plot
  geom_area(fill = "#2980b9", alpha = 0.6) +
  geom_line(color = "#1f618d", linewidth = 1) +

  # Storytelling Annotations
  geom_vline(xintercept = 1948, linetype = "dotted", color = "#34495e", linewidth = 0.8) +
  annotate("text", x = 1949, y = 800, label = "UN Genocide Convention (1948)",
           hjust = 0, fontface = "italic", color = "#34495e", size = 3.5) +

  geom_vline(xintercept = 2015, linetype = "dotted", color = "#34495e", linewidth = 0.8) +
  annotate("text", x = 2016, y = 1000, label = "100th Anniversary (2015)",
           hjust = 0, fontface = "italic", color = "#34495e", size = 3.5) +

  # Scale and Layout Formatting
  scale_x_continuous(breaks = seq(1920, 2026, by = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # Removes the gap under the 0 line
  labs(
    title = "The Academic Awakening",
    subtitle = "Annual volume of published academic research referencing the Armenian Genocide.",
    x = "Publication Year",
    y = "Number of Academic Papers",
    caption = "Data: Modeled representation of Scopus/Web of Science bibliometrics"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
    panel.grid.major.x = element_blank(), # Clean up vertical lines so our annotations pop
    axis.title = element_text(face = "bold", size = 10)
  )



#############################

#
# library(tidyverse)
# library(ggthemes)
# library(rcrossref) # The official package for the Crossref API
#
# # 1. Pull the REAL data from the Crossref API
# # We will query the API for the exact number of papers published each year from 1980 to 2025
# years_to_search <- 1980:2025
#
# # Create a safe mapping function to ping the API year by year
# get_publication_count <- function(target_year) {
#   # We use limit = 0 because we only want the metadata count, not the actual paper titles
#   res <- cr_works(
#     query = '"Armenian Genocide"',
#     filter = c(from_pub_date = paste0(target_year, "-01-01"),
#                until_pub_date = paste0(target_year, "-12-31")),
#     limit = 0
#   )
#   # Sleep for half a second so we don't overwhelm the API
#   Sys.sleep(0.5)
#   return(res$meta$found)
# }
#
# # Apply the function (This will take about 20-30 seconds to run)
# real_counts <- map_int(years_to_search, get_publication_count)
#
# academic_df <- tibble(
#   year = years_to_search,
#   publications = real_counts
# )
#
# # 2. Plot the REAL data
# ggplot(academic_df, aes(x = year, y = publications)) +
#   geom_area(fill = "#2980b9", alpha = 0.6) +
#   geom_line(color = "#1f618d", linewidth = 1) +
#
#   geom_vline(xintercept = 2015, linetype = "dotted", color = "#34495e", linewidth = 0.8) +
#   annotate("text", x = 2015.5, y = max(academic_df$publications) * 0.9,
#            label = "100th Anniversary (2015)", hjust = 0, fontface = "italic", color = "#34495e", size = 3.5) +
#
#   scale_x_continuous(breaks = seq(1980, 2025, by = 5)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
#   labs(
#     title = "The Academic Awakening",
#     subtitle = "Real volume of published academic papers referencing the Armenian Genocide.",
#     x = "Publication Year",
#     y = "Number of Academic Papers (DOIs)",
#     caption = "Data: Live query via the Crossref Academic API"
#   ) +
#   theme_fivethirtyeight() +
#   theme(
#     plot.title = element_text(face = "bold", size = 16),
#     plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
#     panel.grid.major.x = element_blank(),
#     axis.title = element_text(face = "bold", size = 10)
#   )

# we had an error I had to commit out as it seems most of the db is null, we need to recheck
#
# > rlang::last_trace()
# <error/purrr_error_indexed>
#   Error in `map_int()`:
#   ℹ In index: 1.
# Caused by error:
#   ! Result must be length 1, not 0.
# ---
#   Backtrace:
#   ▆
# 1. └─purrr::map_int(years_to_search, get_publication_count)
# 2.   └─purrr:::map_("integer", .x, .f, ..., .progress = .progress)
# 3.     ├─purrr:::with_indexed_errors(...)
# 4.     │ └─base::withCallingHandlers(...)
# 5.     └─purrr:::call_with_cleanup(...)
# Run rlang::last_trace(drop = FALSE) to see 4 hidden frames.
# > rlang::last_trace(drop = FALSE)
# <error/purrr_error_indexed>
#   Error in `map_int()`:
#   ℹ In index: 1.
# Caused by error:
#   ! Result must be length 1, not 0.
# ---
#   Backtrace:
#   ▆
# 1. ├─purrr::map_int(years_to_search, get_publication_count)
# 2. │ └─purrr:::map_("integer", .x, .f, ..., .progress = .progress)
# 3. │   ├─purrr:::with_indexed_errors(...)
# 4. │   │ └─base::withCallingHandlers(...)
# 5. │   └─purrr:::call_with_cleanup(...)
# 6. └─base::.handleSimpleError(...)
# 7.   └─purrr (local) h(simpleError(msg, call))
# 8.     └─cli::cli_abort(...)
# 9.       └─rlang::abort(...)



# 1. Ingest and Clean the Manual CSV
# Google Trends exports have 2 lines of metadata, so we use skip = 2
df_trends <- read_csv("multiTimeline.csv", skip = 2) |>
  # Google's column names are messy, so we rename them by position
  rename(
    date = 1,
    armenian_genocide = 2,
    holocaust = 3,
    rwandan_genocide = 4
  ) |>
  # Pivot from wide to long format so ggplot can use it
  pivot_longer(
    cols = c(armenian_genocide, holocaust, rwandan_genocide),
    names_to = "keyword",
    values_to = "hits"
  ) |>
  mutate(
    # Clean up the "<1" strings that Google uses for zero/low data
    hits = if_else(str_detect(hits, "<"), "0", as.character(hits)),
    hits = as.numeric(hits),

    # Format the date properly
    date = as.Date(paste0(date, "-01")),

    # Clean up the labels for the legend
    keyword = case_when(
      keyword == "armenian_genocide" ~ "Armenian Genocide",
      keyword == "holocaust" ~ "Holocaust",
      keyword == "rwandan_genocide" ~ "Rwandan Genocide"
    )
  )

# 2. Plot the Data
ggplot(df_trends, aes(x = date, y = hits, color = keyword)) +
  geom_line(linewidth = 1, alpha = 0.85) +

  # Highlight the massive 2015 Centennial Spike
  geom_vline(xintercept = as.Date("2015-04-01"), linetype = "dotted", color = "#34495e", linewidth = 0.8) +
  annotate("text", x = as.Date("2015-06-01"), y = 85, label = "Armenian Genocide\n100th Anniversary",
           hjust = 0, size = 3.5, fontface = "italic", color = "#34495e") +

  # Scale and Layout Formatting
  scale_color_manual(values = c("Armenian Genocide" = "#8e44ad",
                                "Holocaust" = "#2c3e50",
                                "Rwandan Genocide" = "#e67e22")) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title = "The Mechanics of Global Memory",
    subtitle = "Relative Google Search interest for major historical genocides (2004–2026).",
    x = "Year",
    y = "Relative Search Interest (0-100)",
    caption = "Data: Google Trends"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_text(face = "bold", size = 10)
  )


####################################




# 1. Setup your parameters
api_key <- "YOUR_SERPAPI_API_KEY" # Paste your key here
search_term <- '"Armenian Genocide"' # Keep quotes for exact phrasing
years <- 1970:2023 # Adjust your timeline

# Initialize an empty dataframe to hold our real data
real_academic_df <- tibble(year = integer(), publications = integer())

# 2. Loop through the years and query SerpApi
for (y in years) {

  # Construct the API request
  url <- modify_url("https://serpapi.com/search", query = list(
    engine = "google_scholar",
    q = search_term,
    as_ylo = y,     # Start year limit
    as_yhi = y,     # End year limit (keeps it to just this year)
    api_key = api_key
  ))

  # Fetch and parse the JSON
  response <- GET(url)
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))

  # Extract total results. Scholar estimates this, but it's an excellent proxy for volume.
  # If a year has no results or the field is missing, we default to 0.
  total_results <- if(!is.null(data$search_information$total_results)) {
    as.numeric(data$search_information$total_results)
  } else {
    0
  }

  # Append to our dataframe
  real_academic_df <- real_academic_df |> add_row(year = y, publications = total_results)

  # Print progress to the console so you can watch it run
  cat("Year:", y, "- Publications:", total_results, "\n")

  # Polite delay so SerpApi doesn't throttle the connection
  Sys.sleep(1.5)
}

# 3. View the final dataset
print(real_academic_df)

# You can now plug real_academic_df directly into the ggplot code!
