library(tidyverse)
library(gt)

source("~/R/newsletter/initial_setup.R")


# Create the data frame with Armenian translations
metro_fares_ <- tribble(
  ~City, ~Country, ~Fare_Type, ~Local_Cost, ~Local_Currency, ~Cost_AMD, ~average_net_salary_amd,
  "Երևան", "Հայաստան", "Ստանդարտ ուղևորություն", 300, "AMD", 300, 215988,
  "Թբիլիսի", "Վրաստան", "Ստանդարտ ուղևորություն", 1, "GEL", 130, 222744,
  "Բաքու", "Ադրբեջան", "Մետրոյի մեկ ուղևորություն", 0.50, "AZN", 113, 194244,
  "Թեհրան", "Իրան", "Մետրոյի մեկ ուղևորություն", 10000, "IRR", 94, 105224,
  "Ստամբուլ", "Թուրքիա", "Մետրոյի մեկ ուղևորություն", 27, "TRY", 450, 362756,
  "Մոսկվա", "Ռուսաստան", "Մետրոյի մեկ ուղևորություն", 57, "RUB", 223, 525620,
  "Կիև", "Ուկրաինա", "Ստանդարտ ուղևորություն", 8, "UAH", 100, NA,
  "Մինսկ", "Բելառուս", "Մետրոյի մեկ ժետոն", 0.90, "BYN", 94, 275240,
  "Տաշքենդ", "Ուզբեկստան", "Կանխիկ/բանկային քարտ", 2000, "UZS", 70, 201556,
  "Ալմաթի", "Ղազախստան", "Մեկ ուղևորություն", 90, "KZT", 70, 253136,
  "Վարշավա", "Լեհաստան", "Մեկ տոմս", 3.40, "PLN", 319, 715552,
  "Պրահա", "Չեխիա", "30 րոպեանոց տոմս", 30, "CZK", 450, 721276,
  "Բուդապեշտ", "Հունգարիա", "Մեկ տոմս", 600, "HUF", 600, 463504,
  "Բուխարեստ", "Ռումինիա", "Մեկ ուղևորություն", 3, "RON", 243, 481084,
  "Սոֆիա", "Բուլղարիա", "Մեկ ուղևորություն", 1.60, "BGN", 290, 551628
)

exchanges <- read_csv("~/R/Gcapatker/2024_03_24_CBA_FX/CBA_FX_data.csv") |> 
  filter(year >= 2025) |>
  group_by(FX_ISO) |> 
  filter(date == max(date)) |> 
  ungroup() |> 
  extract(
    FX_ISO, into = c("Local_Currency", "factor"),
    regex = "([A-Z]+)(\\d+)", convert = TRUE
  ) |> 
  mutate(AMD = AMD/factor) |> 
  select(-factor, -date) |> 
  filter(!Local_Currency %in% c("TRY", "HUF")) |> 
  bind_rows(
    tribble(
      ~Local_Currency, ~AMD, ~year,
      "AMD", 1, 2025,
      "AZN", 235.75, 2025,
      "TRY", 11.25, 2025,
      "HUF", 1.002, 2025,
      "RON", 83.09, 2025,
      "BGN", 211.42, 2025
    )
  )


metro_fares <- 
  metro_fares_ |> 
  left_join(exchanges, by = "Local_Currency") |> 
  mutate(Cost_AMD_adjasted = Local_Cost * AMD)


# Create the formatted table
# Create the formatted table with hyperlinks
# Create the formatted table with hyperlinks
metro_fares %>%
  select(
    City, 
    Country, 
    Fare_Type, #= case_when(
    #   City == "Երևան" ~ glue::glue("<a href='https://www.301.am/yerevans-public-transport-fare-hike-to-300-amd-places-city-among-most-expensive-in-cis-capitals/'>Ստանդարտ ուղևորություն</a>"),
    #   City == "Թբիլիսի" ~ glue::glue("<a href='https://www.worldofmetro.com/tbilisi-metro/'>Մետրոյի մեկ ուղևորություն</a>"),
    #   City == "Բաքու" ~ glue::glue("<a href='https://turan.az/en/social/subway-and-bus-fares-have-increased-in-azerbaijan-782016'>Մետրոյի մեկ ուղևորություն</a>"),
    #   City == "Թեհրան" ~ glue::glue("<a href='https://www.iraniantours.com/price/'>Մետրոյի մեկ ուղևորություն</a>"),
    #   City == "Ստամբուլ" ~ glue::glue("<a href='https://www.duvarenglish.com/public-transportation-fares-in-istanbul-increased-by-35-pct-news-65538'>Մետրոյի մեկ ուղևորություն</a>"),
    #   City == "Մոսկվա" ~ glue::glue("<a href='https://weheart.moscow/publictransport/'>Մետրոյի մեկ ուղևորություն</a>"),
    #   City == "Կիև" ~ glue::glue("<a href='https://en.tripmydream.com/ukraine/kiev/local-transport'>Ստանդարտ ուղևորություն</a>"),
    #   City == "Մինսկ" ~ glue::glue("<a href='https://www.belarus.by/en/travel/transport-in-belarus/minsk-metro'>Մետրոյի մեկ ժետոն</a>"),
    #   City == "Տաշքենդ" ~ glue::glue("<a href='https://www.uzdaily.uz/en/public-transport-fares-in-tashkent-to-increase-from-1-november/'>Կանխիկ/բանկային քարտ</a>"),
    #   City == "Ալմաթի" ~ glue::glue("<a href='https://www.lonelyplanet.com/articles/getting-around-kazakhstan'>Մեկ ուղևորություն</a>"),
    #   City == "Վարշավա" ~ glue::glue("<a href='https://community.ricksteves.com/travel-forum/poland/senior-transit-in-warsaw'>20 րոպեանոց տոմս</a>"),
    #   City == "Պրահա" ~ glue::glue("<a href='https://pragueclassicalconcerts.com/en/public-transport-tickets'>30 րոպեանոց տոմս</a>"),
    #   City == "Բուդապեշտ" ~ glue::glue("<a href='https://housinganywhere.com/Budapest--Hungary/budapest-public-transport'>Մեկ տոմս</a>"),
    #   City == "Բուխարեստ" ~ glue::glue("<a href='https://www.inyourpocket.com/bucharest/Bucharest-Public-Transport'>Մեկ ուղևորություն</a>"),
    #   City == "Սոֆիա" ~ glue::glue("<a href='https://openbulgaria.org/post/public-transport-sofia/'>Մեկ ուղևորություն</a>")
    # ),
    Local_Cost, 
    Local_Currency, 
    Cost_AMD = Cost_AMD_adjasted
  ) |> 
  arrange(desc(Cost_AMD)) |> 
  gt() %>%
  tab_header(
    title = md("**Հասարակական տրանսպորտի ուղեվարձերի համեմատություն<br>տարբեր քաղաքներում**"),
    subtitle = "Գները ցուցադրված են տեղական արժույթով և հայկական դրամով (AMD)"
  ) %>%
  fmt_number(
    columns = c(Local_Cost),
    decimals = 2,
    dec_mark = ",",
    sep_mark = "."
  ) %>%
  fmt_number(
    columns = Cost_AMD,
    decimals = 0,
    dec_mark = ",",
    sep_mark = "."
  ) %>%
  cols_merge(
    columns = c(Local_Cost, Local_Currency),
    pattern = "{1} {2}"
  ) %>%
  cols_label(
    City = "Քաղաք",
    Country = "Երկիր",
    Fare_Type = "Տոմսի տեսակ",
    Local_Cost = "Տեղական գին",
    Cost_AMD = "Գինը դրամով"
  ) %>%
  tab_source_note(
    source_note = caption_f(suffix_text = "Գները առ 2025թ․ հունվար               |      ")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f6f6f6")
    ),
    locations = cells_body(
      rows = seq(1, nrow(metro_fares), 2)
    )
  ) %>%
  text_transform(
    locations = cells_body(columns = Fare_Type),
    fn = function(x) map_chr(x, ~ as.character(html(.x)))
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    heading.border.bottom.style = "solid",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2)
  )

# 
# Հղումներ․
# 
# 1. Yerevan - 220 AMD*
# Source: https://www.301.am/yerevans-public-transport-fare-hike-to-300-amd-places-city-among-most-expensive-in-cis-capitals/
# Երևանում ամենամատչելի երդևեկության տոմսի արժեքը 300 դրամ է, որը ներառում է 3 ուղևորություն 90 րոպեում։ Միջին արժեքը հաշվարկվել է 10 ուղևորության անսահմանափակ փաթեթի վրա, որը գինը մեկ ուղևորության համար 220 դրամ է։
# 2.  Tbilisi - 1 GEL
# Source: https://www.worldofmetro.com/tbilisi-metro/
# 3.  Baku - 0.50 AZN
# Sources:
# https://turan.az/en/social/subway-and-bus-fares-have-increased-in-azerbaijan-782016
# https://azerbaijan.travel/practical-information
# 4. Tehran - 10,000 IRR
# Sources:
# https://www.saadatrent.com/english/article/cost-of-travel-to-iran
# https://www.iraniantours.com/price/
# 5.  Istanbul - 27 TRY
# Source: https://www.duvarenglish.com/public-transportation-fares-in-istanbul-increased-by-35-pct-news-65538
# 5.Moscow - 57 RUB
# Source: https://weheart.moscow/publictransport/
# 7.  Kyiv - 8 UAH
# Source: https://en.tripmydream.com/ukraine/kiev/local-transport
# 8. Minsk - 0.90 BYN
# Source: https://www.belarus.by/en/travel/transport-in-belarus/minsk-metro
# 9.Tashkent - 2,000 UZS
# Source: https://www.uzdaily.uz/en/public-transport-fares-in-tashkent-to-increase-from-1-november/
# 10.  Almaty - 90 KZT
# Source: https://www.lonelyplanet.com/articles/getting-around-kazakhstan
# 11. Warsaw - 3.40 PLN
# Source: https://community.ricksteves.com/travel-forum/poland/senior-transit-in-warsaw
# 12. Prague - 30 CZK
# Source: https://pragueclassicalconcerts.com/en/public-transport-tickets
# 13. Budapest - 600 HUF
# Source: https://housinganywhere.com/Budapest--Hungary/budapest-public-transport
# 14. Bucharest - 3 RON
# Source: https://www.inyourpocket.com/bucharest/Bucharest-Public-Transport
# 15. Sofia - 1.60 BGN
# Sources:
# https://openbulgaria.org/post/public-transport-sofia/
# https://erasmus.uni-sofia.bg/site/income/practical-information-2/local-transportation/


metro_fares |> 
  bind_rows(
    tribble(
      ~City, ~Country, ~Fare_Type, ~Local_Cost, ~Local_Currency, ~Cost_AMD, ~average_net_salary_amd,
      "Երևան հին", "Հայաստան", "մինչև փոփոխություն", 100, "AMD", 100, 215988
    )
  ) |> 
  mutate(
    tickers_per_salary = average_net_salary_amd / Cost_AMD
  ) |> 
  filter(
    !is.na(tickers_per_salary)
  ) |> 
  mutate(
    City = fct_reorder(City, tickers_per_salary)
  ) |> 
  ggplot(aes(tickers_per_salary, City)) +
  geom_col()


#################################################3


numbeo_db_cities_one_way <- 
  numbeo_db_cities_transport |> 
  filter(
    grepl("One-way", indicator),
    !is.na(info_entries),
    cities %in% major_cities_dict$cities
  ) |> 
  select(country, cities, mean_price) |> 
  mutate(mean_price_amd = mean_price * 400)

metro_fares |> 
  transmute(cities_am = City, Local_Cost, Local_Currency, Cost_AMD) |> 
  janitor::clean_names() |> 
  left_join(major_cities_dict, by = "cities_am") |> 
  left_join(numbeo_db_cities_one_way, by = "cities") |> 
  relocate(mean_price, mean_price_amd, cost_amd, .after = "country") |> 
  mutate(discrepancy = abs((mean_price_amd - cost_amd)/cost_amd)) |> 
  filter(discrepancy <= 0.15)

#########################

library(tidyverse)

# Create analysis dataframe
discrepancy_analysis <- metro_fares %>%
  transmute(cities_am = City, Local_Cost, Local_Currency, Cost_AMD) %>%
  janitor::clean_names() %>%
  left_join(major_cities_dict, by = "cities_am") %>%
  left_join(numbeo_db_cities_one_way, by = "cities") %>%
  relocate(mean_price, mean_price_amd, cost_amd, .after = "country") %>%
  mutate(
    discrepancy = mean_price_amd/cost_amd,
    abs_diff_pct = abs(1 - discrepancy) * 100,
    direction = if_else(discrepancy > 1, "Numbeo Higher", "Manual Higher")
  ) %>%
  filter(!is.na(discrepancy)) %>%
  arrange(desc(abs_diff_pct))

# Create visualization
ggplot(discrepancy_analysis, 
       aes(x = reorder(cities, abs_diff_pct), 
           y = abs_diff_pct,
           fill = direction)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Price Discrepancies Between Numbeo and Manual Collection",
    x = "City",
    y = "Absolute Percentage Difference",
    fill = "Price Difference Direction"
  ) +
  scale_fill_manual(values = c("Numbeo Higher" = "#69b3a2", "Manual Higher" = "#404080"))

# Print top discrepancies
print("Top discrepancies between Numbeo and manually collected data:")
discrepancy_analysis %>%
  select(cities, discrepancy, abs_diff_pct, direction) %>%
  arrange(desc(abs_diff_pct)) %>%
  mutate(
    abs_diff_pct = round(abs_diff_pct, 1),
    discrepancy = round(discrepancy, 2)
  ) %>%
  print(n = Inf)
