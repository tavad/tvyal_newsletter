library(tidyverse)
library(scales)
library(countrycode)


iso_to_unicode_flag <- function(iso2c) {
  sapply(iso2c, function(code) {
    if (is.na(code)) return(NA)
    paste0(
      intToUtf8(127462L + which(LETTERS == substr(code, 1, 1)) - 1L),
      intToUtf8(127462L + which(LETTERS == substr(code, 2, 2)) - 1L)
    )
  })
}


indicator_gr <- 
  numbeo_db_countries_clean |> 
  mutate(indicator_group = fct_inorder(indicator_group)) |> 
  count(indicator_group) |> 
  pull(indicator_group)

numbeo_db_cities_transport <- 
  numbeo_db_cities_clean |> 
  filter(indicator_group %in% indicator_gr[c(3,10)]) |> 
  select(-url, -city_urls)

major_cities_dict <-
  tibble(
    cities = c(
      "Moscow", "Saint Petersburg", "Novosibirsk", "Istanbul", "Warsaw", 
      "Prague", "Budapest", "Bucharest", "Sofia", "Ankara", "Tbilisi",
      "Baku", "Yerevan", "Minsk", "Tashkent", "Almaty", "Tehran"
    ),
    cities_am = c(
      "Մոսկվա", "Սանկտ Պետերբուրգ", "Նովոսիբիրսկ", "Ստամբուլ", "Վարշավա",
      "Պրահա", "Բուդապեշտ", "Բուխարեստ", "Սոֆիա", "Անկարա", "Թբիլիսի", 
      "Բաքու", "Երևան", "Մինսկ", "Տաշքենդ", "Ալմաթի", "Թեհրան"
    )
  )
  

numbeo_db_cities_transport |> 
  filter(
    grepl("One-way", indicator),
    !is.na(info_entries),
    cities %in% major_cities_dict$cities
  ) |> 
  select(country, cities, mean_price) |> 
  left_join(major_cities_dict, by = "cities") |> 
  filter(cities != "Yerevan") |> 
  bind_rows(
    tribble(
      ~country, ~cities, ~cities_am, ~mean_price,
      "Armenia", "Yerevan (old price)", "Երևան (հին գին)", 0.25,
      "Armenia", "Yerevan (new price)", "Երևան (նոր գին)", 0.55,
    )
  ) |> 
  mutate(
    iso2c = countrycode(country, origin  = "country.name", destination = "iso2c"),
    country_arm = countrycode(iso2c, origin = "iso2c", destination = "cldr.short.hy"),
    mean_price_amd = mean_price * 400,
    country_flag = iso_to_unicode_flag(iso2c),
    # cities = paste0(country_flag, "   ", cities),
    cities = fct_reorder(cities, mean_price_amd),
    cities_am = fct_reorder(cities_am, mean_price_amd),
  ) |> 
  ggplot(aes(mean_price_amd, cities_am, fill = ifelse(grepl("Yerevan", cities), "Yerevan", "Other"))) +
  geom_col(alpha = 1) +
  geom_text(
    aes(x = 0, label = number(mean_price_amd)),
    hjust = -0.3, size = 4, color = "white"
  ) +
  geom_text(
    aes(x = 0, label = country_flag),
    hjust = 1.3, size = 6
  ) +
  scale_fill_manual(values = new_palette_colors[c(2,6)]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Եվրոպական տրանսպորտ՝ եվրոպական գներ",
    subtitle = "Հասարակական տրանսպորտի ուղեվարձերի համեմատություն տարբեր քաղաքներում\nմեկ ուղևորություն դրամ",
    caption = caption_f("Numbeo")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_markdown(hjust = 0, size = 12),
    strip.text = element_blank(),
  )



numbeo_db_cities_transport |> 
  filter(
    grepl("Monthly Pass", indicator),
    !is.na(info_entries),
    cities %in% major_cities_dict$cities
  ) |> 
  select(country, cities, mean_price) |> 
  left_join(major_cities_dict, by = "cities") |> 
  filter(cities != "Yerevan") |>
  bind_rows(
    tribble(
      ~country, ~cities, ~cities_am, ~mean_price,
      "Armenia", "Yerevan (old price)", "Երևան (հին գին)", 16.2,
      "Armenia", "Yerevan (new price)", "Երևան (նոր գին)", 22.5,
    )
  ) |>
  mutate(
    iso2c = countrycode(country, origin  = "country.name", destination = "iso2c"),
    country_arm = countrycode(iso2c, origin = "iso2c", destination = "cldr.short.hy"),
    mean_price_amd = mean_price * 400,
    country_flag = iso_to_unicode_flag(iso2c),
    # cities = paste0(country_flag, "   ", cities),
    cities = fct_reorder(cities, mean_price_amd),
    cities_am = fct_reorder(cities_am, mean_price_amd),
  ) |> 
  ggplot(aes(mean_price_amd, cities_am, fill = ifelse(grepl("Yerevan", cities), "Yerevan", "Other"))) +
  geom_col(alpha = 1) +
  geom_text(
    aes(x = 0, label = number(mean_price_amd)),
    hjust = -0.3, size = 4, color = "white"
  ) +
  geom_text(
    aes(x = 0, label = country_flag),
    hjust = 1.3, size = 6
  ) +
  scale_fill_manual(values = new_palette_colors[c(2,6)]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Եվրոպական տրանսպորտ՝ եվրոպական գներ",
    subtitle = "Հասարակական տրանսպորտի ուղեվարձերի համեմատություն տարբեր քաղաքներում\nամսավարձ, դրամ",
    caption = caption_f("Numbeo")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_markdown(hjust = 0, size = 12),
    strip.text = element_blank(),
  )



numbeo_db_cities_transport |> 
  filter(
    grepl("Gasoline", indicator),
    !is.na(info_entries),
    cities %in% major_cities_dict$cities
  ) |> 
  select(country, cities, mean_price) |> 
  left_join(major_cities_dict, by = "cities") |> 
  mutate(
    iso2c = countrycode(country, origin  = "country.name", destination = "iso2c"),
    country_arm = countrycode(iso2c, origin = "iso2c", destination = "cldr.short.hy"),
    mean_price_amd = mean_price * 400,
    country_flag = iso_to_unicode_flag(iso2c),
    # cities = paste0(country_flag, "   ", cities),
    cities = fct_reorder(cities, mean_price_amd),
    cities_am = fct_reorder(cities_am, mean_price_amd),
  ) |> 
  ggplot(aes(mean_price_amd, cities_am, fill = ifelse(grepl("Yerevan", cities), "Yerevan", "Other"))) +
  geom_col(alpha = 1) +
  geom_text(
    aes(x = 0, label = number(mean_price_amd)),
    hjust = -0.3, size = 4, color = "white"
  ) +
  geom_text(
    aes(x = 0, label = country_flag),
    hjust = 1.3, size = 6
  ) +
  scale_fill_manual(values = new_palette_colors[c(2,6)]) +
  labs(
    x = NULL,
    y = NULL,
    title = "1 լիտր բենզինի գին, դրամ",
    subtitle = "Հասարակական տրանսպորտի ուղեվարձերի համեմատություն տարբեր քաղաքներում",
    caption = caption_f("Numbeo")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_markdown(hjust = 0, size = 12),
    strip.text = element_blank(),
  )

numbeo_db_cities_transport |> 
  filter(
    grepl("Taxi", indicator),
    !is.na(info_entries),
    cities %in% major_cities_dict$cities
  ) |> 
  select(country, cities, mean_price, indicator) |> 
  left_join(major_cities_dict, by = "cities") |> 
  mutate(
    iso2c = countrycode(country, origin  = "country.name", destination = "iso2c"),
    country_arm = countrycode(iso2c, origin = "iso2c", destination = "cldr.short.hy"),
    mean_price_amd = mean_price * 400,
    country_flag = iso_to_unicode_flag(iso2c),
    cities_am = paste0(cities_am, "   ", country_flag),
    cities_am = fct_reorder(cities_am, mean_price_amd, .fun = sum),
    cities = fct_reorder(cities, mean_price_amd),
  ) |> 
  ggplot(aes(mean_price_amd, cities_am, fill = ifelse(grepl("Yerevan", cities), "Yerevan", "Other"))) +
  facet_wrap(~indicator, scales = "free_x") +
  geom_col(alpha = 1) +
  geom_text(
    aes(x = 0, label = number(mean_price_amd)),
    hjust = -0.3, size = 4, color = "white"
  ) +
  scale_fill_manual(values = new_palette_colors[c(2,6)]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Եվրոպական տրանսպորտ՝ եվրոպական գներ",
    subtitle = "Տաքսու գները, դրամ",
    caption = caption_f("Numbeo")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_markdown(hjust = 0, size = 12),
  )

# այսպիսի պայմաններում ավելի ձեռք է տալիս տաքսուվ ուղևորվել քան վերցնել հասարակական տրանսպորտ, որը 
# տաքսու հետ համեմատաբար ավելի թանկ է։ Այս պարագայում կարող է տաքսիների քանակը երևանում շատանալ։
# Փաստացի տրանսպորտի թանկացումը կարող է հակառակ էֆֆեկտը բերել։ Հասրարական տրանսպորտի հիմնկանա նպատակներից է
# փողոցների թեթևացումը մեքենաներից և խցանումներից։ Այսպես Քաղապետարանը ամեն գլխավոր նպատակներից պետք է լինի
# քաղաքի խցանումնրի թեթևացում և այնպես անլ որ մարդիկ նախընտրեն տրանսպորտը։ Այս պարագայում հակառակ էֆեկտը կարող է բերել։


numbeo_db_cities_transport |> 
  filter(
    indicator %in% c("Taxi Start (Normal Tariff)", "One-way Ticket (Local Transport)"),
    !is.na(info_entries),
    cities %in% major_cities_dict$cities
  ) |> 
  select(country, cities, mean_price, indicator) |> 
  mutate(
    indicator = case_match(
      indicator,
      "Taxi Start (Normal Tariff)" ~ "taxi_start", 
      "One-way Ticket (Local Transport)" ~ "one_way"
    )
  ) |> 
  left_join(major_cities_dict, by = "cities") |> 
  pivot_wider(names_from = indicator, values_from = mean_price) |> 
  filter(cities != "Yerevan") |>
  bind_rows(
    tribble(
      ~country, ~cities, ~cities_am, ~ one_way, ~taxi_start,
      "Armenia", "Yerevan (old price)", "Երևան (հին գին)", 0.25, 1.5,
      "Armenia", "Yerevan (new price)", "Երևան (նոր գին)", 0.55, 1.5
    )
  ) |>
  mutate(
    taxi_to_one_way = taxi_start / one_way,
    iso2c = countrycode(country, origin  = "country.name", destination = "iso2c"),
    country_arm = countrycode(iso2c, origin = "iso2c", destination = "cldr.short.hy"),
    country_flag = iso_to_unicode_flag(iso2c),
    # cities = paste0(country_flag, "   ", cities),
    cities = fct_reorder(cities, taxi_to_one_way, .desc = TRUE),
    cities_am = fct_reorder(cities_am, taxi_to_one_way, .desc = TRUE),
  ) |> 
  ggplot(aes(taxi_to_one_way, cities_am, fill = ifelse(grepl("Yerevan", cities), "Yerevan", "Other"))) +
  geom_col(alpha = 1) +
  geom_text(
    aes(x = 0, label = number(taxi_to_one_way, accuracy = 0.1)),
    hjust = -0.3, size = 4, color = "white"
  ) +
  geom_text(
    aes(x = 0, label = country_flag),
    hjust = 1.3, size = 6
  ) +
  scale_fill_manual(values = new_palette_colors[c(2,6)]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Քանի անգամ կարելի է հասարակակն տրանսպորտից օգտվել\nհամեմատած տաքսու մինիմալ գնին",
    # subtitle = "Հասարակական տրանսպորտի ուղեվարձերի համեմատություն տարբեր քաղաքներում\n1 լիտր բենզինի գին, դրամ",
    caption = caption_f("Numbeo")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_markdown(hjust = 0, size = 12),
    strip.text = element_blank(),
  )



numbeo_db_cities_transport |> 
  filter(
    indicator %in% c("One-way Ticket (Local Transport)", "Average Monthly Net Salary (After Tax)"),
    !is.na(info_entries),
    cities %in% major_cities_dict$cities
  ) |> 
  select(country, cities, mean_price, indicator) |> 
  mutate(
    indicator = case_match(
      indicator,
      "Average Monthly Net Salary (After Tax)" ~ "mean_net_salary", 
      "One-way Ticket (Local Transport)" ~ "one_way"
    )
  ) |> 
  left_join(major_cities_dict, by = "cities") |> 
  pivot_wider(names_from = indicator, values_from = mean_price) |> 
  filter(cities != "Yerevan") |>
  bind_rows(
    tribble(
      ~country, ~cities, ~cities_am, ~ one_way, ~mean_net_salary,
      "Armenia", "Yerevan (old price)", "Երևան (հին գին)", 0.25, 539.97,
      "Armenia", "Yerevan (new price)", "Երևան (նոր գին)", 0.55, 539.97
    )
  ) |> 
  mutate(
    mean_salary_to_one_way = mean_net_salary / one_way,
    iso2c = countrycode(country, origin  = "country.name", destination = "iso2c"),
    country_arm = countrycode(iso2c, origin = "iso2c", destination = "cldr.short.hy"),
    country_flag = iso_to_unicode_flag(iso2c),
    # cities = paste0(country_flag, "   ", cities),
    cities = fct_reorder(cities, mean_salary_to_one_way),
    cities_am = fct_reorder(cities_am, mean_salary_to_one_way),
  ) |> 
  ggplot(aes(mean_salary_to_one_way, cities_am, fill = ifelse(grepl("Yerevan", cities), "Yerevan", "Other"))) +
  geom_col(alpha = 1) +
  geom_text(
    aes(x = 0, label = number(mean_salary_to_one_way, accuracy = 1)),
    hjust = -0.3, size = 4, color = "white"
  ) +
  geom_text(
    aes(x = 0, label = country_flag),
    hjust = 1.3, size = 6
  ) +
  scale_fill_manual(values = new_palette_colors[c(2,6)]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Քանի ուղետոմս ամիսը կարելի է ձեռք բերել միջին աշխատավարձով",
    subtitle = "Միջին աշխատավարձև և մեկ ուղետոմսի հարաբորությունը քաղաքում\nհատ",
    caption = caption_f("Numbeo")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_markdown(hjust = 0, size = 12),
    strip.text = element_blank(),
  )

