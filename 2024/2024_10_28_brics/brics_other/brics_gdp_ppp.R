library(tidyverse)
library(WDI)
library(countrycode)
library(treemapify)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("~/R/newsletter/initial_setup.R")

gdp_ppp <- 
  WDI(indicator = "NY.GDP.MKTP.PP.CD") |>
  # WDI(indicator = "NY.GDP.MKTP.CD") |>  
  as_tibble() |> 
  rename(gdp_ppp = 5)

g7_countries <- c("CA", "FR", "DE", "IT", "JP", "GB", "US")
brics_countries <- c("BR", "RU", "IN", "CN", "ZA")
new_brics_countries <-  c("EG", "ET", "IR", "AE")
brics_potential <- c("AO", "BF", "CM", "CF", "CG", "CD", "GQ", "GH", "LY", "ML", "NG", "SN", "SS", "SD", "TN", "UG", "ZW", "BO", "CO", "CU", "SV", "NI", "PE", "VE", "AF", "AZ", "BH", "BD", "ID", "IQ", "KZ", "KW", "LA", "MY", "MM", "PK", "PS", "SA", "LK", "SY", "TH", "TR", "VN", "YE", "BY", "RS")

valid_countries <- 
  countrycode::codelist |> 
  filter(!is.na(iso2c)) |> 
  pull(iso2c)

gdp_ppp_plot_data <- 
  gdp_ppp |> 
  filter(iso2c %in% valid_countries) |> 
  mutate(
    block = case_when(
      iso2c %in% g7_countries ~ "G7 countries",
      iso2c %in% brics_countries ~ "BRICS",
      iso2c %in% new_brics_countries ~ "BRICS+",
      iso2c %in% brics_potential ~ "Potential BRICS+ members",
      TRUE ~ "Other Countries"
    )
  ) |> 
  group_by(year, block) |> 
  summarise(
    gdp_ppp = sum(gdp_ppp, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  filter(gdp_ppp != 0) |> 
  group_by(year) |> 
  mutate(
    pct = gdp_ppp / sum(gdp_ppp)
  ) |> 
  ungroup() |> 
  mutate(
    pct_text = ifelse(
      year %% 5 == 0 | year == max(year),
      percent(pct, accuracy = 0.1),
      NA
    ) 
  )


gdp_ppp_plot_data |> 
  ggplot(aes(year, gdp_ppp / 1e12, fill = block, label = pct_text)) +
  geom_area(alpha = 1) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  scale_fill_manual(values = new_palette_colors[c(2,4,5,6,8)]) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Share of GDP in some blocks",
    subtitle = "trillion USD"
  )

gdp_ppp_plot_data |> 
  ggplot(aes(year, pct, fill = block, label = pct_text)) +
  geom_area(alpha = 1) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = new_palette_colors[c(2,4,5,6,8)]) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Share of GDP in some blocks",
    subtitle = "trillion USD"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),  
  )



###################################################


iso_to_unicode_flag <- function(iso2c) {
  sapply(iso2c, function(code) {
    if (is.na(code)) return(NA)
    paste0(
      intToUtf8(127462L + which(LETTERS == substr(code, 1, 1)) - 1L),
      intToUtf8(127462L + which(LETTERS == substr(code, 2, 2)) - 1L)
    )
  })
}




gdp_ppp_flag_db <- 
  gdp_ppp |> 
  filter(iso2c %in% valid_countries) |> 
  mutate(
     block = case_when(
      iso2c %in% g7_countries ~ "G7 countries",
      iso2c %in% brics_countries ~ "BRICS",
      iso2c %in% new_brics_countries ~ "BRICS+",
      iso2c %in% brics_potential ~ "Potential BRICS+ members",
      TRUE ~ "Other Countries"
    ),
    estimate = ifelse(is.na(gdp_ppp), "estimated", ""),
    flag_unicode = iso_to_unicode_flag(iso2c),
    # country = countrycode(iso2c, origin = 'iso2c', destination = 'country.name'),
    country = countrycode(iso2c, origin = 'iso2c', destination = 'cldr.short.hy')
  ) |> 
  arrange(iso3c, year) |> 
  fill(gdp_ppp, .direction = "down") |> 
  group_by(year) |> 
  mutate(
    pct = gdp_ppp / sum(gdp_ppp, na.rm = TRUE),
    block = fct_reorder(block, gdp_ppp)
  ) |> 
  ungroup()



library(rvest)

members <- 
  read_html("https://en.wikipedia.org/wiki/Member_states_of_BRICS#Countries_that_have_applied_for_membership") |> 
  html_elements("table") |> 
  html_table()

brics_applicants <- 
  members[[2]] |> 
  mutate(iso2c = countrycode(Country, origin = 'country.name', destination = 'iso2c')) |> 
  pull(iso2c)


gdp_ppp_flag_db |> 
  filter(
    # iso2c %in% c(brics_countries, new_brics_countries, brics_potential),
    year == max(year)
  ) |> 
  mutate(
    block = case_when(
      iso2c %in% g7_countries ~ "G7",
      block %in% c("BRICS", "BRICS+") ~ "BRICS+",
      iso2c %in% brics_applicants ~ "Applied for BRICS membership",
      TRUE ~ "Other Countries"
    ),
    # block = fct_inorder()
  ) |> 
  group_by(block) |> 
  mutate(
    pct_group = sum(pct),
    block_label = paste(block, percent(pct_group, accuracy = 0.1)),
    block_label_2 = case_when(
      block %in% c("G7", "BRICS+") ~ block_label,
      iso2c %in% brics_applicants ~ "  ",
      TRUE ~ " "
      ),
    block_label = fct_reorder(block_label, gdp_ppp)
  ) |> 
  ggplot(aes(
    area = gdp_ppp,
    fill = block_label,
    label = paste0(
      flag_unicode, " ", country, "\n", 
      number(gdp_ppp/1e9, accuracy = 1), " | ",
      percent(pct, accuracy = 0.1)
    ),
    subgroup = block_label_2
  )) +
  geom_treemap(
    layout = "squarified",
    start = "topleft",
    alpha = 1,
    # layout = "fixed"
  ) +
  geom_treemap_text(
    place = "centre",
    start = "topleft",
    size = 12,
    color = "white",
    reflow = TRUE
  ) +
  geom_treemap_subgroup_border(
    colour = "white",  start = "topleft"
    ) +
  geom_treemap_subgroup_text(
    start = "topleft",
    place = "topleft",
    alpha = 0.5,
    padding.x = grid::unit(4, "mm"),
    padding.y = grid::unit(4, "mm"),
    color = "white"
  ) +
  scale_fill_manual(
    values = new_palette_colors[c(2,4,6,8)]
  ) +
  labs(
    fill = NULL,
    title = "BRICS+ պոտենցիալ անդամների ՀՆԱ-ն, 2023թ․",
    subtitle = "մլրդ ԱՄՆ դոլար, գնողունակության պարիտետով ճշգրտված,\n% համաշխարհայինից"
  )

############################################


plot_data_1 <- 
  gdp_ppp_flag_db |> 
  filter(
    block %in% c("BRICS"),
    year >= 1990
  ) |> 
  mutate(
    country = ifelse(country == "Հարավաֆրիկյան Հանրապետություն", "ՀԱՀ", country),
    country = fct_reorder(country, pct),
    pct_text = ifelse(year %% 5 == 0 & iso2c != "ZA", pct, NA),
    pct_text = percent(pct_text, accuracy = 0.1),
  )

plot_data_2 <-
  # gdp_ppp_flag_db |> 
  # filter(
  #   block %in% c("G7 countries"),
  #   year >= 1990
  # ) |> 
  # group_by(year) |> 
  # summarise(pct = sum(pct, na.rm = TRUE), .groups = "drop")
  gdp_ppp_plot_data |> 
  filter(block == "G7 countries",  year >= 1990) |> 
  mutate(
    pct_text = ifelse(year %% 5 == 0, pct, NA),
    pct_text = percent(pct_text, accuracy = 0.1),
  ) |> 
  select(year, pct, pct_text)


g7_pct <- plot_data_2 |> filter(year == max(year)) |> pull(pct) |> percent(accuracy = 0.1)

plot_data_3 <- 
  plot_data_1 |> 
  filter(year == max(year)) |> 
  arrange(desc(country)) |> 
  mutate(
    pct_text = percent(pct, accuracy = 0.1),
    pct2 = ifelse(pct == max(pct), pct / 2, (pct + lag(pct)) / 2),
    pct = cumsum(pct2),
    pct = case_when(
      iso2c %in% c("IN") ~ pct - 0.03,
      iso2c %in% c("RU") ~ pct - 0.037,
      iso2c %in% c("BR") ~ pct - 0.022,
      iso2c %in% c("ZA") ~ pct + 0.011,
      TRUE ~ pct
    )
  ) |> 
  bind_rows(
    tibble(
      country = "Մեծ յոթնյակ", flag_unicode = "G7", year = 2023, pct = 0.385, pct_text = g7_pct
    )
  ) |> 
  mutate(
    country = fct_inorder(country) |> fct_rev()
  )



max_year = plot_data_1$year |> max()
ymax = plot_data_1 |> 
  filter(year == max(year)) |> 
  summarise(pct = sum(pct)) |> 
  pull(pct)

ggplot() +
  geom_area(
    data = plot_data_1,
    mapping = aes(year, pct, fill = country),
    color = "white",
    alpha = 1
  ) +
  geom_line(
    data = plot_data_2,
    mapping = aes(year, pct), color = new_palette_colors[1], size = 1.5, alpha = 1
  ) +
  geom_text(
    data = plot_data_2,
    mapping = aes(year, pct, label = pct_text, vjust = ifelse(year == 2020, -1, 3)),
    color = new_palette_colors[1], hjust = 0,
  ) +
  geom_text(
    data = plot_data_1,
    mapping = aes(
      year, pct, fill = country, label = pct_text, 
      hjust = ifelse(year == min(year), -0.1, 0)
    ),
    position = position_stack(vjust = 0.5), color = "white"
  ) +
  geom_text(
    data = plot_data_3,
    aes(year + 0.4, pct, color = country, label = flag_unicode),
    hjust = 0, vjust = 0, fontface = "bold", size = 10
  ) +
  geom_text(
    data = plot_data_3,
    aes(year + 0.4, pct, color = country, label = country),
    hjust = 0, vjust = 1.6, size = 4.5
  ) +
  geom_text(
    data = plot_data_3,
    aes(year + 2.4, pct, color = country, label = pct_text),
    hjust = 0, vjust = 0, size = 10
  ) +
  geom_segment(
    data = tibble(x = max_year, xend = max_year, y = 0, yend = ymax),
    mapping = aes(x, y, xend = xend, yend = yend),
    linewidth = 1, color = "black"
  ) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  scale_y_continuous(n.breaks = 8, label = percent_format()) +
  scale_fill_manual(values = colfunc2(10)[3:7]) +
  scale_color_manual(values = c(new_palette_colors[1], colfunc2(10)[3:7])) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "BRICS versus the G7",
    subtitle = "Տոկոս համաշխարհային ՀՆԱ-ից (գնողունակության պարիտոտով ճշգրված)",
    # subtitleC = "ներկայացված եկամտային խմբերով",
    caption = caption_f("Համաշխարհային բանկ")
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "gray", linewidth = 0.1,
      linetype = 1
    ),
    legend.position = "none",
    plot.margin = margin(10, 75, 10, 10)
  )

ggsave("plots/brics_v_g7_gdp.png", ggplot2::last_plot(), width = 12, height = 8)

####################################################################

library(tidytext)

gdp_ppp_flag_db |> 
  filter(
    block %in% c("BRICS", "BRICS+", "G7 countries"),
    year == max(year)
  ) |> 
  mutate(
    block = ifelse(grepl("BRICS", block), "BRICS", "G7"),
    country = case_when(
      country == "Հարավաֆրիկյան Հանրապետություն" ~ "ՀԱՀ",
      country == "Արաբական Միացյալ Էմիրություններ" ~ "ԱՄԷ",
      TRUE ~ country
    ),
    # country = paste(country, "   "),
    country = reorder_within(country, pct, block),
    pct_text = percent(pct, accuracy = 0.1)
  ) |> 
  ggplot(aes(pct, country)) +
  geom_col(aes(fill = block), alpha = 1) +
  geom_text(aes(label = pct_text), hjust = -0.1) +
  geom_text(aes(x = 0, label = flag_unicode), hjust = 1, size = 11) +
  facet_wrap(~block, scales = "free_y") +
  scale_x_continuous(limits = c(0, 0.22)) +
  scale_y_reordered() +
  scale_fill_manual(values = new_palette_colors[c(6,2)]) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "BRICS versus the G7",
    subtitle = "Տոկոս համաշխարհային ՀՆԱ-ից (գնողունակության պարիտոտով ճշգրված)",
    # subtitleC = "ներկայացված եկամտային խմբերով",
    caption = caption_f("Համաշխարհային բանկ")
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(hjust = 0),
  )


# այս վերլուծությունը գրվել է կազանում կայացած բրիքս 2024 սամիթից հետո։ տնտեսությունները


library(ggtext)
library(patchwork)



# First create the circles plot
circles_plot <- 
  gdp_ppp_flag_db |> 
  filter(
    block %in% c("BRICS", "BRICS+", "G7 countries"),
    year == max(year)
  ) |> 
  mutate(block = ifelse(grepl("BRICS", block), "BRICS", "G7")) %>%
  group_by(block) |> 
  summarise(total_pct = sum(pct)) |> 
  ggplot(aes(x = c(0.85,2), y = 1)) +
  geom_point(aes(size = total_pct, color = block), shape = 16, alpha = 1) +
  geom_text(
    aes(label = percent(total_pct, accuracy = 0.01)),
    # position = position_nudge(y = 0.2),
    size = 4, color = "white"
  ) +
  geom_text(
    aes(label = block),
    position = position_nudge(y = 1.7),
    size = 4, fontface = "bold"
  ) +
  geom_text(
    data = tibble(x = 1.425, y = 1, label = "ՀՆԱ-Ի ԸՆԴՀԱՆՈՒՐ\nԾԱՎԱԼԸ"),
    aes(x, y, label = label),
    size = 4.5, fontface = "bold"
  ) +
  scale_x_continuous(limits = c(0.5, 2.7)) +
  scale_y_continuous(limits = c(-0.1, 3)) +
  scale_size_continuous(range = c(28.4, 34.95)) +
  scale_color_manual(values = new_palette_colors[c(5, 1)]) +
  labs(
    x = NULL, y = NULL,
    title = "BRICS versus the G7",
    subtitle = "Տոկոս համաշխարհային ՀՆԱ-ից (գնողունակության պարիտոտով ճշգրված)",
  )  +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
  )

main_plot <-
  gdp_ppp_flag_db |> 
  filter(
    block %in% c("BRICS", "BRICS+", "G7 countries"),
    year == max(year)
  ) |> 
  mutate(
    block = ifelse(grepl("BRICS", block), "BRICS", "G7"),
    country = case_when(
      country == "Հարավաֆրիկյան Հանրապետություն" ~ "ՀԱՀ",
      country == "Արաբական Միացյալ Էմիրություններ" ~ "ԱՄԷ",
      TRUE ~ country
    ),
    country = paste(flag_unicode, country),
    country = reorder_within(country, pct, block),
  ) |> 
  ggplot(aes(pct, country)) +
  geom_col(aes(fill = block), alpha = 1) +
  geom_text(aes(label = percent(pct, accuracy = 0.1)), hjust = -0.1) +
  facet_wrap(~block, scales = "free_y") +
  scale_x_continuous(limits = c(0, 0.22)) +
  scale_y_reordered() +
  scale_fill_manual(values = new_palette_colors[c(6,2)]) +
  labs(
    x = NULL,
    y = NULL,
    caption = caption_f("Համաշխարհային բանկ")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_markdown(hjust = 0),
    strip.text = element_blank(),
  )

# Combine the plots
final_plot <- circles_plot / main_plot +
  plot_layout(heights = c(1.3, 4))

final_plot

ggsave(filename = "~/Brics_3.png", ggplot2::last_plot(), width = 12, height = 8)

##############################################################

brics_16_tables <- 
  read_html("https://en.wikipedia.org/wiki/16th_BRICS_summit") |> 
  html_elements("table") |> 
  html_table()

brics_16_present <- 
  brics_16_tables[[3]] |> 
  rename(country = 1) |> 
  mutate(
    iso2c = countrycode(country, origin = 'country.name', destination = 'iso2c'),
  ) |> 
  pull(iso2c)

brics_16_present <- c(brics_countries, new_brics_countries, brics_16_present) |> 
  unique()

population <- WDI(indicator = "SP.POP.TOTL", start = 1990) |> 
  as_tibble() |> 
  rename(population = 5)


population |> 
  filter(iso2c %in% valid_countries) |> 
  mutate(
    block = case_when(
      iso2c %in% brics_16_present ~ "brics_16_present",
      TRUE ~ "Other Countries"
    )
  ) |> 
  filter(year == max(year)) |> 
  group_by(block) |> 
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop") |> 
  mutate(
    pct = population / sum(population)
  )


gdp_ppp_flag_db |> 
  filter(iso2c %in% valid_countries) |> 
  mutate(
    block = case_when(
      iso2c %in% brics_16_present ~ "brics_16_present",
      TRUE ~ "Other Countries"
    )
  ) |> 
  filter(year == max(year)) |> 
  group_by(block) |> 
  summarise(gdp_ppp = sum(gdp_ppp, na.rm = TRUE), .groups = "drop") |> 
  mutate(
    pct = gdp_ppp / sum(gdp_ppp)
  )

gdp_ppp_flag_db |> 
  filter(iso2c == "TR", year == max(year)) |> 
  view()
