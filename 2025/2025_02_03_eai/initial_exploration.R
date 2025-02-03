library(readxl)
library(scales)
library(tidyverse)
library(RcppRoll)

source("../../initial_setup.R")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

GDP_factors <- read_csv("eai_components_db.csv")

new_data <- 
  tribble(
    ~year, ~month, ~name, ~industry, ~agriculture, ~construction, ~trade, ~services,
    # January-December 2024 monthly data
    2024, 1, "mln dram", 205500.3, NA, 17581.3, 340782.5, 243189.5,
    2024, 1, "yoy_growth", 119.3, NA, 109.5, 119.3, 107.3,
    2024, 1, "month_growth", 55.6, NA, 13.9, 48.3, 85.0,
    2024, 2, "mln dram", 255700.9, NA, 23975.9, 408294.1, 241349.7,
    2024, 2, "yoy_growth", 137.3, NA, 111.7, 128.3, 108.5,
    2024, 2, "month_growth", 119.2, NA, 135.5, 121.0, 99.3,
    2024, 3, "mln dram", 262483.8, NA, 30638.2, 489092.6, 274742.5,
    2024, 3, "yoy_growth", 130.9, NA, 113.1, 130.4, 105.8,
    2024, 3, "month_growth", 101.4, NA, 129.4, 120.1, 113.6,
    2024, 4, "mln dram", 229590.7, NA, 35574.7, 499475.8, 286429.8,
    2024, 4, "yoy_growth", 116.6, NA, 117.8, 127.2, 108.6,
    2024, 4, "month_growth", 86.9, NA, 115.8, 101.4, 103.7,
    2024, 5, "mln dram", 229707.0, NA, 42905.7, 501540.5, 278057.8,
    2024, 5, "yoy_growth", 106.9, NA, 118.1, 117.6, 107.1,
    2024, 5, "month_growth", 99.8, NA, 120.1, 101.4, 97.0,
    2024, 6, "mln dram", 224885.5, NA, 51339.7, 515033.8, 289003.5,
    2024, 6, "yoy_growth", 101.3, NA, 118.0, 116.2, 110.2,
    2024, 6, "month_growth", 97.5, NA, 119.9, 104.3, 103.7,
    2024, 7, "mln dram", 228288.1, NA, 57358.0, 547931.7, 288276.3,
    2024, 7, "yoy_growth", 102.2, NA, 117.7, 114.0, 108.5,
    2024, 7, "month_growth", 102.0, NA, 111.6, 107.3, 99.5,
    2024, 8, "mln dram", 241763.7, NA, 71361.9, 560144.7, 315093.4,
    2024, 8, "yoy_growth", 102.1, NA, 116.5, 114.9, 113.4,
    2024, 8, "month_growth", 107.5, NA, 124.8, 102.5, 108.6,
    2024, 9, "mln dram", 245600.8, NA, 75636.3, 556166.1, 307197.3,
    2024, 9, "yoy_growth", 104.6, NA, 114.7, 113.4, 115.0,
    2024, 9, "month_growth", 101.6, NA, 105.7, 99.5, 97.1,
    2024, 10, "mln dram", 275461.9, NA, 78174.2, 571561.5, 299947.0,
    2024, 10, "yoy_growth", 105.3, NA, 113.5, 113.7, 108.7,
    2024, 10, "month_growth", 110.9, NA, 103.3, 102.1, 97.8,
    2024, 11, "mln dram", 274166.6, NA, 82476.6, 613899.0, 296805.2,
    2024, 11, "yoy_growth", 81.0, NA, 111.6, 111.5, 112.3,
    2024, 11, "month_growth", 99.5, NA, 106.4, 105.4, 98.9,
    2024, 12, "mln dram", 325220.9, NA, 142442.8, 774343.0, 347424.3,
    2024, 12, "yoy_growth", 85.2, NA, 112.9, 110.3, 119.4,
    2024, 12, "month_growth", 117.7, NA, 172.2, 124.3, 117.1,
    # Annual data (month=13)
    2024, 13, "mln dram", 2998370.2, 958804.3, 709465.3, 6378265.3, 3467516.3,
    2024, 13, "yoy_growth", 104.7, 101.6, 114.5, 117.0, 110.6
  )

GDP_factors <- 
  GDP_factors |> 
  filter(year != 2024) |> 
  bind_rows(new_data) |> 
  filter(!is.na(year)) |>
  mutate(date = ym(paste(year, month))) |>
  relocate(date) |> 
  arrange(desc(year), month)

newsletter_dir <- "/home/tavad/R/newsletter/2025/2025_02_03_eai"

# GDP_factors |> write_excel_csv(file.path(newsletter_dir, "eai_components_db.csv"))


# GDP_factors |> view()

GDP_factors_full_year <- 
  GDP_factors |> 
  filter(!is.na(year)) |> 
  filter(month == 13) |> 
  select(-date) |> 
  pivot_longer(-c(year, month, name), names_to = "factor") |> 
  pivot_wider() |> 
  select(-month) |> 
  janitor::clean_names() |> 
  group_by(year) |> 
  mutate(
    factor_pct = mln_dram / sum(mln_dram)
  ) |> 
  ungroup() |> 
  arrange(desc(year))

GDP_factors_full_year |> 
  ggplot(aes(year, mln_dram, color = factor)) +
  geom_line()

GDP_factors_full_year |> 
  mutate(
    factor = fct_inorder(factor),
    yoy_growth = yoy_growth/100-1,
    yay_growth_text = percent(yoy_growth, accuracy = 0.1)
  ) |> 
  ggplot(aes(1, yoy_growth, fill = factor)) +
  facet_grid(~year) +
  geom_col(position = position_dodge(), alpha = 1) +
  geom_text(
    aes(
      label = yay_growth_text,
      vjust = ifelse(yay_growth_text >= 0, -0.5, 1.5)
    ),
    position = position_dodge(width = 0.9), size = 3
  ) +
  scale_y_continuous(breaks = seq(-0.1,0.3,0.05), labels = percent_format()) +
  scale_fill_manual(
    values = colfunc2(5),
    labels = c("Արդյունաբերություն", "Գյուղատնտեսություն", "Շինարարություն", "Առևտուր", "Ծառայություններ")
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Տնտեսական ակտիվության ինդեքսի բաղադրիչների աճը",
    subtitle = "տարեկան",
    caption = caption_f()
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "gray", linewidth = 0.1,
      linetype = 1
    ),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )


ggsave(
  file.path(newsletter_dir, "plots", "plot_eai_2024_annual_factors.png"), 
  ggplot2::last_plot(), width = 12, height = 8
)


GDP_factors_full_year |> 
  mutate(
    # factor = fct_inorder(factor),
    factor_pct_txt = percent(factor_pct, accuracy = 0.1)
  ) |> 
  ggplot(aes(year, factor_pct, fill = factor, label = factor_pct_txt)) +
  geom_col(alpha = 1) +
  geom_text(position = position_stack(vjust = .5), color = "white") +
  scale_x_continuous(breaks = 2017:2024,  position = "top") +
  scale_fill_manual(
    values = colfunc2(5),
    labels = c("Գյուղատնտեսություն", "Շինարարություն", "Արդյունաբերություն", "Ծառայություններ", "Առևտուր")
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Տնտեսական ակտիվության ինդեքսի բաղադրիչները",
    subtitle = "2017-2024 թթ․ տեսակարար կշիռ",
    caption = caption_f()
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x.top = element_text(size = 13, vjust = -4),
    legend.text = element_text(size = 12)
  )


ggsave(
  file.path(newsletter_dir, "plots", "plot_eai_2024_annual_share.png"), 
  ggplot2::last_plot(), width = 12, height = 8
)

##################################

GDP_factors_month <- 
  GDP_factors |> 
  filter(!is.na(year)) |> 
  filter(month != 13) |> 
  mutate(date = ym(paste(year, month)) + months(1) - days(1)) |> 
  pivot_longer(-c(date, year, month, name), names_to = "factor") |> 
  pivot_wider() |> 
  janitor::clean_names() |> 
  arrange(factor, date) |> 
  group_by(factor) |> 
  mutate(
    mln_dram_yoy = roll_sumr(mln_dram, 12),
    across(contains("growth"), ~.x/100)
  )
# 
# GDP_factors_month |> 
#   group_by(factor) |> 
#   mutate(
#     month_growth_2 = mln_dram / lag(mln_dram),
#     yoy_growth_2 = mln_dram / lag(mln_dram, 12)
#   ) |> 
#   view()


GDP_factors_month |> 
  ggplot(aes(date, mln_dram, color = factor)) +
  geom_line()

GDP_factors_month |> 
  ggplot(aes(date, mln_dram_yoy, color = factor)) +
  geom_line()


GDP_factors_month |> 
  filter(factor != "agriculture") |> 
  ggplot(aes(date, yoy_growth, color = factor)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~factor)


#############################################################


cpi <- 
  rio::import("https://www.cba.am/stat/stat_data_eng/6_CPI_eng.xls",  skip = 3) |> 
  as_tibble() |> 
  rename(
    date = 1,
    cpi_m = 2,
    cpi_m_food = 3,
    cpi_m_non_food = 4,
    cpi_m_services = 5,
    cpi_to_december = 7,
    cpi_year = 8,
    cpi_cummulative = 9
  ) |> 
  select(-6) |> 
  filter(!is.na(cpi_m)) |>  
  mutate(
    date = ym(date) + months(1) - days(1),
    across(where(is.numeric), ~ .x/100)
  )

cpi$date |> max()



# GDP_factors_month |> f
#   summarise(
#     text = prod(month_growth)
#   )
# 
# GDP_factors_month |> 
#   left_join(cpi |> select(date, cpi_m, cpi_year), by = "date") |> 
#   mutate(
#     test_1 = mln_dram_yoy/lag(mln_dram_yoy, 12),
#     test_2 = test_1 / cpi_year
#   ) |> 
#   filter(factor == "industry", year == 2024, month == 12) |> 
#   view()

economic_activity <- 
  GDP_factors_month |> 
  # mutate(
  #   factor == fct_inorder(factor)
  # ) |> 
  select(date, year, month, factor, mln_dram, mln_dram_yoy) |> 
  group_by(factor) |> 
  mutate(
    growth_year = mln_dram_yoy/lag(mln_dram_yoy, 12),
  ) |>
  # left_join(cpi |> select(date, cpi_year), by = "date") |> 
  # mutate(
  #   yoy_test_2 = yoy_test / cpi_year,
  # ) |> 
  group_by(year, factor) |> 
  mutate(
    mln_dram_cumsum = cumsum(mln_dram),
  ) |> 
  ungroup() |> 
  group_by(factor) |> 
  mutate(   cum_growth = mln_dram_cumsum/lag(mln_dram_cumsum, 12)) |> 
  ungroup()
  
  # filter(factor == "industry") |> 
  # view()
  # 
  # 
  # 
  # 
  # filter(year %in% 2023:2024, month == 12) |> 
  # group_by(factor) |> 
  # mutate(
  #   test = mln_dram_yoy/lag(mln_dram_yoy)
  # ) |> 
  # select(year, factor, test) |> 
  # filter(!is.na(test))
# 
# 
# economic_activity <-
#   GDP_factors_month |> 
#   filter(factor != "agriculture") |> 
#   # group_by(factor) |> 
#   # mutate(
#   #   growth_month = mln_dram / lag(mln_dram),
#   #   growth_yoy_1 = mln_dram / lag(mln_dram, 12),
#   #   growth_yoy_2 = roll_prodr(growth_month, 12),
#   #   growth_yoy_3 = roll_prodr(month_growth, 12)    
#   #   ) |> 
#   # view()
#   
#   mutate(
#     growth_month = mln_dram/lag(mln_dram),
#     growth_year = mln_dram_yoy/lag(mln_dram_yoy, 12)
#   )

factor_dict <- 
  tibble(
    factor_arm = c("Գյուղատնտեսություն", "Շինարարություն", "Արդյունաբերություն", "Ծառայություններ", "Առևտուր"),
    factor = c("agriculture", "construction", "industry", "services", "trade")
  )

economic_activity |> 
  filter(factor != "agriculture", !is.na(growth_year)) |> 
  select(factor, date, growth_year) |> 
  pivot_longer(growth_year) |> 
  left_join(factor_dict, by = "factor") |> 
  ggplot(aes(date, value - 1, color = factor)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line(size = 1.5, alpha = 1) +
  # geom_smooth() +
  # geom_line(aes(date, growth_adjusted_month)) +
  # geom_line(aes(date, growth_month), color = "red") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 8, labels = percent_format()) +
  scale_color_manual(values = colfunc2(5)[1:4]) +
  facet_wrap(~factor_arm, scales = "free_x") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Տնտեսական ակտիվության ցուցանիշի (ՏԱՑ) բաղադրիչների փոփոխությունը",
    subtitle = "Տարեկան կտրվածքով, տվյալ 12 ամիսը նախորդ տարվա նույն ժամանակահատվածի համեմատ",
    caption =  caption_f("")
  ) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "gray", linewidth = 0.1,
      linetype = 1
    ),
    strip.text = element_text(size = 12),
  )

ggsave(
  file.path(newsletter_dir, "plots", "plot_eai_2024_yoy_change.png"), 
  ggplot2::last_plot(), width = 12, height = 8
)

# economic_activity %>% 
#   filter(!is.na(growth_month)) |> 
#   ggplot() +
#   geom_line(aes(date, iea / 100)) +
#   geom_line(aes(date, iea_adjusted / 100), color = "red") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   scale_y_continuous(labels = percent_format(), breaks = seq(0.4, 1.6, 0.2)) +
#   labs(
#     x = NULL, y = "2019 = 100%",
#     title = "Տնտեսական ակտիվության ցուցանիշ"
#   )
# 
# economic_activity |> 
#   filter(
#     year >= 2020,
#   ) |> 
#   na.omit() |> 
#   ggplot(aes(date, growth_adjusted_year - 1)) +
#   geom_line() +
#   scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") +
#   scale_y_continuous(breaks = seq(-0.2,0.2,0.02), labels = percent_format())

##########################################

economic_activity |> count(factor)


economic_activity |> 
  # filter(year >= max(year) - 1) |>
  filter(month != 12) |> 
  bind_rows(
    GDP_factors_full_year |> 
      filter(
        year %in% c(2023:2024)
      ) |> 
      transmute(
        date = ym(paste(year, 12)) + month(12) - days(1),
        year,
        factor,
        mln_dram_yoy = mln_dram,
        growth_year = yoy_growth / 100,
        mln_dram_cumsum = mln_dram,
        cum_growth = yoy_growth / 100,
      )
  ) |> 
  filter(
    year %in% c(2023:2024),
    factor != "agriculture",
    # factor == "industry"
  ) |> 
  arrange(date) |> 
  mutate(
    month = month(date),
    month_name = format(date, "%b"),
    month_name = fct_inorder(month_name),
  ) |> 
  left_join(factor_dict, by = "factor") |> 
  ggplot(aes(
    month_name,
    growth_year - 1, 
    fill = as.character(year),
    label = percent(growth_year - 1, accuracy = 0.1),
  )) +
  facet_wrap(~factor_arm, scale = "free") +
  geom_col(
    position = position_dodge(preserve = "single"), alpha = 1,
  ) +
  geom_line(
    aes(x = month, y = cum_growth - 1, color = as.character(year)),
    alpha = 1, size = 1.5
  ) +
  geom_text(
    aes(y = growth_year - 1 + 0.01),
    # position = position_dodge(preserve = "single")
    position = position_dodge(width = 1), 
    size = 2.5,
    # size = 4
  ) +
  scale_y_continuous(
    labels = percent_format(), 
    n.breaks = 8,
  ) +
  scale_fill_manual(values = new_palette_colors[c(2,6)]) +
  scale_color_manual(values = new_palette_colors[c(3,7)]) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL, fill = NULL, 
    title = "ՏԱՑ բաղադրիչներ, 2023 և 2024 թվականների համեմատություն",
    subtitle = "Նախորդ տարվա նույն ամսվա համեմատ (ճշգրտված),\nՀետագիծը ներկայացնում է տվյալ տարվա կուտակային աճը",
    caption = caption_f(source = "ԱՎԾ")
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "gray", linewidth = 0.1,
      linetype = 1
    ),
    axis.text.x = element_text(vjust = 6),
    legend.text = element_text(size = 16),
  )

ggsave(
  file.path(newsletter_dir, "plots", "plot_eai_2024_2023_all.png"), 
  ggplot2::last_plot(), width = 12, height = 8
)


ggsave(
  file.path(newsletter_dir, "plots", "plot_eai_2024_2023_industry.png"), 
  ggplot2::last_plot(), width = 12, height = 8
)


#############

# Create the data
data <- tribble(
  ~indicator, ~value, ~color, ~text_color,
  "Инфляция", 0.3, "#1B4D3E", "#B86B25",
  "Промышленность", 4.7, "#1B4D3E", "#B86B25",
  "Экономическая\nактивность", 8.0, "#1B4D3E", "#B86B25",
  "Услуги", 10.6, "#1B4D3E", "#2B4D3E",
  "Строительство", 14.5, "#1B4D3E", "#2B4D3E",
  "Торговля", 17.0, "#1B4D3E", "#2B4D3E",
  "Импорт", 33.8, "#1B4D3E", "#2B4D3E",
  "Экспорт", 53.1, "#1B4D3E", "#2B4D3E"
)

# Create the main plot
p <- ggplot(data, aes(x = reorder(indicator, value), y = value)) +
  # Add gradient background
  annotation_custom(
    grid::rectGrob(
      gp = grid::gpar(fill = "white", alpha = 0.3),
      width = unit(1, "npc"),
      height = unit(1, "npc")
    )
  ) +
  # Add bars
  geom_col(fill = "#1B4D3E", width = 0.7) +
  # Add text
  geom_text(aes(label = sprintf("+%.1f%%", value), color = text_color), 
            hjust = -0.2,
            size = 4) +
  # Customize colors
  scale_color_identity() +
  # Flip coordinates for horizontal bars
  coord_flip() +
  # Add labels
  labs(
    title = "Основные экономические показатели\nАрмении за 2024 год",
    subtitle = "Рост в % (в сравнении с 2023 г.)",
    caption = "Источник: armstat.am",
    x = NULL,
    y = NULL
  ) +
  # Customize theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
    plot.background = element_rect(
      fill = "#F5F7F7",
      color = NA
    ),
    panel.background = element_rect(
      fill = "#F5F7F7",
      color = NA
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E0E5E5", linetype = "dashed"),
    axis.text.y = element_text(
      size = 10, 
      margin = margin(r = 10),
      color = data$text_color[match(levels(reorder(data$indicator, data$value)), data$indicator)]
    ),
    axis.text.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  ) +
  # Extend y-axis for labels
  scale_y_continuous(
    limits = c(0, max(data$value) * 1.2),
    expand = expansion(mult = c(0, 0.1))
  )

# Print the plot
p