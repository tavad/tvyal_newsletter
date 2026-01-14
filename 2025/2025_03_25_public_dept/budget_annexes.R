library(readxl)
library(scales)

source("~/R/newsletter/initial_setup.R")

tribble(
  ~year, ~budget_plan, ~debt_payments,
  2025, 3482409450.9, 395251120.2,
  2024, 3017259848.0, 322225518.2,
  2023, 2590973151.1, 272994148.2,
  2022, 2184040185.1, 213921425.5,
  2021, 1850877541.2, 195159513.5,
  2020, 1855697119.5, 168184641.6,
  2019, 1648063122.3, 158125278.2,
  2018, 1465200573.2, NA
) |> 
  mutate(pct_dept = debt_payments / budget_plan)


minfin_dir <- "/home/tavad/R/projects/minfin/budget_anexes"

budget_annex_1 <- 
  tibble(
    files = list.files(minfin_dir, full.names = TRUE, recursive = TRUE)
  ) |> 
  extract(
    files, into = c("year", "annex"), regex = paste0(minfin_dir, "\\/(.*)\\/(.*)$"),
    remove = FALSE
  ) |> 
  mutate(
    year = str_extract(year, "\\d{4}"),
    extention = str_replace(files, ".*\\.(\\w{3,4})$", "\\1"),
    # annex = str_to_lower(annex),
    # annex = str_replace(annex, ".ավելված", "havelvac"),
    # annex = str_replace(annex, "havelvac\\.", "havelvac "),
    # annex = str_replace(annex, "havelvac", "havelvac "),
    # annex = str_replace_all(annex, " +", " ")
  ) |> 
  filter(
    !is.na(year), year >= 2019,
    grepl("xls", extention)
  ) |> 
  group_by(year) |> 
  slice_head(n = 1) |> 
  mutate(
    table = map(files, read_excel, col_names = FALSE)
  ) |> 
  unnest(table)
  
budget_annex_clean <- 
  budget_annex_1 |> 
  select(-files, -annex, -extention) |> 
  rename(
    code = `...1`, code_name = `...2`, value = `...3`
  ) |> 
  mutate(
    value = parse_number(value)
  ) |> 
  filter(
    !is.na(value), !is.na(code)
  ) |> 
  group_by(year) |> 
  mutate(
    total = ifelse(code == "Ընդամենը", value, NA)
  ) |> 
  fill(total, .direction = "down") |> 
  mutate(
    pct = value / total,
    rank = length(value) - rank(value)
  ) |> 
  ungroup()

budget_annex_clean |> write_excel_csv("budget_annex_clean.csv")

budget_annex_clean |> 
  filter(code == "1006")

budget_annex_clean |> 
  filter(!is.na(code_name)) |> 
  filter(value >= 0) |> 
  mutate(
    code_name = fct_lump_n(code_name, n = 7, w = pct, other_level = "Այլ ծախսեր")
  ) |> 
  group_by(year, code_name, total) |> 
  summarise(
    value = sum(value), pct  = sum(pct), rank = min(rank),
    .groups = "drop"
  ) |> 
  mutate(
    code_name = fct_reorder(code_name, pct, .desc = TRUE),
    code_name = fct_relevel(code_name, "Այլ ծախսեր", after = Inf),
    text_label = ifelse(
      pct >= 0.08,
      paste0(number(value / 1e6, accuracy = 0.1, suffix = " Դ"), "\n"), 
      ""
    ),
    text_label = paste0(text_label, percent(pct, accuracy = 0.1))
  ) |> 
  ggplot(aes(year, pct)) +
  geom_col(aes(fill = code_name), alpha = 1) +
  geom_text(
    aes(label = text_label, fill = code_name), 
    position = position_stack(vjust = 0.5), 
    color = "white"
  ) +
  geom_text(
    data = budget_annex_clean |> filter(is.na(code_name)), 
    mapping = aes(year, 1.04, label = number(value / 1e6, accuracy = 0.1, suffix = " Դ"))
  ) +
  # geom_text(
  #   data = tibble(label = "Ընդհամենը բյուջետային ծախսեր, մլրդ ՀՀ դրամ", code_name = NA), 
  #   mapping = aes("2019", 1.1, label = label), hjust = 0.05
  # ) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = percent_format()) +
  scale_fill_manual(values = new_palette_colors) +
  guides(fill = guide_legend(nrow = 3)) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "ՀՀ պետական բյուջեի հիմնական ծախսերը",
    subtitle = "Ընդհամենը բյուջետային ծախսեր, մլրդ ՀՀ դրամ",
    caption = caption_f(source = "Ֆինանսների նախարարություն")
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )
  



