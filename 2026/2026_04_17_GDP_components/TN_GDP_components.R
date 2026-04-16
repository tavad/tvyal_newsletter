# TN_GDP_components.R — Newsletter 2026-04-17
# "Behind the Impressive Numbers: Shadows in Armenia's Economic Growth"
# 5 plots × 3 languages = 15 PNG output files
# (plot_05 stream chart developed but may be excluded from the final article)

library(ragg)
library(tidyverse)
library(rvest)
library(scales)
library(readxl)
library(ggstream)

if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
source("../../initial_setup.R")

# ── output dirs ───────────────────────────────────────────────────────────────
ARTICLE_SLUG <- "2026-04-17-gdp-components"
dirs <- setNames(
  file.path(
    path.expand("~/docs/partnership/website_v4/newsletter"),
    c("en", "hy", "ru"), paste0("2026/", ARTICLE_SLUG, "/plots")
  ),
  c("en", "hy", "ru")
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

save_plot_lang <- function(plot, filename, dir_key, width = 12, height = 7, dpi = 300) {
  ggsave(
    file.path(dirs[[dir_key]], filename), plot,
    width = width, height = height, dpi = dpi, bg = "white",
    device = ragg::agg_png
  )
  message("saved [", dir_key, "]: ", filename)
}

# ── colour aliases ────────────────────────────────────────────────────────────
green <- "#005C4B"
red   <- new_palette_colors[6]   # #f95d6a

# ══════════════════════════════════════════════════════════════════════════════
# DATA DOWNLOAD — set DOWNLOAD_FRESH <- TRUE to re-fetch from Armstat
# ══════════════════════════════════════════════════════════════════════════════
DOWNLOAD_FRESH <- FALSE

if (DOWNLOAD_FRESH) {
  national_account_html_elements <-
    read_html("https://www.armstat.am/am/?nid=202") |>
    html_elements("a")

  national_account_urls <-
    tibble(
      url  = html_attr(national_account_html_elements, "href"),
      text = html_text(national_account_html_elements)
    ) |>
    filter(grepl("^\\.\\./", url)) |>
    mutate(
      text = str_trim(text),
      url  = str_replace(url, "^\\.\\.", "https://www.armstat.am")
    ) |>
    filter(text != "")

  GDP_services_links <-
    national_account_urls |>
    filter(grepl("^ՀՆԱ", text)) |>
    pull(url)

  # quarterly GDP (link index 4 = quarterly volume/value file)
  system(paste0("curl -A 'Mozilla/5.0' \"", GDP_services_links[4], "\" -o \"GDP_quarter.xls\""))

  # annual GDP (link index 1 = annual volume/value file)
  system(paste0("curl -A 'Mozilla/5.0' \"", GDP_services_links[1], "\" -o \"GDP_annual.xls\""))

  message("Download complete.")
}

# ── load & tidy ───────────────────────────────────────────────────────────────
GDP_quarter <- left_join(
  read_excel("GDP_quarter.xls", skip = 4) |>
    rename(code = 1, arm = 2, eng = 3, rus = 4) |>
    pivot_longer(matches("\\d{4}"), names_to = "date", values_to = "production"),
  read_excel("GDP_quarter.xls", skip = 4, sheet = 4) |>
    rename(code = 1, arm = 2, eng = 3, rus = 4) |>
    pivot_longer(matches("\\d{4}"), names_to = "date", values_to = "vol_YoY_pct"),
  by = join_by(code, arm, eng, rus, date)
)
GDP_quarter |> write_excel_csv("GDP_quarter_tidy.csv")

GDP_annual <- left_join(
  read_excel("GDP_annual.xls", skip = 4) |>
    rename(code = 1, arm = 2, eng = 3, rus = 4) |>
    pivot_longer(matches("\\d{4}"), names_to = "year", values_to = "production"),
  read_excel("GDP_annual.xls", skip = 4, sheet = 4) |>
    rename(code = 1, arm = 2, eng = 3, rus = 4) |>
    pivot_longer(matches("\\d{4}"), names_to = "year", values_to = "vol_YoY_pct"),
  by = join_by(code, arm, eng, rus, year)
) |>
  mutate(year = as.integer(year))
GDP_annual |> write_excel_csv("GDP_annual_tidy.csv")

# ── GDP growth series ─────────────────────────────────────────────────────────
GDP_annual_pct <- GDP_annual |>
  filter(grepl("Domestic product", eng)) |>
  transmute(year, GDP_growth = vol_YoY_pct / 100 - 1) |>
  na.omit()

GDP_quarter_pct <- GDP_quarter |>
  filter(grepl("Domestic product", eng)) |>
  transmute(date, source = "revised", GDP_growth = vol_YoY_pct / 100 - 1) |>
  na.omit()

# handy references used in labels and calculations
max_year  <- max(GDP_annual_pct$year)
prev_year <- max_year - 1
gdp_prev  <- GDP_annual_pct |> filter(year == prev_year) |> pull(GDP_growth)
gdp_latest <- GDP_annual_pct |> filter(year == max_year)  |> pull(GDP_growth)

# ── sector name dictionaries (HY / EN / RU) ───────────────────────────────────
# Used in plot labels; EN/RU are short hand-crafted equivalents of the
# Armenian `armenian_short_names` — kept here to avoid depending on the
# full eng/rus column text which contains long legal titles.
sector_names <- list(
  hy = tibble::tribble(
    ~code, ~short,
    "A", "Գյուղատնտեսություն",
    "B", "Հանքագործություն",
    "C", "Արդյունաբերություն",
    "D", "Էլեկտրամատակարարում",
    "E", "Ջրամատակարարում",
    "F", "Շինարարություն",
    "G", "Առևտուր",
    "H", "Փոխադրումներ",
    "I", "Հանրային սնունդ",
    "J", "Տեղ. և կապ",
    "K", "Ֆինանսներ",
    "L", "Անշարժ գույք",
    "M", "Մասն. գործ.",
    "N", "Վարչ. ծառ.",
    "O", "Պետ. կառ.",
    "P", "Կրթություն",
    "Q", "Առողջ.",
    "R", "Մշակույթ",
    "S", "Այլ ծառ.",
    "T", "Տնային տնտ."
  ),
  en = tibble::tribble(
    ~code, ~short,
    "A", "Agriculture",
    "B", "Mining",
    "C", "Manufacturing",
    "D", "Electricity",
    "E", "Water supply",
    "F", "Construction",
    "G", "Trade",
    "H", "Transport",
    "I", "Food service",
    "J", "ICT",
    "K", "Finance",
    "L", "Real estate",
    "M", "Prof. services",
    "N", "Admin. services",
    "O", "Public admin.",
    "P", "Education",
    "Q", "Healthcare",
    "R", "Culture",
    "S", "Other services",
    "T", "Households"
  ),
  ru = tibble::tribble(
    ~code, ~short,
    "A", "Сельское хоз-во",
    "B", "Добыча",
    "C", "Промышленность",
    "D", "Электроснабж.",
    "E", "Водоснабж.",
    "F", "Строительство",
    "G", "Торговля",
    "H", "Транспорт",
    "I", "Общепит",
    "J", "ИКТ",
    "K", "Финансы",
    "L", "Недвижимость",
    "M", "Проф. услуги",
    "N", "Адм. услуги",
    "O", "Гос. управл.",
    "P", "Образование",
    "Q", "Здравоохр.",
    "R", "Культура",
    "S", "Проч. услуги",
    "T", "Домохоз-ва"
  )
)

# ── waterfall group labels (language-aware) ───────────────────────────────────
group_labels <- list(
  hy = list(
    other       = "Այլ\nծառա-ներ",
    taxes       = "TA",
    fisim       = "FISIM",
    gdp         = "ՀՆԱ\nաճ",
    energy      = "Էներգիա և ջրամ.",
    taxes_short = "Ապ. հարկ (- սուբ.)",
    positive    = "Դրական",
    negative    = "Բացասական",
    total       = "Ընդհանուր",
    fill_lbl    = "Ազդեցություն՝",
    fisim_note  = "FISIM — Ֆինանս.միջնորդության անուղղ. չափվ. ծառ."
  ),
  en = list(
    other       = "Other\nservices",
    taxes       = "TA",
    fisim       = "FISIM",
    gdp         = "GDP\ngrowth",
    energy      = "Energy & water",
    taxes_short = "Net taxes on products",
    positive    = "Positive",
    negative    = "Negative",
    total       = "Total",
    fill_lbl    = "Impact:",
    fisim_note  = "FISIM — Financial Intermediation Services Indirectly Measured"
  ),
  ru = list(
    other       = "Прочие\nуслуги",
    taxes       = "TA",
    fisim       = "FISIM",
    gdp         = "Рост\nВВП",
    energy      = "Энергия и водоснаб.",
    taxes_short = "Налоги на продукты (нетто)",
    positive    = "Положит.",
    negative    = "Отрицат.",
    total       = "Итого",
    fill_lbl    = "Вклад:",
    fisim_note  = "FISIM — Косвенно изм. услуги фин. посредничества"
  )
)

# ── annual contribution calculation ──────────────────────────────────────────
GDP_year_contribution <-
  GDP_annual |>
  left_join(GDP_annual_pct, by = "year") |>
  group_by(eng) |>
  mutate(
    vol_YoY_pct  = vol_YoY_pct / 100,
    contribution = lag(production) * (vol_YoY_pct - 1),
    contribution = ifelse(grepl("gross,", eng), NA, contribution)
  ) |>
  group_by(year) |>
  mutate(
    contribution = contribution / sum(contribution, na.rm = TRUE) * GDP_growth,
    contribution = ifelse(grepl("Domestic product", eng), vol_YoY_pct - 1, contribution)
  ) |>
  select(-GDP_growth)

groupping_contributions_annual <- function(
  select_year,
  groupped_codes_ = c("H", "I", "L", "M", "N", "P", "Q", "S", "T"),
  lang = "hy"
) {
  gl <- group_labels[[lang]]
  sn <- sector_names[[lang]]

  df <- GDP_year_contribution
  if (!is.null(select_year)) df <- filter(df, year %in% select_year)

  df |>
    left_join(sn, by = "code") |>
    ungroup() |>
    filter(!is.na(contribution)) |>
    mutate(
      groupped_codes = case_when(
        code %in% c("D", "E")           ~ "D+E",
        code %in% groupped_codes_        ~ gl$other,
        grepl("Taxes on products", eng)  ~ gl$taxes,
        grepl("Financial Intermediate", eng) ~ gl$fisim,
        grepl("Domestic product", eng)   ~ gl$gdp,
        TRUE                             ~ code
      ),
      groupped_arm = case_when(
        code %in% c("D", "E")            ~ gl$energy,
        code %in% groupped_codes_         ~ paste(groupped_codes_, collapse = "+"),
        grepl("Domestic product", eng)    ~ NA_character_,
        grepl("Financial Intermediate", eng) ~ NA_character_,
        grepl("Taxes on products", eng)   ~ gl$taxes_short,
        code %in% LETTERS                 ~ short,
        TRUE                              ~ NA_character_
      )
    ) |>
    group_by(groupped_codes, year, groupped_arm) |>
    summarise(contribution = sum(contribution), .groups = "drop") |>
    group_by(year) |>
    arrange(year) |>
    mutate(
      labels         = percent(contribution, accuracy = 0.1),
      groupped_codes = fct_reorder(groupped_codes, contribution),
      groupped_codes = fct_relevel(groupped_codes, gl$taxes, gl$fisim, gl$gdp, after = Inf),
      id             = as.numeric(groupped_codes)
    ) |>
    arrange(id) |>
    mutate(
      annotation = ifelse(
        is.na(groupped_arm), " ",
        paste(str_replace(as.character(groupped_codes), "\n", " "), groupped_arm, sep = " — ")
      ),
      annotation = fct_reorder(annotation, id),
      end   = cumsum(contribution),
      start = end - contribution,
      end   = ifelse(groupped_codes == gl$gdp, 0, end),
      start = ifelse(groupped_codes == gl$gdp, contribution, start),
      fill_ = case_when(
        groupped_codes == gl$gdp ~ gl$total,
        contribution < 0         ~ gl$negative,
        TRUE                     ~ gl$positive
      )
    ) |>
    ungroup()
}

# ── quarterly contribution calculation ────────────────────────────────────────
GDP_quarter_contribution <-
  GDP_quarter |>
  mutate(source = "revised") |>
  left_join(GDP_quarter_pct, by = c("date", "source")) |>
  group_by(eng, source) |>
  mutate(
    vol_YoY_pct  = vol_YoY_pct / 100,
    contribution = lag(production) * (vol_YoY_pct - 1),
    contribution = ifelse(grepl("gross,", eng), NA, contribution)
  ) |>
  group_by(date, source) |>
  mutate(
    contribution = contribution / sum(contribution, na.rm = TRUE) * GDP_growth,
    contribution = ifelse(grepl("Domestic product", eng), vol_YoY_pct - 1, contribution)
  ) |>
  select(-GDP_growth)

groupping_contributions <- function(
  select_date,
  groupped_codes_ = c("H", "I", "L", "M", "N", "P", "Q", "S", "T"),
  lang = "hy"
) {
  gl <- group_labels[[lang]]
  sn <- sector_names[[lang]]

  df <- GDP_quarter_contribution
  if (!is.null(select_date)) df <- filter(df, date %in% select_date)

  df |>
    left_join(sn, by = "code") |>
    ungroup() |>
    filter(!is.na(contribution)) |>
    mutate(
      groupped_codes = case_when(
        code %in% c("D", "E")            ~ "D+E",
        code %in% groupped_codes_         ~ gl$other,
        grepl("Taxes on products", eng)   ~ gl$taxes,
        grepl("Financial Intermediate", eng) ~ gl$fisim,
        grepl("Domestic product", eng)    ~ gl$gdp,
        TRUE                              ~ code
      ),
      groupped_arm = case_when(
        code %in% c("D", "E")            ~ gl$energy,
        code %in% groupped_codes_         ~ paste(groupped_codes_, collapse = "+"),
        grepl("Domestic product", eng)    ~ NA_character_,
        grepl("Financial Intermediate", eng) ~ NA_character_,
        grepl("Taxes on products", eng)   ~ gl$taxes_short,
        code %in% LETTERS                 ~ short,
        TRUE                              ~ NA_character_
      )
    ) |>
    group_by(groupped_codes, date, source, groupped_arm) |>
    summarise(contribution = sum(contribution), .groups = "drop") |>
    group_by(date, source) |>
    arrange(date, source) |>
    mutate(
      labels         = percent(contribution, accuracy = 0.1),
      groupped_codes = fct_reorder(groupped_codes, contribution),
      groupped_codes = fct_relevel(groupped_codes, gl$taxes, gl$fisim, gl$gdp, after = Inf),
      id             = as.numeric(groupped_codes)
    ) |>
    arrange(id) |>
    mutate(
      annotation = ifelse(
        is.na(groupped_arm), " ",
        paste(str_replace(as.character(groupped_codes), "\n", " "), groupped_arm, sep = " — ")
      ),
      annotation = fct_reorder(annotation, id),
      end   = cumsum(contribution),
      start = end - contribution,
      end   = ifelse(groupped_codes == gl$gdp, 0, end),
      start = ifelse(groupped_codes == gl$gdp, contribution, start),
      fill_ = case_when(
        groupped_codes == gl$gdp ~ gl$total,
        contribution < 0         ~ gl$negative,
        TRUE                     ~ gl$positive
      )
    ) |>
    ungroup()
}

# ══════════════════════════════════════════════════════════════════════════════
# LABELS — one list per language
# ══════════════════════════════════════════════════════════════════════════════
# Note: gdp_prev / gdp_latest are derived above from the data, so legend
# numbers always reflect the freshly downloaded data.

labels <- list(

  hy = list(
    locale  = "arm",
    dir_key = "hy",

    # plot_01: annual growth bars
    p01_title = "ՀՆԱ-ի աճի տարեկան ցուցանիշը",
    p01_sub   = "Տոկոսային աճ",

    # plot_02: sector dumbbell
    p02_title   = "ՀՆԱ-ի աճի տարբերությունները",
    p02_sub     = "Տնտ. ճյուղերի աճի համեմատական պատկեր",
    p02_leg_old = paste0(prev_year, "թ․ ՀՆԱ աճ՝ ", percent(gdp_prev,   accuracy = 0.1)),
    p02_leg_new = paste0(max_year,  "թ․ ՀՆԱ աճ՝ ", percent(gdp_latest, accuracy = 0.1)),

    # plot_03: quarterly bars (quarter labels for facets)
    p03_title  = "ՀՆԱ-ի աճի ցուցանիշը",
    p03_sub    = "Նախորդ տարվա համեմատ, եռամսյակային ցուցանիշ, %",
    p03_q_sfx  = c("-ին եռ.", "-րդ եռ.", "-րդ եռ.", "-րդ եռ."),
    p03_annual = "ՏԱՐԵԿԱՆ",

    # plot_04: annual waterfall
    p04_title = paste0(max_year, "թ․ ՀՆԱ-ի աճին նպաստ. գործոնները"),
    p04_sub   = "Տոկոսային կետ",

    # plot_05: stream chart
    p05_title   = "Քողարկված աճ․ Ինչո՞վ է ապրում Հայ. տնտ.",
    p05_sub     = "Հայ. տնտ. աճի նպաստ. ըստ եռամս.",
    p05_ann_lbl = "2025թ. ՀՆԱ աճ՝ 7.2%"   # update after data refresh
  ),

  en = list(
    locale  = "eng",
    dir_key = "en",

    p01_title = "Armenia's Annual GDP Growth",
    p01_sub   = "Year-on-year percentage change",

    p02_title   = "GDP Growth by Sector",
    p02_sub     = paste0("Comparative sector growth: ", prev_year, " vs ", max_year),
    p02_leg_old = paste0(prev_year, " GDP growth: ", percent(gdp_prev,   accuracy = 0.1)),
    p02_leg_new = paste0(max_year,  " GDP growth: ", percent(gdp_latest, accuracy = 0.1)),

    p03_title  = "Quarterly GDP Growth",
    p03_sub    = "Year-on-year by quarter, %",
    p03_q_sfx  = c("st quarter", "nd quarter", "rd quarter", "th quarter"),
    p03_annual = "ANNUAL",

    p04_title = paste0("Factors Contributing to ", max_year, " GDP Growth"),
    p04_sub   = "Percentage points",

    p05_title   = "Masked Growth: What Is Driving Armenia's Economy",
    p05_sub     = "Contributions to Armenia's economic growth by quarter",
    p05_ann_lbl = "2025 GDP growth: 7.2%"
  ),

  ru = list(
    locale  = "rus",
    dir_key = "ru",

    p01_title = "Ежегодный рост ВВП Армении",
    p01_sub   = "Изменение год к году, %",

    p02_title   = "Различия в росте ВВП по секторам",
    p02_sub     = paste0("Сравнение роста отраслей: ", prev_year, " vs ", max_year),
    p02_leg_old = paste0("Рост ВВП в ", prev_year, "г.: ", percent(gdp_prev,   accuracy = 0.1)),
    p02_leg_new = paste0("Рост ВВП в ", max_year,  "г.: ", percent(gdp_latest, accuracy = 0.1)),

    p03_title  = "Рост ВВП по кварталам",
    p03_sub    = "К аналогичному кв. прошлого года, %",
    p03_q_sfx  = c("-й кв.", "-й кв.", "-й кв.", "-й кв."),
    p03_annual = "ГОДОВОЙ",

    p04_title = paste0("Факторы роста ВВП Армении в ", max_year, " году"),
    p04_sub   = "Процентные пункты",

    p05_title   = "Скрытый рост: что движет экономикой Армении",
    p05_sub     = "Вклад в экономический рост Армении по кварталам",
    p05_ann_lbl = "2025 Рост ВВП: 7,2%"
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# PLOT 01 — annual GDP growth bar chart
# ══════════════════════════════════════════════════════════════════════════════
make_p01 <- function(lbl) {
  use_locale(lbl$locale)

  GDP_annual_pct |>
    mutate(GDP_growth = GDP_growth * 100) |>
    ggplot(aes(year, GDP_growth, fill = year)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.9, alpha = 1) +
    geom_text(
      aes(
        y     = GDP_growth + 0.6,
        label = percent(GDP_growth / 100, accuracy = 0.1),
        color = ifelse(GDP_growth >= 0, "black", "white")
      ),
      position = position_dodge(width = 0.8)
    ) +
    scale_x_continuous(breaks = 2013:max_year) +
    scale_fill_gradientn(colors = colfunc2(100)[70:10]) +
    scale_color_manual(values = c("black", "white"), guide = "none") +
    labs(
      x = NULL, y = NULL, fill = NULL,
      title    = lbl$p01_title,
      subtitle = lbl$p01_sub,
      caption  = caption_f(source = "Armstat", language = lbl$locale)
    ) +
    theme(
      legend.position    = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "gray", linewidth = 0.1),
      axis.text.x        = element_text(size = 14),
      axis.text.y        = element_blank()
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# PLOT 02 — sector dumbbell: prev_year vs max_year
# Uses annotate() for legend dots to avoid the 19-row length warning
# ══════════════════════════════════════════════════════════════════════════════
make_p02 <- function(lbl, lang) {
  use_locale(lbl$locale)
  sn <- sector_names[[lang]]

  legend_data <- tibble(
    x     = 0.123,
    y     = c(2.5, 1.5),
    label = c(lbl$p02_leg_old, lbl$p02_leg_new)
  )

  GDP_annual |>
    left_join(sn, by = "code") |>
    mutate(
      date = ymd(paste(year, "12-31")),
      year = year(date)
    ) |>
    filter(
      date %in% c(max(date), max(date) - years(1)),
      !is.na(code),
      !grepl("^T$", code)
    ) |>
    mutate(
      vol_YoY_pct  = vol_YoY_pct / 100 - 1,
      sector_label = ifelse(is.na(code), short, paste0(short, " (", code, ")")),
      sector_label = str_trunc(sector_label, 40),
      sector_label = fct_reorder(sector_label, vol_YoY_pct, max),
      year         = ifelse(year == min(year), "min_year", "max_year")
    ) |>
    select(sector_label, year, vol_YoY_pct) |>
    pivot_wider(names_from = year, values_from = vol_YoY_pct) |>
    mutate(color = ifelse(min_year > max_year, "#f95d6a", "#005C4B")) |>
    ggplot() +
    geom_vline(xintercept = 0, color = "gray40") +
    geom_segment(
      aes(x = min_year, xend = max_year,
          y = sector_label, yend = sector_label, color = I(color)),
      linewidth = 1.2, lineend = "round", linejoin = "round",
      arrow = arrow(length = unit(0.1, "inches"))
    ) +
    geom_text(
      aes(x = ifelse(min_year < max_year, min_year, max_year),
          y = sector_label, label = sector_label),
      hjust = 1.1, size = 3.5
    ) +
    geom_point(aes(x = min_year, y = sector_label), color = "#f95d6a", size = 3) +
    geom_point(aes(x = max_year, y = sector_label), color = "#005C4B", size = 3) +
    # legend dots — use annotate() to avoid length-1 aesthetic warning
    annotate("point", x = 0.11, y = 2.5, color = "#f95d6a", size = 3) +
    annotate("point", x = 0.11, y = 1.5, color = "#005C4B", size = 3) +
    geom_text(data = legend_data, aes(x, y, label = label), hjust = 0) +
    scale_x_continuous(breaks = seq(-0.2, 1, 0.1), labels = percent_format()) +
    coord_cartesian(clip = "off") +
    labs(
      x = NULL, y = NULL,
      title    = lbl$p02_title,
      subtitle = lbl$p02_sub,
      caption  = caption_f(source = "Armstat", language = lbl$locale)
    ) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y        = element_blank(),
      plot.margin        = margin(10, 10, 10, 150)
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# PLOT 03 — quarterly GDP growth faceted bars (2021 onwards)
# ══════════════════════════════════════════════════════════════════════════════
make_p03 <- function(lbl) {
  use_locale(lbl$locale)

  # quarter suffix lookup: "1" → "1st quarter", etc.
  q_lut <- setNames(
    paste0(1:4, lbl$p03_q_sfx),
    as.character(1:4)
  )

  GDP_quarter_pct |>
    mutate(
      quarter      = str_extract(date, "\\d$") |> as.numeric(),
      quarter_text = q_lut[as.character(quarter)],
      quarter_text = ifelse(is.na(quarter), lbl$p03_annual, quarter_text),
      date         = yq(date) + months(3) - days(1),
      year         = year(date),
      GDP_growth   = GDP_growth * 100
    ) |>
    filter(year >= 2021, source == "revised") |>
    complete(nesting(quarter, quarter_text), year) |>
    ggplot(aes(year, GDP_growth, fill = as.factor(year))) +
    geom_col(position = position_dodge(width = 0.8), width = 0.9, alpha = 1) +
    geom_text(
      aes(
        y     = GDP_growth + 0.4,
        label = percent(GDP_growth / 100, accuracy = 0.1),
        color = ifelse(GDP_growth >= 0, "black", "white")
      ),
      position = position_dodge(width = 0.8)
    ) +
    scale_y_continuous(breaks = seq(0, 16, 2)) +
    scale_fill_manual(values = new_palette_colors[c(2, 4, 5, 6, 8)]) +
    scale_color_manual(values = c("black", "white"), guide = "none") +
    facet_wrap(~quarter_text, nrow = 1, strip.position = "bottom") +
    labs(
      x = NULL, y = NULL, fill = NULL,
      title    = lbl$p03_title,
      subtitle = lbl$p03_sub,
      caption  = caption_f(source = "Armstat", language = lbl$locale)
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "gray", linewidth = 0.1),
      axis.text          = element_blank(),
      strip.background   = element_rect(fill = "#2f4b7c11"),
      strip.text         = element_text(size = 10, face = "bold")
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# PLOT 04 — waterfall: annual GDP component contributions (latest year)
# ══════════════════════════════════════════════════════════════════════════════
make_p04 <- function(lbl, lang) {
  use_locale(lbl$locale)
  gl <- group_labels[[lang]]

  groupping_contributions_annual(
    select_year     = max_year,
    groupped_codes_ = c("H", "I", "M", "N", "P", "Q", "R", "S", "T"),
    lang            = lang
  ) |>
    mutate(
      labels = ifelse(groupped_codes == gl$gdp, labels, str_remove(labels, "\\%$"))
    ) |>
    ggplot() +
    geom_hline(yintercept = 0, color = new_palette_colors[3]) +
    geom_rect(
      aes(xmin = id - 0.45, xmax = id + 0.45,
          ymin = end, ymax = start, fill = fill_, linetype = annotation)
    ) +
    geom_text(aes(x = groupped_codes, y = (end + start) / 2, label = labels)) +
    geom_segment(
      aes(x    = id - 0.45,
          xend = ifelse(id == max(id), id + 0.45, id + 0.45 + 1),
          y    = end, yend = end),
      linetype = 1, color = new_palette_colors[3]
    ) +
    scale_y_continuous(labels = percent_format(), n.breaks = 6) +
    scale_fill_manual(values = new_palette_colors[c(6, 2, 8)]) +
    labs(
      x = NULL, y = NULL,
      fill     = gl$fill_lbl,
      linetype = NULL,
      title    = lbl$p04_title,
      subtitle = lbl$p04_sub,
      caption  = caption_f(
        source      = "Armstat",
        language    = lbl$locale,
        suffix_text = gl$fisim_note
      )
    ) +
    guides(
      linetype = guide_legend(override.aes = list(fill = "#FFFFFF00"), nrow = 3),
      fill     = guide_legend(order = 1)
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      legend.text        = element_text(size = 9)
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# PLOT 05 — stream chart: GDP component dynamics (developed, may skip in article)
# Note: keep the factor levels list in sync with whatever groupped_codes appear
#       after filtering — run groupping_contributions(NULL) to inspect, then
#       update the levels vector if the data changes.
# ══════════════════════════════════════════════════════════════════════════════
make_p05 <- function(lbl, lang) {
  use_locale(lbl$locale)
  gl <- group_labels[[lang]]

  tbl_positives <-
    groupping_contributions(
      select_date     = NULL,
      groupped_codes_ = c("H", "I", "M", "N", "P", "Q", "R", "S", "T"),
      lang            = lang
    ) |>
    filter(source == "revised") |>
    left_join(GDP_quarter_pct |> filter(source == "revised"), by = c("date", "source")) |>
    filter(contribution >= 0) |>
    group_by(date, source) |>
    mutate(contribution = contribution / sum(contribution) * GDP_growth) |>
    filter(contribution >= 0)

  last_segment <- tbl_positives |>
    mutate(date = yq(date) + months(3) - days(1)) |>
    ungroup() |>
    filter(date == max(date)) |>
    group_by(date) |>
    summarise(contribution = sum(contribution))

  tbl_positives |>
    ungroup() |>
    mutate(groupped_codes = as.character(groupped_codes)) |>
    arrange(groupped_codes) |>
    mutate(
      date = yq(date) + months(3) - days(1),
      groupped_codes = fct_inorder(groupped_codes),
      # keep in sync with the codes that actually appear — inspect with:
      # groupping_contributions(NULL, lang=lang) |> filter(date==max(date)) |> pull(groupped_codes)
      groupped_codes = factor(
        groupped_codes,
        levels = c("G", "L", "K", "C", "F", "D+E", "A", "O", "B", "J",
                   gl$other, gl$taxes, gl$fisim, gl$gdp)
      )
    ) |>
    filter(
      date >= ymd("2020-01-01"),
      groupped_codes != gl$gdp,
      !grepl("FISIM", as.character(groupped_codes))
    ) |>
    arrange(groupped_codes) |>
    mutate(annotation = fct_inorder(annotation)) |>
    ggplot(aes(date, contribution)) +
    geom_stream(
      aes(fill = annotation),
      type = "ridge", bw = 0.95, extra_span = 0.025, n_grid = 4000
    ) +
    geom_segment(
      data    = last_segment,
      mapping = aes(x = date, y = 0, xend = date, yend = 0.11),
      linewidth = 0.7, linetype = 2, color = new_palette_colors[1]
    ) +
    geom_text(
      data    = last_segment,
      mapping = aes(x = date - days(20), y = 0.11, label = lbl$p05_ann_lbl),
      hjust = 1, vjust = 1.5, color = new_palette_colors[1]
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(
      breaks = seq(-0.18, 0.18, 0.02),
      labels = percent_format(accuracy = 1)
    ) +
    scale_fill_manual(values = colfunc3(12)) +
    guides(fill = guide_legend(ncol = 3)) +
    labs(
      x = NULL, y = NULL, fill = NULL,
      title    = lbl$p05_title,
      subtitle = lbl$p05_sub,
      caption  = caption_f(source = "Armstat", language = lbl$locale)
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# GENERATE ALL PLOTS — 5 plots × 3 languages = 15 files
# ══════════════════════════════════════════════════════════════════════════════
for (lang in c("hy", "en", "ru")) {
  lbl <- labels[[lang]]
  dk  <- lbl$dir_key

  message("\n── language: ", lang, " ─────────────────────────────────────────────")

  save_plot_lang(tvyal_logo(make_p01(lbl)),       "plot_01.png", dk, width = 12, height = 7)
  save_plot_lang(tvyal_logo(make_p02(lbl, lang)), "plot_02.png", dk, width = 12, height = 8)
  save_plot_lang(tvyal_logo(make_p03(lbl)),       "plot_03.png", dk, width = 12, height = 7)
  save_plot_lang(tvyal_logo(make_p04(lbl, lang)), "plot_04.png", dk, width = 12, height = 8)
  save_plot_lang(tvyal_logo(make_p05(lbl, lang)), "plot_05.png", dk, width = 14, height = 8)
}

use_locale("arm")   # reset to Armenian
message("\nDone. 15 plots saved (5 plots × 3 languages).")
