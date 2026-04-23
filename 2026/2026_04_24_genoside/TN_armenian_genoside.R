# TN_armenian_genoside.R — Newsletter 2026-04-24
# "111 Years: How the World Remembers"
# 4 plots × 3 languages = 12 PNG output files

library(rvest)
library(tidyverse)
library(countrycode)
library(ggflags)
library(ggrepel)

if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
source("../../initial_setup.R")

# ── output dirs ───────────────────────────────────────────────────────────────
ARTICLE_SLUG <- "2026-04-24-genocide"
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


# ══════════════════════════════════════════════════════════════════════════════
# DATA — loaded once, shared across all plot functions
# ══════════════════════════════════════════════════════════════════════════════

# plot_01: recognition timeline
recognition_raw <- read_html("https://en.wikipedia.org/wiki/Armenian_genocide_recognition") |>
  html_elements("table") |>
  html_table() |>
  pluck(1) |>
  set_names(c("country", "year", "note"))

recognition_df <- recognition_raw |>
  mutate(
    country = str_remove_all(country, "\\[.*?\\]") |> str_trim(),
    iso2c   = tolower(countrycode(country, origin = "country.name", destination = "iso2c")),
    year    = str_remove_all(year, "\\[.*?\\]")
  ) |>
  separate_rows(year, sep = ",\\s*") |>
  mutate(year = as.integer(year)) |>
  filter(!is.na(year), !is.na(iso2c), year >= 1990) |>
  arrange(year) |>
  group_by(year) |>
  mutate(stack_pos = row_number()) |>
  ungroup()

year_counts <- recognition_df |> count(year, name = "n")

# plot_02: Google Trends
trends_df <- read_csv("time_series_Worldwide_20040101-0400_20260423-2232.csv",
                      show_col_types = FALSE) |>
  rename(date = 1, hits = 2) |>
  mutate(
    date = as.Date(paste0(date, "-01")),
    hits = suppressWarnings(as.integer(hits)),
    hits = replace_na(hits, 0L)
  )

# plot_03: Tsitsernakaberd visits
visits_raw <- read_html("https://en.wikipedia.org/wiki/List_of_visitors_to_Tsitsernakaberd") |>
  html_elements("#mw-content-text") |>
  html_text()

visits_df <- tibble(text = visits_raw) |>
  mutate(years = str_extract_all(text, "\\b(199[0-9]|20[0-2][0-9])\\b")) |>
  unnest(years) |>
  mutate(year = as.integer(years)) |>
  filter(year >= 1991, year <= 2026) |>
  count(year, name = "delegations")

vip_visits <- tribble(
  ~year, ~visitor,            ~iso2c,
  2001,  "Pope John Paul II", "va",
  2001,  "Jacques Chirac",    "fr",
  2015,  "Vladimir Putin",    "ru",
  2015,  "François Hollande", "fr",
  2016,  "Pope Francis",      "va",
  2018,  "Emmanuel Macron",   "fr",
  2026,  "JD Vance",          "us"
) |>
  left_join(visits_df, by = "year") |>
  group_by(year) |>
  mutate(
    y_flag = if (n() == 1) {
      first(delegations) * 0.65
    } else {
      seq(first(delegations) * 0.30, first(delegations) * 0.72, length.out = n())
    }
  ) |>
  ungroup()

# plot_04: Google Scholar
scholar_df <- read_csv("armenian_genocide_scholar.csv", show_col_types = FALSE) |>
  filter(year >= 1960)


# ══════════════════════════════════════════════════════════════════════════════
# LABELS — one list per language
# ══════════════════════════════════════════════════════════════════════════════
labels <- list(

  hy = list(
    locale  = "arm",
    dir_key = "hy",

    p01_title = "Միջազգային ճանաչում. Հայոց ցեղասպանության ճանաչումն ըստ պետությունների, 1990–2026թթ.",
    p01_sub   = "Յուրաքանչյուր դրոշ խորհրդանշում է պետական մակարդակով մեկ պաշտոնական ճանաչում։\n2015թ. (100-րդ տարելիցի) ընթացքում արձանագրվել է ճանաչման 12 նոր դեպք։",

    p02_title  = "Որոնողական ակտիվության դինամիկան",
    p02_sub    = "«Հայոց ցեղասպանություն» եզրույթի որոնողական հետաքրքրությունը Google համակարգում\n(համաշխարհային ամսական տվյալներ, 2004–2026թթ., 100 = առավելագույն ցուցանիշ, ապրիլ 2015թ.)։",
    p02_spike1 = "100-րդ տարելից\n(2015)",
    p02_spike2 = "ԱՄՆ նախագահի կողմից ճանաչում\n(2021)",
    p02_y      = "Հարաբերական հետաքրքրություն (0–100)",

    p03_title = "Պաշտոնական այցեր Ծիծեռնակաբերդ. Օտարերկրյա պատվիրակություններ, 1991–2026թթ.",
    p03_sub   = "Երևանում Հայոց ցեղասպանության հուշահամալիր այցելած օտարերկրյա պաշտոնական պատվիրակությունների ընդհանուր քանակը։\nՆարնջագույն նշիչով ներկայացված է 2015թ. արձանագրված առավելագույն ցուցանիշը (220)։",
    p03_y     = "Պատվիրակություններ",

    p04_title = "Ակադեմիական հրապարակումներ. Հայոց ցեղասպանության վերաբերյալ գիտական հոդվածներ, 1960–2025թթ.",
    p04_sub   = "Google Scholar որոնողական համակարգի տարեկան ցուցանիշները։Ի տարբերություն դիվանագիտական ակտիվության,\nգիտական հրապարակումների ծավալը պահպանվում է 2015թ. 100-րդ տարելիցին արձանագրված մակարդակին մոտ։",
    p04_y     = "Հրապարակումներ"
  ),

  en = list(
    locale  = "eng",
    dir_key = "en",

    p01_title = "The World Recognizes: Armenian Genocide Recognition by Country, 1990–2026",
    p01_sub   = "Each flag = one formal state recognition.\n2015 (the centennial) produced 12 in a single year.",

    p02_title  = "Once a Year, the World Searches",
    p02_sub    = "Google search interest in \"Armenian Genocide\",\nworldwide monthly 2004–2026 (100 = peak month, April 2015).",
    p02_spike1 = "100th Anniversary\n(2015)",
    p02_spike2 = "Biden recognition\n(2021)",
    p02_y      = "Relative interest (0–100)",

    p03_title = "The Pilgrimage to Tsitsernakaberd: Official Delegations, 1991–2026",
    p03_sub   = "Total recorded official foreign visits to the Armenian Genocide Memorial in Yerevan.\nOrange bar = 2015 centennial peak (220).",
    p03_y     = "Delegations",

    p04_title = "The Academic Record: Scholarly Publications on the Armenian Genocide, 1960–2025",
    p04_sub   = "Annual Google Scholar results.\nUnlike diplomatic attention, academic output has remained near the 2015 centennial level.",
    p04_y     = "Publications"
  ),

  ru = list(
    locale  = "rus",
    dir_key = "ru",

    p01_title = "Международное признание: признание Геноцида армян по государствам, 1990–2026 гг.",
    p01_sub   = "Каждый флаг обозначает факт официального признания на государственном уровне.\nВ 2015 году (в период 100-летней годовщины) было зафиксировано 12 актов признания.",

    p02_title  = "Динамика поисковых запросов",
    p02_sub    = "Динамика интереса к поисковому запросу «Геноцид армян» в системе Google\n(глобальные ежемесячные данные, 2004–2026 гг.; 100 = максимальный показатель, апрель 2015 г.).",
    p02_spike1 = "100-летняя годовщина\n(2015)",
    p02_spike2 = "Признание президентом США\n(2021)",
    p02_y      = "Относительный интерес (0–100)",

    p03_title = "Официальные визиты в мемориальный комплекс «Цицернакаберд»: иностранные делегации, 1991–2026 гг.",
    p03_sub   = "Общее количество официально зафиксированных визитов иностранных делегаций в мемориальный комплекс памяти жертв Геноцида армян в г. Ереван.\nОранжевым цветом выделен пиковый показатель 2015 года (220 визитов).",
    p03_y     = "Делегации",

    p04_title = "Академические исследования: научные публикации, посвященные Геноциду армян, 1960–2025 гг.",
    p04_sub   = "Ежегодные показатели базы данных Google Scholar. В отличие от активности в дипломатической сфере,\nобъем научных публикаций сохраняется на уровне показателей 2015 года (периода 100-летней годовщины).",
    p04_y     = "Публикации"
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# PLOT 01 — flag stacked timeline: recognition by country, 1990–2026
# ══════════════════════════════════════════════════════════════════════════════
make_p01 <- function(lbl) {
  use_locale(lbl$locale)

  ggplot(recognition_df, aes(x = year, y = stack_pos)) +
    geom_flag(aes(country = iso2c), size = 10) +
    geom_text(
      data = year_counts |> filter(n >= 2),
      aes(x = year, y = n + 1, label = n),
      inherit.aes = FALSE,
      family = "GHEA Mariam", fontface = "bold", size = 4.5,
      color = new_palette_colors[1]
    ) +
    scale_y_continuous(
      name   = NULL,
      breaks = NULL,
      limits = c(0, max(recognition_df$stack_pos) + 2)
    ) +
    scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
    labs(
      title    = lbl$p01_title,
      subtitle = lbl$p01_sub,
      x        = NULL,
      caption  = caption_f("Wikipedia / MFA Armenia", lbl$locale)
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank()
    )
}


# ══════════════════════════════════════════════════════════════════════════════
# PLOT 02 — Google Trends heartbeat, 2004–2026
# ══════════════════════════════════════════════════════════════════════════════
make_p02 <- function(lbl) {
  use_locale(lbl$locale)

  spike_labels <- tibble(
    date  = as.Date(c("2015-04-01", "2021-04-01")),
    label = c(lbl$p02_spike1, lbl$p02_spike2)
  ) |>
    left_join(trends_df, by = "date")

  ggplot(trends_df, aes(x = date, y = hits)) +
    geom_area(fill = new_palette_colors[1], alpha = 0.2) +
    geom_line(color = new_palette_colors[1], linewidth = 0.8) +
    geom_point(
      data  = spike_labels,
      aes(x = date, y = hits),
      color = new_palette_colors[7], size = 3, inherit.aes = FALSE
    ) +
    geom_text(
      data  = spike_labels,
      aes(x = date, y = hits + 5, label = label),
      family = "GHEA Mariam", size = 4, vjust = 0,
      color = new_palette_colors[1], inherit.aes = FALSE
    ) +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
    scale_y_continuous(limits = c(0, 115), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title    = lbl$p02_title,
      subtitle = lbl$p02_sub,
      x        = NULL,
      y        = lbl$p02_y,
      caption  = caption_f("Google Trends, worldwide monthly", lbl$locale)
    )
}


# ══════════════════════════════════════════════════════════════════════════════
# PLOT 03 — Tsitsernakaberd diplomatic visits, 1991–2026
# ══════════════════════════════════════════════════════════════════════════════
make_p03 <- function(lbl) {
  use_locale(lbl$locale)

  ggplot() +
    geom_col(
      data = visits_df,
      aes(x = year, y = delegations),
      fill = new_palette_colors[1], alpha = 0.65
    ) +
    geom_col(
      data = visits_df |> filter(year == 2015),
      aes(x = year, y = delegations),
      fill = new_palette_colors[7], alpha = 0.9
    ) +
    geom_smooth(
      data     = visits_df,
      aes(x = year, y = delegations),
      method   = "loess", span = 0.45, se = FALSE,
      color    = new_palette_colors[6], linetype = "dashed", linewidth = 0.7
    ) +
    geom_flag(
      data = vip_visits,
      aes(x = year, y = y_flag, country = iso2c),
      size = 9
    ) +
    geom_text_repel(
      data  = vip_visits,
      aes(x = year, y = y_flag, label = visitor),
      family        = "GHEA Mariam",
      size          = 4,
      color         = new_palette_colors[1],
      nudge_y       = 15,
      segment.color = "grey70",
      seed          = 42,
      box.padding   = 0.3
    ) +
    scale_x_continuous(breaks = seq(1991, 2026, by = 5)) +
    labs(
      title    = lbl$p03_title,
      subtitle = lbl$p03_sub,
      x        = NULL,
      y        = lbl$p03_y,
      caption  = caption_f("Wikipedia – List of visitors to Tsitsernakaberd", lbl$locale)
    ) +
    theme(panel.grid.major.x = element_blank())
}


# ══════════════════════════════════════════════════════════════════════════════
# PLOT 04 — Google Scholar publications, 1960–2025
# ══════════════════════════════════════════════════════════════════════════════
make_p04 <- function(lbl) {
  use_locale(lbl$locale)

  ggplot(scholar_df, aes(x = year, y = publications)) +
    geom_col(fill = new_palette_colors[1], alpha = 0.7) +
    geom_col(
      data = scholar_df |> filter(year == 2015),
      aes(x = year, y = publications),
      fill = new_palette_colors[7], alpha = 0.9
    ) +
    geom_smooth(
      method = "loess", span = 0.3, se = FALSE,
      color = new_palette_colors[6], linetype = "dashed", linewidth = 0.7
    ) +
    scale_x_continuous(breaks = seq(1960, 2025, by = 5)) +
    labs(
      title    = lbl$p04_title,
      subtitle = lbl$p04_sub,
      x        = NULL,
      y        = lbl$p04_y,
      caption  = caption_f("Google Scholar", lbl$locale)
    ) +
    theme(panel.grid.major.x = element_blank())
}


# ══════════════════════════════════════════════════════════════════════════════
# GENERATE ALL PLOTS — 4 plots × 3 languages = 12 files
# ══════════════════════════════════════════════════════════════════════════════
for (lang in c("hy", "en", "ru")) {
  lbl <- labels[[lang]]
  dk  <- lbl$dir_key

  message("\n── language: ", lang, " ─────────────────────────────────────────────")

  save_plot_lang(tvyal_logo(make_p01(lbl)), "plot_01.png", dk, width = 14, height = 7)
  save_plot_lang(tvyal_logo(make_p02(lbl)), "plot_02.png", dk, width = 14, height = 7)
  save_plot_lang(tvyal_logo(make_p03(lbl)), "plot_03.png", dk, width = 14, height = 8)
  save_plot_lang(tvyal_logo(make_p04(lbl)), "plot_04.png", dk, width = 14, height = 7)
}

use_locale("arm")   # reset to Armenian
message("\nDone. 12 plots saved (4 plots × 3 languages).")
