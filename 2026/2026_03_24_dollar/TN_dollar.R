# TN_dollar.R — Newsletter 2026-03-24
# "Dollar Near a 22-Year Low — Armenia's Hidden Currency Risk"
# 2 plots × 3 languages = 6 PNG output files

library(ragg)       # must be first
library(tidyverse)
library(ggrepel)
library(scales)

if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
source("../../initial_setup.R")

# ── output dirs ───────────────────────────────────────────────────────────────
dirs <- setNames(
  file.path(
    path.expand("~/docs/partnership/website_v4/newsletter"),
    c("en", "hy", "ru"), "2026/2026-03-24-dollar/plots"
  ),
  c("en", "hy", "ru")
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

save_plot_lang <- function(plot, filename, dir_key, width = 10, height = 6, dpi = 300) {
  ggsave(
    file.path(dirs[[dir_key]], filename), plot,
    width = width, height = height, dpi = dpi, bg = "white",
    device = ragg::agg_png
  )
  message("saved [", dir_key, "]: ", filename)
}

# ── colours ───────────────────────────────────────────────────────────────────
navy   <- new_palette_colors[1]   # #003f5c
blue   <- new_palette_colors[2]   # #2f4b7c
red    <- new_palette_colors[6]   # #f95d6a
orange <- new_palette_colors[7]   # #ff7c43
teal   <- "#2f9e8f"
grey   <- "#8899aa"

# ── data ──────────────────────────────────────────────────────────────────────
cba <- read_csv(
  path.expand("~/docs/research/2026_03_21_cba_daily/cba_analytical_accounts.csv"),
  show_col_types = FALSE
) |> mutate(date = as.Date(date))

fx_usd <- read_csv(
  path.expand("~/R/Gcapatker/2024_03_24_CBA_FX/CBA_FX_data_cleaned.csv"),
  show_col_types = FALSE
) |>
  mutate(date = as.Date(date)) |>
  filter(FX_ISO == "USD")

# ── labels ────────────────────────────────────────────────────────────────────
labels <- list(
  en = list(
    p1_title    = "CBA's Sterilization Buffer Is Almost Gone",
    p1_subtitle = "Net International Reserves surge while Net Domestic Assets collapse",
    p1_nir      = "NIR — Net International Reserves",
    p1_nda      = "NDA — Net Domestic Assets",
    p1_ylab     = "Billion AMD",
    p1_ann1     = "Jan 14–15\nEmergency repo spike\n(Iranian capital arrives)",
    p1_ann2     = "Jan 26\nCBA officially\nreprices IRR",
    p1_caption  = caption_f("CBA Analytical Accounts (daily)", language = "eng"),

    p2_title    = "The Dram at Its Strongest in 22 Years",
    p2_subtitle = "USD/AMD official CBA rate, 2003–2026",
    p2_ylab     = "AMD per 1 USD",
    p2_current  = "377 today",
    p2_ann09    = "2009\ncrash\n\u221238%",
    p2_ann14    = "2014\nshock\n\u221216%",
    p2_ann22    = "2022\nreversal",
    p2_caption  = caption_f("CBA official FX rates", language = "eng")
  ),
  hy = list(
    p1_title    = "ԿԲ-ի Ստերիլիզացիոն Բուֆերը Գրեթե Սպառվել Է",
    p1_subtitle = "Զուտ միջազգային պահուստներն աճում են, մինչ զուտ ներքին ակտիվները փլուզվում են",
    p1_nir      = "ԶՄՊ — Զուտ միջազգային պահուստներ",
    p1_nda      = "ԶՆԱ — Զուտ ներքին ակտիվներ",
    p1_ylab     = "Մլրդ դրամ",
    p1_ann1     = "Հուն. 14–15\nԱրտակ. ռեպո-սպայք\n(Իրանական կապիտալ)",
    p1_ann2     = "Հուն. 26\nԿԲ-ն պաշտ.\nվերահաշ. ռիալը",
    p1_caption  = caption_f("ՀՀ ԿԲ վերլուծական հաշիվներ (ամենօրյա)", language = "arm"),

    p2_title    = "Դրամն Ամրագույն Կետում Է 22 Տարվա Ընթացքում",
    p2_subtitle = "ԱՄՆ դոլար / ՀՀ դրամ, ԿԲ պաշտոնական փոխարժեք, 2003–2026",
    p2_ylab     = "ՀՀ դրամ 1 ԱՄՆ դոլարի դիմաց",
    p2_current  = "377 այսօր",
    p2_ann09    = "2009\nփլուզ.\n\u221238%",
    p2_ann14    = "2014\nցնցում\n\u221216%",
    p2_ann22    = "2022\nշրջ.",
    p2_caption  = caption_f("ՀՀ ԿԲ պաշտոնական փոխարժեքներ", language = "arm")
  ),
  ru = list(
    p1_title    = "Стерилизационный Буфер ЦБА Почти Исчерпан",
    p1_subtitle = "Чистые международные резервы растут, тогда как чистые внутренние активы обваливаются",
    p1_nir      = "ЧМР — Чистые международные резервы",
    p1_nda      = "ЧВА — Чистые внутренние активы",
    p1_ylab     = "Млрд драмов",
    p1_ann1     = "14–15 янв.\nЭкстрен. спайк репо\n(Иранский капитал)",
    p1_ann2     = "26 янв.\nЦБА офиц.\nпереоценка IRR",
    p1_caption  = caption_f("Аналитические счета ЦБА (ежедневно)", language = "rus"),

    p2_title    = "Драм На 22-летнем Максимуме Силы",
    p2_subtitle = "Официальный курс USD/AMD по данным ЦБА, 2003–2026",
    p2_ylab     = "Драмов за 1 USD",
    p2_current  = "377 сегодня",
    p2_ann09    = "Кризис\n2009\n\u221238%",
    p2_ann14    = "Шок\n2014\n\u221216%",
    p2_ann22    = "Разворот\n2022",
    p2_caption  = caption_f("Официальные курсы ЦБА", language = "rus")
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# PLOT 1 — NIR vs NDA, 2024–2026
# ══════════════════════════════════════════════════════════════════════════════

cba_recent <- cba |>
  filter(date >= "2024-01-01") |>
  select(date, NIR, NDA) |>
  mutate(across(c(NIR, NDA), ~ .x / 1000))   # millions → billions AMD

ann_jan14 <- as.Date("2026-01-14")
ann_jan26 <- as.Date("2026-01-26")

# y-range for annotation placement
nir_max <- max(cba_recent$NIR, na.rm = TRUE)
nda_min <- min(cba_recent$NDA, na.rm = TRUE)
y_top   <- nir_max * 1.02

lang_locale <- c(en = "eng", hy = "arm", ru = "rus")

make_plot1 <- function(lang) {
  lb <- labels[[lang]]
  use_locale(lang_locale[[lang]])

  df_long <- cba_recent |>
    pivot_longer(c(NIR, NDA), names_to = "series", values_to = "value") |>
    mutate(series = recode(series, NIR = lb$p1_nir, NDA = lb$p1_nda))

  ggplot(df_long, aes(date, value, colour = series)) +
    geom_hline(yintercept = 0, colour = grey, linewidth = 0.4, linetype = "dashed") +
    # vertical date markers
    geom_vline(xintercept = ann_jan14, linetype = "dotted",
               colour = red, linewidth = 0.5, alpha = 0.7) +
    geom_vline(xintercept = ann_jan26, linetype = "dotted",
               colour = orange, linewidth = 0.5, alpha = 0.7) +
    geom_line(linewidth = 1.1) +
    # Jan 14 — label placed left of the marker, high up
    annotate("label",
      x = ann_jan14 - 45, y = y_top * 0.97,
      label = lb$p1_ann1,
      fill = "#fff0f0", colour = red, size = 2.9, hjust = 1,
      lineheight = 0.9
    ) +
    # Jan 26 — label placed right of the marker, mid height
    annotate("label",
      x = ann_jan26 + 10, y = nir_max * 0.55,
      label = lb$p1_ann2,
      fill = "#fff8f0", colour = orange, size = 2.9, hjust = 0,
      lineheight = 0.9
    ) +
    scale_colour_manual(values = c(blue, red), name = NULL) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b '%y") +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.08))) +
    labs(
      title    = lb$p1_title,
      subtitle = lb$p1_subtitle,
      x = NULL, y = lb$p1_ylab,
      caption  = lb$p1_caption
    ) +
    theme_tvyal26() +
    theme(legend.position = "bottom")
}

for (lang in c("en", "hy", "ru")) {
  p <- tvyal_logo(make_plot1(lang))
  save_plot_lang(p, "plot1_nir_nda.png", lang)
}
use_locale("arm")  # reset

# ══════════════════════════════════════════════════════════════════════════════
# PLOT 2 — USD/AMD long history, 2003–2026
# ══════════════════════════════════════════════════════════════════════════════

crises <- tibble(
  xmin  = as.Date(c("2008-10-01", "2014-10-01", "2022-02-01")),
  xmax  = as.Date(c("2009-07-01", "2015-07-01", "2022-11-01")),
  label = c("2009", "2014", "2022")
)

current_rate <- 377

make_plot2 <- function(lang) {
  lb <- labels[[lang]]
  use_locale(lang_locale[[lang]])

  # crisis label x = midpoint of shading band
  ann_x <- list(
    "2009" = as.Date("2008-10-01") + as.numeric(as.Date("2009-07-01") - as.Date("2008-10-01")) / 2,
    "2014" = as.Date("2014-10-01") + as.numeric(as.Date("2015-07-01") - as.Date("2014-10-01")) / 2,
    "2022" = as.Date("2022-02-01") + as.numeric(as.Date("2022-11-01") - as.Date("2022-02-01")) / 2
  )

  ggplot(fx_usd |> filter(date >= "2003-01-01"), aes(date, AMD)) +
    geom_rect(
      data = crises,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = label),
      inherit.aes = FALSE, alpha = 0.12, show.legend = FALSE
    ) +
    scale_fill_manual(values = c("2009" = red, "2014" = red, "2022" = orange)) +
    geom_line(colour = navy, linewidth = 0.9) +
    geom_hline(yintercept = current_rate, colour = teal,
               linewidth = 0.8, linetype = "dashed") +
    # "377 today" label
    annotate("label",
      x = as.Date("2005-01-01"), y = current_rate,
      label = lb$p2_current,
      fill = "#e8faf7", colour = teal, size = 3.2, fontface = "bold"
    ) +
    # crisis labels — use label (with background box) for legibility
    annotate("label",
      x = ann_x[["2009"]], y = 545,
      label = lb$p2_ann09,
      fill = "#fff0f0", colour = red, size = 3.2, lineheight = 0.9
    ) +
    annotate("label",
      x = ann_x[["2014"]], y = 545,
      label = lb$p2_ann14,
      fill = "#fff0f0", colour = red, size = 3.2, lineheight = 0.9
    ) +
    annotate("label",
      x = ann_x[["2022"]], y = 505,
      label = lb$p2_ann22,
      fill = "#fff8f0", colour = orange, size = 3.2, lineheight = 0.9
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 expand = expansion(mult = 0.01)) +
    scale_y_continuous(limits = c(270, 590), breaks = seq(300, 550, 50),
                       labels = comma) +
    labs(
      title    = lb$p2_title,
      subtitle = lb$p2_subtitle,
      x = NULL, y = lb$p2_ylab,
      caption  = lb$p2_caption
    ) +
    theme_tvyal26()
}

for (lang in c("en", "hy", "ru")) {
  p <- tvyal_logo(make_plot2(lang))
  save_plot_lang(p, "plot2_usd_amd.png", lang)
}
use_locale("arm")  # reset

# ── also copy to R dir plots/ ─────────────────────────────────────────────────
r_plots <- path.expand("~/R/newsletter/2026/2026_03_24_dollar/plots")
dir.create(r_plots, showWarnings = FALSE)
for (lang in c("en", "hy", "ru")) {
  for (f in c("plot1_nir_nda.png", "plot2_usd_amd.png")) {
    src  <- file.path(dirs[[lang]], f)
    dest <- file.path(r_plots, paste0(lang, "_", f))
    if (file.exists(src)) file.copy(src, dest, overwrite = TRUE)
  }
}

message("Done. 6 plots saved + copied to R dir.")
