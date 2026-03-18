# ── tvyal newsletter 2026-03-18: the ruble paradox ───────────────────────────
# 3 plots × 3 languages = 9 files

library(ragg)
library(tidyverse)
library(tidyquant)
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
    c("en", "hy", "ru"), "2026/2026-03-18-exchange/plots"
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
navy   <- new_palette_colors[2]
orange <- new_palette_colors[7]
green  <- "#005C4B"
red    <- new_palette_colors[6]

# ─────────────────────────────────────────────────────────────────────────────
# DATA PULL — live from Yahoo Finance (once)
# ─────────────────────────────────────────────────────────────────────────────
message("Pulling data from Yahoo Finance...")

safe_pull <- function(ticker, col, from = "2022-01-01") {
  tq_get(ticker, from = from, get = "stock.prices") |>
    select(date, close) |>
    arrange(date) |>
    fill(close, .direction = "down") |>
    filter(!is.na(close)) |>
    rename(!!col := close)
}

usdrub_daily <- safe_pull("USDRUB=X", "usdrub")
brent_daily  <- safe_pull("BZ=F",     "brent")

to_weekly <- function(df, col) {
  df |>
    mutate(week = floor_date(date, "week", week_start = 1)) |>
    group_by(week) |>
    summarise(!!col := last(.data[[col]], na_rm = TRUE), .groups = "drop")
}

usdrub_weekly <- to_weekly(usdrub_daily, "usdrub")
brent_weekly  <- to_weekly(brent_daily,  "brent")
weekly        <- inner_join(usdrub_weekly, brent_weekly, by = "week") |>
  filter(!is.na(usdrub), !is.na(brent))

# plot 1: last 8 weeks from anchor
anchor_date <- as.Date("2026-01-19")
weeks_8_base <- weekly |>
  filter(week >= anchor_date) |>
  slice_head(n = 8) |>
  mutate(
    usdrub_idx = usdrub / first(usdrub) * 100,
    brent_idx  = brent  / first(brent)  * 100
  )
anchor_week <- min(weeks_8_base$week)

# plot 3: seasonal avg 2022-2025 + 2026 actuals (computed from monthly closes)
usdrub_monthly <- usdrub_daily |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month) |>
  summarise(close = last(usdrub), .groups = "drop") |>
  mutate(ret = (close / lag(close) - 1) * 100, year = year(month), month_n = month(month)) |>
  filter(!is.na(ret))

seasonal_avg <- usdrub_monthly |>
  filter(year >= 2022, year <= 2025) |>
  group_by(month_n) |>
  summarise(avg = round(mean(ret), 2), .groups = "drop")

actual_2026 <- usdrub_monthly |>
  filter(year == 2026) |>
  select(month_n, actual = ret)

seasonal_base <- tibble(month_n = 1:12) |>
  left_join(seasonal_avg, by = "month_n") |>
  left_join(actual_2026,  by = "month_n")

current <- usdrub_weekly |> filter(week == max(week))
band <- tibble(xmin = current$week, xmax = current$week + 100, ymin = 78, ymax = 87)

# ─────────────────────────────────────────────────────────────────────────────
# LABELS — one list per language
# ─────────────────────────────────────────────────────────────────────────────
labels <- list(

  eng = list(
    locale       = "eng",
    dir_key      = "en",
    caption_lang = "eng",
    # plot 1
    brent_label  = "Brent crude",
    usdrub_label = "USD/RUB (ruble weakening \u2192)",
    p1_title     = "Oil Surged 46%. The Ruble Weakened Anyway.",
    p1_sub       = "Indexed to 100 on {anchor_label}",
    p1_y         = "Index (anchor week = 100)",
    # plot 2
    spike1       = "Mar 2022\n135 \u2014 panic\nrecovered in weeks",
    spike2       = "Oct 2023 \u2014 101\nrecovered to 87",
    spike3       = "Nov 2024 \u2014 108\n(Gazprombank sanctions)\nrecovered to 75",
    expected     = "Expected\n78\u201387",
    now_prefix   = "Now: ",
    p2_title     = "The Ruble: Four Years of Spikes \u2014 and Four Recoveries",
    p2_sub       = "USD/RUB weekly. Higher = weaker ruble. Green band = expected range, next 3 months.",
    p2_y         = "USDRUB",
    # plot 3
    leg_str      = "Hist. avg \u2014 strengthens",
    leg_wk       = "Hist. avg \u2014 weakens",
    leg_act      = "Actual 2026",
    p3_title     = "March is Historically the Ruble’s Strongest Month",
    p3_sub       = "Avg monthly USD/RUB % change, 2022\u20132025. Negative = ruble strengthens.\nNarrow orange bars: actual 2026.",
    p3_y         = "Avg monthly USDRUB change (%)"
  ),

  arm = list(
    locale       = "arm",
    dir_key      = "hy",
    caption_lang = "arm",
    # plot 1
    brent_label  = "Brent նավթ",
    usdrub_label = "USD/RUB (արժեզրկում \u2192)",
    p1_title     = "Նավթի 46% աճ և ռուբլու արժեզրկում",
    p1_sub       = "Բազիսային արժեք՝ 100 ({anchor_label})",
    p1_y         = "Ինդեքս (բազիսային շաբաթ = 100)",
    # plot 2
    spike1       = "Մարտ 2022թ․\n135 \u2014 ճգնաժամ\nկայունացում շաբաթների ընթացքում",
    spike2       = "Հոկտ. 2023թ. \u2014 101\nկայունացում 87-ի մակարդակում",
    spike3       = "Նոյ. 2024թ. \u2014 108\n(«Գազպրոմբանկ»)\nկայունացում 75-ում",
    expected     = "Կանխատեսում\n78\u201387",
    now_prefix   = "Այս պահին՝ ",
    p2_title     = "Ռուբլու դինամիկան. արժեզրկման և կայունացման 4-ամյա ցիկլեր",
    p2_sub       = "USD/RUB (շաբաթական)։ Աճը՝ արժեզրկում։ Կանաչ գոտին առաջիկա 3 ամսվա կանխատեսումն է։",
    p2_y         = "USD/RUB",
    # plot 3
    leg_str      = "Պատմ. միջին \u2014 արժևորում",
    leg_wk       = "Պատմ. միջին \u2014 արժեզրկում",
    leg_act      = "Փաստացի 2026թ.",
    p3_title     = "Մարտը ռուբլու արժևորման պատմականորեն ամենաակտիվ ամիսն է",
    p3_sub       = "USD/RUB միջ. ամսական փոփոխությունը (%), 2022\u20132025թթ. (անկումը՝ արժևորում)։\nՆարնջագույնը՝ 2026թ. փաստացի տվյալներն են։",
    p3_y         = "USD/RUB միջին ամսական փոփոխությունը (%)"
  ),

  rus = list(
    locale       = "rus",
    dir_key      = "ru",
    caption_lang = "rus",
    # plot 1
    brent_label  = "Нефть Brent",
    usdrub_label = "USD/RUB (ослабление \u2192)",
    p1_title     = "Рост цен на нефть на 46% и ослабление рубля",
    p1_sub       = "Базовое значение: 100 ({anchor_label})",
    p1_y         = "Индекс (базовая неделя = 100)",
    # plot 2
    spike1       = "Март 2022 г.\n135 \u2014 кризис\nбыстрая стабилизация",
    spike2       = "Окт. 2023 г. \u2014 101\nстабилизация на 87",
    spike3       = "Нояб. 2024 г. \u2014 108\n(Газпромбанк)\nстабилизация на 75",
    expected     = "Прогноз\n78\u201387",
    now_prefix   = "Текущее: ",
    p2_title     = "Динамика рубля: 4-летний цикл колебаний и стабилизации",
    p2_sub       = "USD/RUB (еженедельно). Рост = ослабление. Зеленая зона \u2014 прогноз на 3 месяца.",
    p2_y         = "USD/RUB",
    # plot 3
    leg_str      = "Ист. среднее \u2014 укрепление",
    leg_wk       = "Ист. среднее \u2014 ослабление",
    leg_act      = "Факт 2026 г.",
    p3_title     = "Март \u2014 исторически месяц максимального укрепления рубля",
    p3_sub       = "Среднемес. изменение USD/RUB (%), 2022\u20132025 гг. (снижение = укрепление).\nОранжевым \u2014 фактические данные 2026 г.",
    p3_y         = "Среднемес. изменение USD/RUB (%)"
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# PLOT FUNCTIONS
# ─────────────────────────────────────────────────────────────────────────────
make_p1 <- function(lbl) {
  use_locale(lbl$locale)
  anchor_label <- format(anchor_week, "%b %d, %Y")

  w8 <- weeks_8_base |>
    pivot_longer(cols = c(usdrub_idx, brent_idx), names_to = "series", values_to = "index") |>
    mutate(series_label = case_when(
      series == "brent_idx"  ~ lbl$brent_label,
      series == "usdrub_idx" ~ lbl$usdrub_label
    ))

  end_8 <- w8 |>
    filter(week == max(week)) |>
    mutate(end_label = paste0(series_label, "  ", ifelse(index > 100, "+", ""), round(index - 100, 1), "%"))

  ggplot(w8, aes(x = week, y = index, colour = series)) +
    geom_hline(yintercept = 100, colour = "gray60", linewidth = 0.4, linetype = "dashed") +
    geom_line(linewidth = 1.8) +
    geom_point(data = end_8, size = 4) +
    geom_label_repel(
      data = end_8, aes(label = end_label),
      size = 3.8, fontface = "bold",
      label.size = 0.2, label.padding = unit(0.3, "lines"),
      nudge_x = 2, nudge_y = c(2, -2),
      direction = "y", hjust = 0, show.legend = FALSE
    ) +
    annotate("text",
      x = anchor_week + 1, y = 101.5,
      label = paste0("100 = ", anchor_label), hjust = 0, size = 3.2, colour = "gray50"
    ) +
    scale_colour_manual(values = c(brent_idx = orange, usdrub_idx = navy)) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d",
                 expand = expansion(mult = c(0.02, 0.28))) +
    scale_y_continuous(labels = function(x) paste0(x), breaks = seq(90, 160, 10)) +
    labs(
      title   = lbl$p1_title,
      subtitle = sub("\\{anchor_label\\}", anchor_label, lbl$p1_sub),
      x = NULL, y = lbl$p1_y,
      caption = caption_f(source = "Yahoo Finance", language = lbl$caption_lang)
    ) +
    theme_tvyal26() +
    theme(legend.position = "none")
}

make_p2 <- function(lbl) {
  use_locale(lbl$locale)
  spikes <- tribble(
    ~week,                    ~usdrub, ~label,
    as.Date("2022-03-07"),     135,   lbl$spike1,
    as.Date("2023-10-02"),     101,   lbl$spike2,
    as.Date("2024-11-25"),     108,   lbl$spike3
  )
  ggplot(usdrub_weekly, aes(x = week, y = usdrub)) +
    geom_rect(data = band, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = green, alpha = 0.15) +
    annotate("text", x = current$week + 50, y = 82.5,
             label = lbl$expected, size = 3.2, colour = green, hjust = 0.5) +
    geom_line(colour = navy, linewidth = 1.3) +
    geom_point(data = spikes, aes(x = week, y = usdrub),
               colour = red, size = 3.5, inherit.aes = FALSE) +
    geom_label_repel(
      data = spikes, aes(x = week, y = usdrub, label = label),
      inherit.aes = FALSE,
      size = 3, colour = "gray25", lineheight = 0.85,
      label.size = 0.2, label.padding = unit(0.3, "lines"),
      nudge_y = 12, direction = "x"
    ) +
    geom_point(data = current, colour = red, size = 4) +
    geom_label(data = current, aes(label = paste0(lbl$now_prefix, round(usdrub, 1))),
               nudge_y = -7, size = 3.5, colour = red, label.size = 0.2, fill = "white") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b '%y",
                 expand = expansion(mult = c(0.01, 0.1))) +
    scale_y_continuous(breaks = seq(60, 140, 20), labels = label_number(accuracy = 1)) +
    labs(
      title    = lbl$p2_title,
      subtitle = lbl$p2_sub,
      x = NULL, y = lbl$p2_y,
      caption  = caption_f(source = "Yahoo Finance", language = lbl$caption_lang)
    ) +
    theme_tvyal26()
}

make_p3 <- function(lbl) {
  use_locale(lbl$locale)
  seasonal <- seasonal_base |>
    mutate(month = factor(month.abb[month_n], levels = month.abb))

  seasonal_long <- seasonal |>
    pivot_longer(cols = c(avg, actual), names_to = "type", values_to = "value") |>
    mutate(
      fill_cat = case_when(
        type == "actual" ~ lbl$leg_act,
        value < 0        ~ lbl$leg_str,
        TRUE             ~ lbl$leg_wk
      ),
      bar_width = ifelse(type == "avg", 0.65, 0.22)
    )

  avg_labels <- seasonal |>
    mutate(
      label_y = ifelse(avg < 0, avg - 0.3, avg + 0.3),
      vjust   = ifelse(avg < 0, 1, 0),
      label   = paste0(ifelse(avg > 0, "+", ""), avg, "%")
    )

  ggplot(seasonal_long, aes(x = month, y = value, fill = fill_cat, width = bar_width)) +
    geom_col(position = "identity", na.rm = TRUE, alpha = 0.88) +
    geom_hline(yintercept = 0, colour = "gray40", linewidth = 0.4) +
    geom_text(
      data = avg_labels, aes(x = month, y = label_y, label = label, vjust = vjust),
      inherit.aes = FALSE, size = 3.2, colour = "gray20", fontface = "bold"
    ) +
    scale_fill_manual(
      values = setNames(c(green, navy, orange), c(lbl$leg_str, lbl$leg_wk, lbl$leg_act)),
      name = NULL
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-9, 9)) +
    labs(
      title    = lbl$p3_title,
      subtitle = lbl$p3_sub,
      x = NULL, y = lbl$p3_y,
      caption  = caption_f(source = "Yahoo Finance", language = lbl$caption_lang)
    ) +
    theme_tvyal26() +
    theme(legend.position = "bottom")
}

# ─────────────────────────────────────────────────────────────────────────────
# GENERATE ALL PLOTS
# ─────────────────────────────────────────────────────────────────────────────
for (lbl in labels) {
  dk <- lbl$dir_key
  save_plot_lang(tvyal_logo(make_p1(lbl)), "plot_01.png", dk)
  save_plot_lang(tvyal_logo(make_p2(lbl)), "plot_02.png", dk)
  save_plot_lang(tvyal_logo(make_p3(lbl)), "plot_03.png", dk)
}

message("\nDone. 9 plots saved (3 languages x 3 plots).")
