library(ragg)       # must be first — use ragg device for font rendering (avoids showtext segfault)
library(tidyverse)

# ── fonts ─────────────────────────────────────────────────────────────────────
sysfonts::font_add(
  "GHEA Mariam",
  regular    = "/usr/share/fonts/all_armenian_fonts/Mariam/GHEAMariamReg.otf",
  bold       = "/usr/share/fonts/all_armenian_fonts/Mariam/GHEAMariamBld.otf",
  italic     = "/usr/share/fonts/all_armenian_fonts/Mariam/GHEAMariamRIt.otf",
  bolditalic = "/usr/share/fonts/all_armenian_fonts/Mariam/GHEAMariamBlit.otf"
)

# ── base theme ────────────────────────────────────────────────────────────────
theme_tvyal <- function(base_size = 12, base_family = "GHEA Mariam") {
  colors <- deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
  ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    theme(
      line = element_line(colour = "black"),
      rect = element_rect(fill = "white", linetype = 0, colour = NA),
      text = element_text(family = "GHEA Mariam", colour = colors["Dark Gray"]),
      axis.text    = element_text(family = "GHEA Mariam"),
      axis.title   = element_text(family = "GHEA Mariam"),
      legend.text  = element_text(family = "GHEA Mariam"),
      plot.title   = element_text(
        family = "GHEA Mariam", hjust = 0, size = rel(1.5), face = "bold"
      ),
      axis.ticks  = element_blank(),
      axis.line   = element_blank(),
      legend.background = element_rect(),
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.box        = "vertical",
      panel.background  = element_rect(fill = NA, colour = NA),
      panel.grid        = element_line(colour = NULL),
      panel.grid.major.x = element_line(
        colour = colors["Medium Gray"], linetype = "dotted"
      ),
      panel.grid.major.y = element_line(
        colour = colors["Medium Gray"], linetype = "dotted"
      ),
      panel.grid.minor = element_blank(),
      plot.margin      = unit(c(1, 1, 1, 1), "lines"),
      strip.background = element_rect(fill = "#FCFCFC", colour = NA),
      strip.text       = element_text(
        family = "GHEA Mariam", colour = colors["Dark Gray"], face = "bold"
      )
    )
}

# ── 2026 theme — NO element_path here; logo added via tvyal_logo() ────────────
theme_tvyal26 <- function(base_size = 12, base_family = "GHEA Mariam") {
  theme_tvyal(base_size = base_size, base_family = base_family) +
    theme(
      legend.position = "bottom",
      plot.tag.position = c(0.07, 0.97),
      plot.title = element_text(
        family = "GHEA Mariam", hjust = 0.5,
        size = rel(1.3), face = "bold",
        margin = margin(l = 80, b = 8)
      ),
      plot.subtitle = element_text(
        family = "GHEA Mariam", hjust = 0.5,
        margin = margin(l = 80, b = 5)
      ),
      plot.caption = element_text(
        family = "GHEA Mariam", hjust = 0.5
      )
    )
}

# ── logo — adds image tag AND element_path together so theme alone never crashes
library(ggpath)

tvyal_logo <- function(plot) {
  logo_path <- path.expand("~/docs/partnership/website_v3/logo/static_bold.png")
  plot +
    labs(tag = logo_path) +
    theme(plot.tag = element_path(size = 2))
}

theme_set(theme_tvyal26())

# ── palette ───────────────────────────────────────────────────────────────────
new_palette_colors <- c(
  "#003f5c", "#2f4b7c", "#665191", "#a05195",
  "#d45087", "#f95d6a", "#ff7c43", "#ffa600"
)
colfunc  <- colorRampPalette(c("#2f4b7c", "#fffcf5", "#f95d6a"))
colfunc2 <- colorRampPalette(new_palette_colors)
colfunc3 <- colorRampPalette(c("#005C4B", new_palette_colors, "#FFD17A", "#FFFCF5"))

update_geom_defaults("rect", list(fill  = new_palette_colors[2], alpha = 1))
update_geom_defaults("line", list(color = new_palette_colors[2], alpha = 1))
update_geom_defaults("area", list(fill  = new_palette_colors[2], alpha = 1))


# ── Force text/label geoms to use the custom font ─────────────────────────────
update_geom_defaults("text", list(family = "GHEA Mariam"))
update_geom_defaults("label", list(family = "GHEA Mariam"))

# If using ggrepel, load it so geoms are registered before setting defaults
if (require("ggrepel", quietly = TRUE)) {
  update_geom_defaults("text_repel",  list(family = "GHEA Mariam"))
  update_geom_defaults("label_repel", list(family = "GHEA Mariam"))
}


# ── captions ──────────────────────────────────────────────────────────────────
caption_arm <- "tvyal.com"
caption_eng <- "tvyal.com"
caption_rus <- "tvyal.com"

caption_f <- function(source = NULL, language = "arm", suffix_text = NULL) {
  base <- switch(language,
    arm = caption_arm,
    eng = caption_eng,
    rus = caption_rus,
    caption_eng   # fallback
  )
  source_label <- switch(language,
    arm = "Տվյալների աղբյուր՝ ",
    eng = "Data source: ",
    rus = "Источник данных: ",
    "Data source: "
  )
  caption <- if (is.null(source)) base else paste0(base, "  \u00b7  ", source_label, source)
  if (!is.null(suffix_text)) caption <- paste0(suffix_text, "\n\n", caption)
  caption
}

# ── multilingual helpers ──────────────────────────────────────────────────────

# Set LC_TIME locale for date formatting
use_locale <- function(language = "arm") {
  locale <- switch(language,
    arm = "hy_AM.UTF-8",
    eng = "en_US.UTF-8",
    rus = "ru_RU.UTF-8"
  )
  Sys.setlocale("LC_TIME", locale)
  invisible(language)
}

# Abbreviated month names for each language
month_names_short <- list(
  arm = c("Հնվ", "Փտր", "Մրտ", "Ապր", "Մյս", "Հնս",
          "Հլс", "Օգս", "Սպտ", "Հկտ", "Նյբ", "Դկտ"),
  eng = month.abb,
  rus = c("Янв", "Фев", "Мар", "Апр", "Май", "Июн",
          "Июл", "Авг", "Сен", "Окт", "Ноя", "Дек")
)

# Full month names for each language
month_names_full <- list(
  arm = c("Հունվար", "Փետրվար", "Մարտ", "Ապրիլ", "Մայիս", "Հունիս",
          "Հուլիս", "Օգոստոս", "Սեպտեմբեր", "Հոկտեմբեր", "Նոյեմբեր", "Դեկտեմբեր"),
  eng = month.name,
  rus = c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь",
          "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь")
)

# ── save helper ───────────────────────────────────────────────────────────────
save_last_plot <- function(filename, extension = "png", width = 1152, height = 648, dpi = 192) {
  filename <- str_replace_all(filename, ", ", "_")
  outfile  <- paste0(filename, ".", extension)
  if (extension == "png") {
    ragg::agg_png(outfile, width = width, height = height, res = dpi)
  } else if (extension == "pdf") {
    pdf(outfile, width = width / dpi, height = height / dpi)
  } else {
    stop("Unsupported extension. Use 'png' or 'pdf'.")
  }
  last_plot <- recordPlot()
  replayPlot(last_plot)
  dev.off()
  outfile
}

# ── parallel ──────────────────────────────────────────────────────────────────
doParallel::registerDoParallel(cores = 8)

# Default to Armenian locale (same as before)
Sys.setlocale("LC_TIME", "hy_AM.UTF-8")
