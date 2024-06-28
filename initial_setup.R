library(tidyverse)

theme_tvyal <- function(base_size = 12, base_family = "sans")
{
  colors <- deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
  (ggthemes::theme_foundation(
    base_size = base_size, base_family = base_family) +
      theme(
        line = element_line(colour = "black"),
        rect = element_rect(fill = "white", linetype = 0, colour = NA),
        text = element_text(colour = colors["Dark Gray"]),
        # axis.title = element_blank(),
        # axis.text = element_text(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.background = element_rect(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        panel.grid = element_line(colour = NULL),
        panel.grid.major.x = element_line(
          colour = colors["Medium Gray"], 
          linetype = "dotted"
        ),
        panel.grid.major.y = element_line(
          colour = colors["Medium Gray"], 
          linetype = "dotted"
        ),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
        plot.margin = unit(c(1, 1, 1, 1), "lines"), strip.background = element_rect()
      )
  )
}

theme_set(theme_tvyal())

save_last_plot <-
  function(filename, extension = "png", width = 1152, height = 648) {
    
    filename <- str_replace_all(filename, ", ", "_")
    last_plot <- recordPlot()
    
    if (extension == "png") {
      png(paste0(filename, ".", extension), width = width, height = height)
    } else if (extension == "pdf") {
      pdf(paste0(filename, ".", extension), width = width, height = height)
    } else {
      stop("Unsupported file extension. Use 'png' or 'pdf'.")
    }
    
    replayPlot(last_plot)
    dev.off()
    paste0(filename, ".", extension)
  }

new_palette_colors <- c(
  "#003f5c", "#2f4b7c", "#665191", "#a05195",
  "#d45087", "#f95d6a", "#ff7c43", "#ffa600"
)
colfunc <- colorRampPalette(c("#2f4b7c", "#fffcf5", "#f95d6a"))
colfunc2 <- colorRampPalette(new_palette_colors)
colfunc3 <- colorRampPalette(c("#005C4B", new_palette_colors, "#FFD17A", "#FFFCF5"))

update_geom_defaults("rect", list(fill  = new_palette_colors[2], alpha = 0.8))
update_geom_defaults("line", list(color = new_palette_colors[2], alpha = 0.8))
update_geom_defaults("area", list(fill  = new_palette_colors[2], alpha = 0.8))

caption_arm <-  "Հեղինակ` Աղասի Թավադյան   |   tvyal.com   |   tavadyan.com"
caption_eng <-  "Author: Aghasi Tavadyan   |   tvyal.com   |   tavadyan.com"

caption_f <- function(source = NULL, language = "arm", suffix_text = NULL){
  caption <- case_when(
    language == "arm" & is.null(source) ~ caption_arm, 
    language == "eng" & is.null(source) ~ caption_eng, 
    language == "arm" & !is.null(source) ~ paste0(caption_arm, "    |    Տվյալների աղբյուր՝ ", source), 
    language == "eng" & !is.null(source) ~ paste0(caption_eng, "    |    Data Source: ", source), 
  )
  
  if (!is.null(suffix_text)) {
    caption = paste0(suffix_text, "\n\n", caption)
  }
  
  return(caption)
}

doParallel::registerDoParallel(cores = 8)

Sys.setlocale("LC_TIME", "hy_AM.UTF-8")