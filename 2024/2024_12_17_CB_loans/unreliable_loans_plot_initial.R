library(ggtext)

# Create a more visually striking version
ggplot_loans <- tibble(
  date = ymd("2024-10-01", "2023-04-01"),
  unreliable_loans = c(642, 525),
  date_name = c("2024թ․ հոկտեմբեր", "2023թ․ ապրիլ"),
  percentages = c("68.4%*", "40.0%*")
) |> 
  ggplot(aes(date_name, unreliable_loans)) +
  # Add background shading for depth
  geom_col(aes(fill = date_name), width = 0.7, alpha = 0.95) +
  
  # Improve value labels
  geom_text(
    aes(y = unreliable_loans/2, 
        label = paste0(unreliable_loans, "\nմլրդ դրամ")),
    color = "white", 
    size = 5,
    fontface = "bold",
    family = "Arial Unicode MS"
  ) +
  
  # Add percentage indicators with improved positioning
  geom_text(
    aes(y = unreliable_loans * 0.75, label = percentages),
    color = "white", 
    size = 8,
    fontface = "bold",
    family = "Arial Unicode MS"
  ) +
  
  # Add explanatory text with better formatting
  geom_textbox(
    data = tibble(
      x = "2023թ․ ապրիլ", 
      y = 700,
      label = "1.5 տարվա ընթացքում անհուսալի\nվարկ ունեցողների վարկային բեռը\nաճել է 22.2 %"
    ),
    aes(x = x, y = y, label = label),
    width = unit(0.6, "npc"),
    halign = 0.5,
    fill = "white",
    box.color = NA,
    family = "Arial Unicode MS",
    size = 4.5
  ) +
  
  # Improve color scheme
  scale_fill_manual(values = c("#FF6B6B", "#4A90E2")) +
  
  # Enhanced theme
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Unicode MS"),
    plot.title = element_text(
      size = 16, 
      face = "bold", 
      lineheight = 1.2,
      margin = margin(b = 20)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      size = 12,
      face = "bold",
      margin = margin(t = 10)
    ),
    plot.margin = margin(t = 20, r = 30, b = 20, l = 30)
  ) +
  
  # Improved labels
  labs(
    x = NULL, 
    y = NULL,
    title = "Հայաստանի աշխատավոր բնակչության 15 տոկոսն\nունի անհուսալի վարկ (341 հազար մարդ)",
    subtitle = "Անհուսալի վարկատուների ընդհանուր վարկային բեռը",
    caption = caption_f(
      "CBA, tvyal.com", 
      suffix_text = "* Սպառողական վարկերի մեջ անհուսալի վարկերի տեսակարար կշիռ"
    )
  )