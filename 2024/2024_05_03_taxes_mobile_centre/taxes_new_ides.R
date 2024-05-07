
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

system("Rscript ../../initial_setup.R")

taxes_data <- read_csv("hhpektt_2021_2024_I.csv")

taxes_data_change <- 
  taxes_data |> 
  filter(year %in% c(2022:2023), Q == 4) |> 
  mutate(
    year = ifelse(year == max(year), "end", "start"),
    name = str_to_title(name),
    name = str_trunc(name, width = 30),
    total_taxes = total_taxes / 1e3
  ) |> 
  select(-Q, -date, -total_taxes_Q, -n_Q, -location) |> 
  pivot_wider(
    names_from = "year", values_from = c(total_taxes, n, name),
    # names_vary = "slowest"
  ) |> 
  select(-name_start) |> 
  mutate(
    change = total_taxes_end / total_taxes_start,
    change_direction = ifelse(change > 1, "possitive", "negative"),
    change_abaslute = ifelse(change > 1, change, 1 / change)
  ) |> 
  arrange(desc(change)) |> 
  filter(!is.na(change))


taxes_data_change |> 
  slice_head(n = 20) |>
  mutate(
    color = ifelse(total_taxes_end < total_taxes_start, "#f95d6a", "#2f4b7c"),
    name_end = fct_reorder(name_end, n_end, .desc = TRUE)
  ) |> 
  ggplot(aes(y= name_end)) +
  geom_segment(
    aes(x = total_taxes_start, xend= total_taxes_end,
        yend=name_end, group = name_end, color = I(color)),
    linewidth = 1.2,
    lineend = 'round', linejoin = 'round',
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_point(aes(x = total_taxes_start, color=I("#f95d6a")), size = 3) +
  geom_point(aes(x = total_taxes_end, color=I("#2f4b7c")), size = 3) +
  geom_text(aes(x = total_taxes_start * ifelse(change > 1, 0.8, 1.2), label = n_start, color=I("#f95d6a"))) +
  geom_text(aes(x = total_taxes_end * ifelse(change > 1, 1.2, 0.8), label = n_end, color=I("#2f4b7c"))) +
  geom_text(aes(
    x = sqrt(total_taxes_start * total_taxes_end),
    label = number(change_abaslute, accuracy = 0.1)
  ), vjust = -0.35) +
  scale_x_log10(label = number_format(), n.breaks = 6, expand = c(0.07,0,0.07,0)) +
  scale_y_discrete(expand = c(0.05,0,0.07,0)) +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Ո՞ր կազմակերպություններն են ամենաշատը աճել 2024 թվականի I եռամսյակում",
    subtitle = "Հազար խոշոր հարկատուների մեջ ամենամեծ դիրքային փոփոխությունները\n2024-ի առաջին եռամսյակը համեմատած 2023 -ի եռամսյակի հետ, միլիոն ՀՀ դրամ",
    caption = paste0(caption_arm, "    |    տվյալների աղբյուր` armstat.am")
  )


taxes_data_change |> 
  slice_tail(n = 20) |>
  mutate(
    color = ifelse(total_taxes_end < total_taxes_start, "#f95d6a", "#2f4b7c"),
    name_end = fct_reorder(name_end, n_start, .desc = TRUE)
  ) |> 
  ggplot(aes(y= name_end)) +
  geom_segment(
    aes(x = total_taxes_start, xend= total_taxes_end,
        yend=name_end, group = name_end, color = I(color)),
    linewidth = 1.2,
    lineend = 'round', linejoin = 'round',
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_point(aes(x = total_taxes_start, color=I("#f95d6a")), size = 3) +
  geom_point(aes(x = total_taxes_end, color=I("#2f4b7c")), size = 3) +
  geom_text(aes(x = total_taxes_start * ifelse(change > 1, 0.85, 1.15), label = n_start, color=I("#f95d6a"))) +
  geom_text(aes(x = total_taxes_end * ifelse(change > 1, 1.15, 0.85), label = n_end, color=I("#2f4b7c"))) +
  geom_text(aes(
    x = sqrt(total_taxes_start * total_taxes_end),
    label = number(change_abaslute, accuracy = 0.1)
  ), vjust = -0.35) +
  scale_x_log10(label = number_format(), n.breaks = 6, expand = c(0.07,0,0.07,0)) +
  scale_y_discrete(expand = c(0.05,0,0.07,0)) +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Ո՞ր կազմակերպություններն են ամենաշատը կրճատվել 2024 թվականի I եռամսյակում",
    subtitle = "Հազար խոշոր հարկատուների մեջ ամենամեծ դիրքային փոփոխությունները\n2024-ի առաջին եռամսյակը համեմատած 2023 -ի եռամսյակի հետ, միլիոն ՀՀ դրամ",
    caption = paste0(caption_arm, "    |    տվյալների աղբյուր` armstat.am")
  )



  
