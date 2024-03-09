data_37 <- read_csv("~/R/projects/t/data_37.csv")

headmerstatus_dict <- tibble(
  headmerstatus = 1:5,
  headmerstatus_eng = c("married", "never married", "widowed",
                        "divorced/separated", "cohabiting")
)

library(gt)

caption_arm = ""

data_37 |> 
  filter(headmerstatus != 5) |> 
  group_by(year, headmerstatus) |> 
  summarise(headage = mean(headage, na.rm = TRUE)) |> 
  left_join(headmerstatus_dict) |> 
  ggplot(aes(year, headage, color = headmerstatus_eng)) +
  geom_line()

data_37 |> 
  group_by(year) |> 
  count(headmerstatus) |> 
  mutate(pct = n / sum(n)) |> 
  ungroup() |> 
  left_join(headmerstatus_dict) |> 
  filter(headmerstatus != 5) |> 
  select(-headmerstatus, -n) |> 
  pivot_wider(names_from = headmerstatus_eng, values_from = pct) |> 
  gt() |> 
  data_color(
    columns = -c(year),
    colors = scales::col_numeric(
      palette = c("#f95d6a", "#fffcf5", "#2f4b7c"),
      domain = NULL
    )
  ) |> 
  fmt_number(
    columns = -c(year),
    scale_by = 100,
    decimals = 1,
    pattern = "{x}%"
  ) |> 
  cols_label(
    year = "տարի",
    married = "Ամուսնացած",
    `never married` = "Երբեք չամուսնացած", 
    widowed = "Այրի",
    `divorced/separated` = "Ամուսնալուծված"
  ) |> 
  tab_header(
    title = "Ամուր ընտանիք՝ հարուստ ընտանիք",
    subtitle = "Հայաստանի ընտանիքների հարստությունը ըստ ընտանիքի գլխի ամուսնական կարգավիճակի, 2022թ"
  ) |> 
  tab_footnote(
    footnote = paste0(caption_arm, "    |    Տվյալների աղբյուր` armstat.am")
  )
