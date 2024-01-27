library(tidyverse)
library(tidyverse)
library(scales)
library(toOrdinal)
library(Hmisc)
library(gt)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

household_22 <- read_csv("household_2022.csv")

caption_arm <-  "Հեղինակ` Աղասի Թավադյան   |   tvyal.com   |   tavadyan.com"
caption_eng <-  "Author: Aghasi Tavadyan   |   tvyal.com   |   tavadyan.com"


household_22 |> 
  ggplot(aes(totincome, members)) +
  geom_jitter(alpha = 0.2) +
  geom_density_2d() +
  geom_smooth(method = "lm") +
  scale_x_log10(breaks = 10^(4:6), label = number_format()) +
  scale_y_continuous(breaks = seq(0, 10, 2))


household_22 |> 
  arrange(members) |> 
  select(-hheld_over63_sum) |> 
  mutate(
    work_members = members - rowSums(across(matches("hh.*_sum")), na.rm = TRUE),
    members2 = ifelse(members >= 8, ">=8", members),
    members2 = fct_inorder(members2)
    ) |> 
  ggplot(aes(totincome/work_members, members2)) +
  geom_boxplot() +
  scale_x_log10(breaks = 10^(4:6), label = number_format())

##############

headmerstatus_dict <- tibble(
  headmerstatus = 1:5,
  headmerstatus_eng = c("married", "never married", "widowed",
                        "divorced/separated", "cohabiting")
)

household_22 |>
  arrange(members) |> 
  select(-hheld_over63_sum) |> 
  left_join(headmerstatus_dict) |> 
  mutate(
    work_members = members - rowSums(across(matches("hh.*_sum")), na.rm = TRUE),
    members2 = ifelse(members >= 8, ">=8", members),
    members2 = fct_inorder(members2),
    headmerstatus = as.factor(headmerstatus)
  ) |> 
  ggplot(aes(totincome/work_members, headmerstatus_eng)) +
  geom_boxplot() +
  scale_x_log10(breaks = 10^(4:6), label = number_format())


household_22 |>
  left_join(headmerstatus_dict) |> 
  mutate(
    work_members = members - rowSums(across(matches("hh.*_sum")), na.rm = TRUE),
    income_per_work_member = totincome/work_members,
    income_per_work_member = ifelse(is.infinite(income_per_work_member), NA, income_per_work_member)
  ) |> 
  group_by(headmerstatus_eng) |> 
  summarise(
    mean = mean(totincome),
    median = median(totincome),
    mean_pc = mean(income_per_work_member, na.rm = TRUE),
    median_pc = median(income_per_work_member, na.rm = TRUE)
  )

##############

household_22_income_cuts <-
  household_22 |>
  reframe(
    totincome = wtd.quantile(totincome, weight, probs = seq(0, 1, 0.1))
    # totincome = quantile(totincome, probs = seq(0, 1, 0.1))
  ) |>
  pull(totincome)

decile_description <-
  household_22 |> 
  reframe(deciles = quantile(totincome, probs = seq(0.1, 0.9, 0.1))) %>%
  bind_rows(tibble(deciles = NA), .) |>
  mutate(
    deciles2 = lead(deciles),
    x = row_number(),
    description_arm = case_when(
      x == 1 ~ paste0("1-ին դեցիլ` ", round(deciles2/1000), " հազար դրամից պակաս"),
      x == 10 ~ paste0("10-րդ դեցիլ` ", round(deciles/1000), " հազար դրամից ավել"),
      TRUE ~ paste0(x, "-րդ դեցիլ` ", round(deciles/1000), "-ից մինչև ",
                    round(deciles2/1000)," հազար դրամ"),
    ),
    description_eng = case_when(
      x == 1 ~ paste0("1st decile - less than ", round(deciles2/1000), " thousand drams"),
      x == 10 ~ paste0("10th decile - more than ", round(deciles/1000), " thousand drams"),
      TRUE ~ paste0(toOrdinal(x), " decile - from ", round(deciles/1000), " to ",
                    round(deciles2/1000)," thousand drams"),
    ),
    description_arm = fct_inorder(description_arm),
    description_eng = fct_inorder(description_eng),
    y = 0
  ) |>
  select(description_arm, description_eng, x, y)

household_22 |> 
  # filter(headmerstatus != 5) |> 
  left_join(headmerstatus_dict) |> 
  mutate(
    deciles = cut(
      totincome,
      breaks = household_22_income_cuts,
      labels = 1:10,
      include.lowest = TRUE
    )
  ) |> 
  count(headmerstatus_eng, deciles) |> 
  group_by(deciles) |> 
  mutate(pct = n / sum(n)) |> 
  filter(headmerstatus_eng != "cohabiting") |> 
  ungroup() |> 
  select(-n) |> 
  pivot_wider(names_from = headmerstatus_eng, values_from = pct) |> 
  left_join(decile_description |> mutate(x = as.factor(x)), by = c("deciles" = "x")) |> 
  mutate(deciles = description_arm) |> 
  select(-description_arm, -description_eng, -y) |>
  relocate(deciles, married, `never married`, widowed) |> 
  gt() |> 
  data_color(
    columns = -c(deciles),
    colors = scales::col_numeric(
      palette = c("#f95d6a", "#fffcf5", "#2f4b7c"),
      domain = NULL
    )
  ) |> 
  fmt_number(
    columns = -c(deciles),
    scale_by = 100,
    decimals = 1,
    pattern = "{x}%"
  ) |> 
  cols_label(
    deciles = "Տնային տնտեսության դեցիլ",
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
  


#################
  
  household_22 |> 
    select(-hheld_over63_sum) |> 
    mutate(
      work_members = members - rowSums(across(matches("hh.*_sum")), na.rm = TRUE),
      income_per_working_member = totincome/work_members,
      income_per_working_member = ifelse(is.infinite(income_per_working_member), NA, income_per_working_member)
    ) |> 
    summarise(
      mean = mean(income_per_working_member, na.rm = TRUE)*12/400,
      median = median(income_per_working_member, na.rm = TRUE)*12/400
    )
