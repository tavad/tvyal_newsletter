
# system("curl --insecure 'https://www.cba.am/stat/stat_data_arm/9_fin_market_arm.xlsx' -o '9_fin_market_arm.xlsx'")


fin_market_temp <- read_excel("9_fin_market_arm.xlsx")


col_dic <- tibble(
  name = c("hh_dram", "amn_dolar", "hh_dram_2", "amn_dolar_2",
           "hh_dram_3", "amn_dolar_3", "hh_dram_4", "amn_dolar_4", 
           "hh_dram_5", "amn_dolar_5", "hh_dramov", "amn_dolar_6", 
           "hh_dram_6", "amn_dolar_7", "hh_dramov_2", "amn_dolar_8", 
           "hh_dram_7", "amn_dolar_9", "hh_dram_8", "amn_dolar_10",
           "petakan_karcazamket_partatomseri_ekamtaberut_yunə3",
           "petakan_mijinzamket_partatomseri_ekamtaberut_yunə3",
           "petakan_erkarazamket_partatomseri_ekamtaberut_yunə3"),
  type = rep(c("Deposits", "Loans", "Mortgage loans for individuals",
               "Consumer loans", "CB bonds"),
             times = c(8, 8, 2, 2, 3)),
  length = c(rep(c("less than 1 year", "more than 1 year"),
                 times = 2, each = 4),
             rep(NA, 4), "shortterm", "midterm", "longterm"),
  entity = c(rep(c("Juridical person", "Natural person"),
                 times = 4, each = 2),
             rep("Natural person", 4), rep(NA, 3)),
  currency = c(rep(c("AMD", "USD"), times = 10),
               rep(NA,3))
)

fin_markets <-
  fin_market_temp |> 
  head(-3) |> 
  rename(row1 = 1) |> 
  filter(!is.na(row1)) |> 
  t(
    
  ) |> as_tibble() |> 
  row_to_names(row_number = 1) |> 
  clean_names() |> 
  mutate_all(~ ifelse(.x == "-", NA, .x)) |> 
  select_if(~ !all(is.na(.))) |>  
  hablar::retype() |> 
  rename(year = 1) |> 
  fill(year, .direction = "down") |> 
  mutate(
    month = (row_number() - 2) %% 12 + 1,
    date = ym(paste(year, month))
  ) |>
  select(-month, -year) |> 
  filter(!is.na(hh_dram)) |> 
  pivot_longer(-date, values_to = "pct") |> 
  mutate(
    pct = pct/100,
    pct_text = percent(pct, accuracy = 0.1)
  ) |> 
  left_join(col_dic, by = "name") |> 
  select(-name) |> 
  relocate(pct, pct_text, .after = last_col())

rm(fin_market_temp)

###########################

fin_market_chart <- 
  fin_markets |> 
  filter(
    type %in% c("Loans"),
    # entity == "Natural person",
    entity == "Juridical person"
  ) |> 
  mutate(
    length = ifelse(length == "less than 1 year", "Մեկ տարուց պակաս", "Մեկ տարուց ավել")
  ) |> 
  ggplot(aes(date, pct, lty = currency, color = length)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(), n.breaks = 10) +
  scale_color_manual(values = new_palette_colors[c(2,6)]) +
  labs(
    x = NULL, y = NULL, lty = NULL, color = NULL,
    title = "Վարկի միջին տոկոսադրույքերը ֆիզիկական անձանց համար",
    subtitle = "Հայաստանի ֆինանսական շուկայի տոկոսադրույքները (ամսական)", 
    caption = paste0(caption_arm, "   |    տվյալների աղբյուր՝ cba.am")
  ) +
  theme(legend.position = "bottom")

######################
