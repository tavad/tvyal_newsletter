# folder = "~/R/Gcapatker/2023_12_08_armstat_publications_scrape/7z/"
folder = "C:/Users/Lenovo/Desktop/R/Gcapatker/2023_12_08_armstat_publications_scrape/"


files_dictionary <- 
  tibble(
    path = list.files(paste0(folder, "extracted/"), recursive = TRUE, full.names = TRUE)
  ) |> 
  extract(
    path, into = c("language", "date", "code", "filetype"),
    regex = ".+extracted/([a-z]+)_([\\d_]+)/(.+)\\.([a-z]+)$",
    remove = FALSE
  )

files_dictionary <- 
  files_dictionary |> 
  mutate(
    date = ym(date)
  ) |> 
  extract(
   code, "code", regex = ".+_(\\d+)$" 
  ) |> 
  mutate(code = as.numeric(code))


socio_economic_tables_info <- 
  read_csv("C:/Users/Lenovo/Desktop/R/newsletter/2023_12_11_/socio_economic_tables_info.csv")

joind_dictionary <- 
  socio_economic_tables_info |> 
  filter(!is.na(code)) |> 
  mutate(code = ifelse(grepl("^5", code), code*10, code)) |> 
  inner_join(files_dictionary, join_by(date, code))


ardyunaberutyun <- 
  joind_dictionary |> 
  filter(
    # grepl("միջազգային զբոսաշրջություն", tolower(info)),
    # grepl("իրավախախտումներ", tolower(info)),
    # grepl("անշարժ", tolower(info)),
    # grepl("հաշվ", tolower(info)),
    grepl("արդյունաբերություն", tolower(info))
  )


ardyunaberutyun <- 
  ardyunaberutyun |> 
  filter(date != as.Date("2015-09-01")) |> 
  mutate(sheets = map(path, excel_sheets)) |> 
  unnest(sheets)

ardyunaberutyun2 <- 
  ardyunaberutyun |> 
  mutate(database = map2(path, sheets, ~read_excel(.x, sheet = .y, col_names = FALSE)))

ardyunaberutyun2 |> 
  saveRDS("C:/Users/Lenovo/Desktop/R/Gcapatker/2023_12_08_armstat_publications_scrape/ardyunaberutyun.RDS")



ardyunaberutyun2 <-  
  readRDS("C:/Users/Lenovo/Desktop/R/Gcapatker/2023_12_08_armstat_publications_scrape/ardyunaberutyun.RDS")

library(tidyverse)
library(readxl)

test <- 
  ardyunaberutyun2 |> 
  # slice_sample(n = 100) |> 
  select(date, sheets, database) |> 
  mutate(
    database = map(database, ~mutate(.x, across(everything(), ~as.character(.x))))
  ) |> 
  unnest(database)

test |> 
  rename(x1 = 3) |> 
  filter(
    !if_all(-c(date, sheets), ~is.na(.x)),
    !grepl("___", x1)
  ) |> 
  mutate(
    info = ifelse(
      if_all(-c(date, sheets, x1), ~is.na(.x)),
      x1,
      NA
    )
  ) 

electricity <- 
  test %>%
  rename(x1 = 3) |> 
  filter(
    !if_all(-c(date, sheets), ~is.na(.x)),
    !grepl("___", x1)
    ) %>%
  mutate(
    info = ifelse(
      rowSums(!is.na(select(., -c(date, sheets)))) == 1,
      coalesce(!!!select(., -c(date, sheets))),
      NA_character_
    )
  ) |> 
  filter(!grepl("այդ թվում|որից|ընթացիկ գներով|%-ներ|եռամսյակ", info)) |> 
  fill(info, .direction = "down") |> 
  mutate(
    check = ifelse(info == lag(info), TRUE, FALSE)
  ) |> 
  relocate(date, sheets, info, check) |> 
  filter(check) |> 
  filter(grepl("Էլեկտրաէներգիայի", info))
  
  
  
count(info) |> 
  view()

  
  

# արտադրության ծավալներն ըստ տնտեսական գործունեության տեսակների
# արտադրությունն ըստ ՀՀ մարզերի և ք. Երևանի
# արտադրանքի ծավալը և արտադրության ինդեքսները
# Արդյունաբերությամբ զբաղվող տնտեսավարող սուբյեկտների արտադրանքի  արտադրության և իրացման ծավալներն ըստ աշխատողների թվաքանակով որոշվող չափերի
# Էլեկտրաէներգիայի և ջերմաէներգիայի
# Հանքագործական արդյունաբերութ
# Հիմնային  մետաղների  արտադրությամբ
# Հիմնային քիմիական նյութերի, քիմիական արտադրանքների,
# Մանածագործական արտադրատեսակների 
# Մանածագործական և հագուստի  արտադրությամբ զբաղվող կազմակերպությունների
# Մետաղական արտադրատեսակների 
# Պատրաստի մետաղե արտադրատեսակների, համակարգիչների, էլեկտրոնային

compact_data <- function(data) {
  for (row in 1:nrow(data)) {
    non_na_indices <- which(!is.na(data[row, ]))
    data[row, ] <- c(data[row, non_na_indices], rep(NA, ncol(data) - length(non_na_indices)))
  }
  return(data)
}


electricity <- 
  electricity |> 
  compact_data() |> 
  select_if(~any(!is.na(.)))


electricity %>%
  select(-c(sheets, info, check)) |> 
  view()
  
  summarise(across(everything(), ~mean(is.na(.)) * 100, .names = "{.col}"))
  



# 
#   your_data <- data.frame(
#     x1 = c(NA, NA, NA, NA, NA, NA, 1, 1, 1),
#     x2 = c(1, 1, 1, NA, NA, NA, 2, 2, 2),
#     x3 = c(NA, NA, NA, NA, NA, NA, 3, 3, 3),
#     x4 = c(2, 2, 2, 1, 1, 1, NA, NA, NA),
#     x5 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
#     x6 = c(3, 3, 3, 2, 2, 2, NA, NA, NA),
#     x7 = c(NA, NA, NA, 3, 3, NA, NA, NA, NA),
#     x8 = c(NA, NA, NA, NA, NA, 3, NA, NA, NA)
#   )
#   
#   # Compact the data
#   compact_data_result <- compact_data(your_data)
#   print(compact_data_result)
#   