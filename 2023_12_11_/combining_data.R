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
  mutate(
    database = map(path, read_excel, sheet = 1)
  )

ardyunaberutyun |> 
  saveRDS("C:/Users/Lenovo/Desktop/R/Gcapatker/2023_12_08_armstat_publications_scrape/ardyunaberutyun.RDS")
