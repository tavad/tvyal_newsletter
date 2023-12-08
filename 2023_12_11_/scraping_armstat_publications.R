library(rvest)

# getting all the armstat publication urls from 2010
armstat_publications_urls <- 
  tibble(
    year = 2010:year(today())
  ) |> 
  mutate(
    link =  paste0("https://www.armstat.am/am/?nid=82&year=", year),
    html = map(link, read_html)
  ) |> 
  mutate(
    a_tags = map(html, html_elements, "a"),
    url = map(a_tags, html_attr, "href"),
    name = map(a_tags, html_text)
  ) |> 
  select(year, url, name) |> 
  unnest(c(url, name)) |> 
  mutate(url = str_replace(url, "^\\.\\/\\?", "https://www.armstat.am/am/?"))

arm_months <- c(
  "հունվարին", "փետրվարին", "մարտին",
  "ապրիլին", "մայիսին", "հունիսին",
  "հուլիսին", "օգոստոսին", "սեպտեմբերին",
  "հոկտեմբերին", "նոյեմբերին", "դեկտեմբերին"
)

# filtering out socio-economic publications
socio_economic_urls <- 
  armstat_publications_urls |> 
  filter(grepl("սոցիալ-տնտեսական վիճակը", name)) |> 
  mutate(name = str_trim(name)) |> 
  extract(
    name, into = c("year", "month"),
    regex = ".*(\\d{4}).*[ --]([ա-ֆԱ-Ֆ]+$)", 
    remove = FALSE
  ) |> 
  mutate(
    month = c(1:12)[match(month, arm_months)],
    date = ym(paste(year, month))
  ) |> 
  arrange(date)

# downloading socio-econnomic htmls
socio_economic_htmls <- 
  socio_economic_urls |> 
  mutate(
    html = map(url, read_html)
  )


socio_economic_tables_info <- 
  socio_economic_htmls |> 
  mutate(
    table = map(html, html_elements, "table"),
    table = map(table, html_table)
  ) |> 
  select(date, table) |> 
  unnest(table) |> 
  unnest(table)

socio_economic_tables_info <- 
  socio_economic_tables_info |> 
  rename(info = X1) |> 
  select(-c(X2, X3, X4)) |> 
  mutate(info = str_trim(info)) |> 
  filter(info != "") |> 
  extract(info, "code", regex = "^([\\d\\.]+) .+", remove = FALSE) |> 
  mutate(
    level = str_count(code, "\\."),
    code = str_remove_all(code, "\\.")
  )
  
socio_economic_tables_info |> 
  write_csv("C:/Users/Lenovo/Desktop/R/newsletter/2023_12_11_/socio_economic_tables_info.csv")

  

data_7z_urls <- 
  socio_economic_htmls |> 
  mutate(
    a_tags = map(html, html_elements, "a"),
    file_url = map(a_tags, html_attr, "href")
  ) |> 
  select(url, date, file_url) |> 
  unnest(file_url) |> 
  filter(grepl("7z$", file_url)) |> 
  mutate(
    type = ifelse(grepl("ex|xls|xsl|_e", file_url), "xls", "doc"),
    language = case_when(
      grepl("[\\-]r|r[\\-]", file_url) ~ "rus",
      grepl("[_\\.]r|r[_\\.]", file_url) ~ "rus",
      grepl("[\\-]a|a[\\-]", file_url) ~ "arm",
      grepl("[_\\.]a|a[_\\.]", file_url) ~ "arm",
      TRUE ~ "arm"
    ),
    file_url = str_replace(file_url, "^\\.\\.\\/", "https://www.armstat.am/")
  )

data_7z_urls_xls <- 
  data_7z_urls |> 
  filter(
    language == "arm",
    type == "xls"
  )

# folder = "~/R/Gcapatker/2023_12_08_armstat_publications_scrape/7z/"
folder = "C:/Users/Lenovo/Desktop/R/Gcapatker/2023_12_08_armstat_publications_scrape/"

for (i in 1:nrow(data_7z_urls_xls)) {
  link_7z = pull(data_7z_urls_xls[i,], file_url)
  date = pull(data_7z_urls_xls[i,], date)
  date = paste0(year(date), "_",
                formatC(month(date), digits = 1, flag = "0", format = "fg"))
  language = pull(data_7z_urls_xls[i,], language)
  name = paste0(folder, "7z/", language, "_", date, ".7z")

  system(
    paste0(
      "curl ", link_7z, " -o ", name
    )
  )
  cat(name, "is downloaded")
}



files_7z <- list.files(path = paste0(folder, "7z/"), full.names = TRUE, recursive = FALSE)
# chained = ""



for (i in 1:length(files_7z)) {
  
  folder_name <- str_replace_all(files_7z[i], ".*/7z/(.*)\\.7z", "\\1")
  destination_path <- paste0(folder, 'extracted\\', folder_name)
  command <- sprintf('7z e -o"%s" "%s"', destination_path, files_7z[i])
  
  system(command)
  # chained = paste(chained, "&", command)
}

# cat(chained)





