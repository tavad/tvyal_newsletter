library(tidyverse)

data <- 
  tibble(
    files = list.files("~/R/newsletter/2024/", recursive = TRUE, full.names = TRUE)
  ) |> 
  filter(
    grepl("\\.rmd$|\\.r$", tolower(files)),
    # grepl("TN", files)
  ) |> 
  mutate(
    rmd = map(files, read_csv)
  )

data |> 
  mutate(nrow = map(rmd, nrow)) |> 
  unnest(nrow) |> 
  summarise(sum(nrow))
