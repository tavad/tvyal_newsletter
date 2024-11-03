library(tidyverse)

source("~/R/newsletter/initial_setup.R")

pop_forecast |> 
  filter(name %in% c(
    # "Latvia", 
    "Lithuania"
    # "Estonia"
    )) |> 
  filter(!is.na(pop)) |> 
  ggplot(aes(year, pop, colour = name)) +
  geom_line() +
  geom_vline(xintercept = c(1991, 2004))
