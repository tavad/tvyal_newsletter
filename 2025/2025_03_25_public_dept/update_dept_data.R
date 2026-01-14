library(tidyverse)

# info pages:
# xlsx: https://minfin.am/hy/page/amsakan_vichakagrakan_teghekagrer/
# pdf:  https://minfin.am/hy/page/amsakan_ampop_teghekagir/

script_path = "/home/tavad/R/newsletter/2025/2025_03_25_public_dept"

# requires the file at file.path(script_path, "dept_clean.csv")


arm_month_names <- c(
  "Հունվար", "Փետրվար", "Մարտ", "Ապրիլ", "Մայիս", "Հունիս", "Հուլիս",
  "Օգոստոս", "Սեպտեմբեր", "Հոկտեմբեր", "Նոյեմբեր", "Դեկտեմբեր"
)

xlsx_elements <- 
  read_html("https://minfin.am/hy/page/amsakan_vichakagrakan_teghekagrer/") |> 
  html_elements(".doc_title > a")

max_avalable_date_in_dbs <- 
  tibble(db_name =html_text(xlsx_elements)) |> 
  extract(
    db_name, into = c("year", "month_arm"), 
    regex = ".* (\\d{4}) թվական ?\\(?([Ա-Ֆա-ֆ]*)?\\)?"
  ) |> 
  mutate(
    month_arm = str_to_sentence(month_arm),
    month_arm = ifelse(month_arm == "", "Դեկտեմբեր", month_arm),
    month = c(1:12)[match(month_arm, arm_month_names)],
    date = ym(paste(year, month)) + months(1) - days(1)
  ) |> 
  filter(date == max(date)) |> 
  pull(date)


dept_clean <-  
  read_csv(file.path(script_path, "dept_clean.csv"))

if (max_avalable_date_in_dbs != max(dept_clean$date)) {
  
  dept_dict <- read_csv(file.path(script_path, "dept_dict.csv"))
  
  initial_data <- 
    tibble(
      title = html_text(xlsx_elements),
      link = html_attr(xlsx_elements, "href")
    ) |> 
    mutate(
      data = map(link, ~rio::import(URLencode(.x), which = 1))
    )
  
  # the convoluted function below just adjusts 2 out-of-pace values in 2017 database
  dept_data_initial_setup <- function(tbl){
    
    rnumber_of_rows <- nrow(tbl)
    
    result_tbl <-
      tbl |> 
      rename(indicator = 1) |> 
      mutate(
        indicator = ifelse(
          lag(indicator) %in% c("մլրդ դրամ", "մլն ԱՄՆ դոլար") & is.na(indicator), 
          lag(indicator),
          indicator
        ),
        indicator = ifelse(is.na(indicator), "NA", indicator),
        indicator = ifelse(
          lead(indicator) != indicator | row_number() == rnumber_of_rows, 
          indicator,
          NA
        ),
        indicator = ifelse(indicator == "NA", NA, indicator)
      )
    
    return(result_tbl)
  }
  
  
  dept_data_get_colnames <- function(tbl){
    colnames <- 
      tbl |> 
      filter(indicator == "մլրդ դրամ") |> 
      mutate(indicator = "indicator")
    
    colnames <- unlist(colnames[1,], use.names = FALSE)
    
    return(colnames)
  }
  
  
  dept_data_manipulate <- function(tbl, colnames){
    
    FX_units <- c("մլրդ դրամ", "մլն ԱՄՆ դոլար")
    
    tbl_result <- 
      tbl |> 
      set_names(colnames) |> 
      mutate(
        unit_FX = ifelse(
          is.na(lag(indicator)) | grepl(
            "անվանական արժեքով", lag(indicator)) | 
            grepl("ՀՀ.+պետական.+պարտք", lead(indicator)
            ),
          indicator, 
          NA
        )
      ) |> 
      fill(unit_FX, .direction = "down") |> 
      filter(!indicator %in% FX_units, !is.na(indicator)) |> 
      pivot_longer(-c(indicator, unit_FX), names_to = "date") |> 
      mutate(
        value = parse_number(value),
        date = dmy(date)
      )
    
    return(tbl_result)
  }
  
  dept_clean <- 
    initial_data |> 
    # filter(!grepl("2017", title)) |> 
    mutate(
      data = map(data, dept_data_initial_setup),
      colnames = map(data, dept_data_get_colnames),
      data = map2(data, colnames, dept_data_manipulate)
    ) |> 
    select(-colnames) |> 
    unnest(data) |> 
    mutate(
      year = year(date),
      month = month(date),
      year_report = str_replace(title, ".*(\\d{4}).*", "\\1"),
      indicator = str_remove_all(indicator, "\\*")
    ) |> 
    filter(
      !is.na(value),
      year_report == year | year_report == 2017
    ) |> 
    unique() |> 
    left_join(dept_dict, by = join_by(unit_FX, indicator)) |> 
    select(date, year, month, unit_FX, indicator, code, value) |> 
    arrange(code, unit_FX, indicator, date)
  
  dept_clean |> 
    write_excel_csv(file.path(script_path, "dept_clean.csv"))
  
}
