library(readxl)
library(tidyverse)
library(lubridate)

source(here::here("code/helper_functions.R"))

path <- here::here("data/Summary IG Macro observations_2019_Nov to Dec_updated.xlsx")
#small modification to move the resort to prism into the same column (experiment 1, day 3 column F name added)
#modified the missing values by dragging in each group the type of mutant on Exp 4 day 6 sheet by hand as per email
#experiment 1, day 3 column F name added

path %>%
  excel_sheets() %>%
  set_names() %>% 
  map(read_then_csv, path = path)

#clean up read in
if (file.exists(here::here("raw_data/count_data/Summary IG Macro observations_2019_Nov to Dec_updated-Read me.csv"))) {
  #Delete file if it exists
  file.remove(here::here("raw_data/count_data/Summary IG Macro observations_2019_Nov to Dec_updated-Read me.csv"))
}
if (file.exists(here::here("raw_data/count_data/Summary IG Macro observations_2019_Nov to Dec_updated-column graph for rough comparis.csv"))) {
  #Delete file if it exists
  file.remove(here::here("raw_data/count_data/Summary IG Macro observations_2019_Nov to Dec_updated-column graph for rough comparis.csv"))
}

filenames <- list.files(here::here("raw_data/count_data/"), pattern="*.csv", full.names=TRUE)

experiment_data <- purrr::map(filenames, clean_csv)

full_data <- bind_rows(experiment_data)

full_data$image_date_clean <- convert_excel_dates(full_data$image_date)

full_data$experiment_start_date_clean <- convert_excel_dates(full_data$experiment_start_date)

full_data <- full_data %>%
  mutate(
        # clean up dates
         image_day = as.double(difftime(image_date_clean,experiment_start_date_clean, units = c("days"))),
         #pull out important info from the sheet name
         sheet_day = parse_number(str_extract(sheet_name, "Day .")),
         experiment = str_replace_all(str_extract(sheet_name, "Exp .")," ",""),
         #light tidy of data
         sodium_sulfide = replace_na(as.numeric(sodium_sulfide_added_ug_of_s_l), 0),
         agar_type = ifelse(experimental_medium == "2 x SLAD (BD agar)", "BD Agar",agar_type),
         agar_type = snakecase::to_snake_case(agar_type),
         experimental_medium = ifelse(experimental_medium == "2 x SLAD (BD agar)", "2 x SLAD",experimental_medium),
         isparent = strain == "796",
         nutrient = nitrogen_added_to_agar_medium_u_m_of_ammonium_sulfate)

clean_data <- full_data %>%
  select(strain, liquid_starter_culture_medium, experimental_medium, nutrient,
         sodium_sulfide, total_colonies_before_wash, total_invasive_colonies_after_wash, 
         agar_type,experiment, sheet_day, image_day, isparent)

#update sheet day based on non standard naming. 
clean_data$sheet_day[clean_data$experiment == "ExpA"] <- clean_data$image_day[clean_data$experiment == "ExpA"]

saveRDS(clean_data, here::here("clean_data/cleaned_count_data.rds"))
