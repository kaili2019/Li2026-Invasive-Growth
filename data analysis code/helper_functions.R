#from https://readxl.tidyverse.org/articles/readxl-workflows.html
read_then_csv <- function(sheet, path) {
  pathbase <- path %>%
    basename() %>%
    tools::file_path_sans_ext()
  path %>%
    read_excel(sheet = sheet) %>% 
    write_csv(paste0("raw_data/count_data/",pathbase, "-", sheet, ".csv"))
}

clean_csv <- function(filename){
  csv_data <- read_csv(filename)
  #select rows before the resorted table for prism
    csv_data <- csv_data[1:(which(csv_data[,1] == "resort for prism")-1),]
  #select rows before target mutants listed
  if(any(csv_data[,1] == "List of target yeast deletion mutats:", na.rm = TRUE)){
    csv_data <- csv_data[1:(which(csv_data[,1] == "List of target yeast deletion mutats:")-1),] 
  }
  #remove completely empty rows
  empty_rows <- apply(csv_data, 1, function(x) all(is.na(x)))
  csv_data <- csv_data[!empty_rows,]
  #convert filename to column 
  file_sheet <- stringr::str_split_fixed(filename,"-",2)
  csv_data <- csv_data %>%
    mutate(across(everything(), as.character))%>%
    janitor::clean_names()%>%
    mutate(file_name = str_remove(file_sheet[,1],here::here("raw_data/count_data/")),
           sheet_name = file_sheet[,2])
}

#useful function from 
#https://github.com/tidyverse/readxl/issues/716
# a function that takes a character vector that may contain dates in various formats, and attempts to convert each format to a date value appropriately
convert_excel_dates <-
  function(x){
    case_when(
      str_detect(x, "^[0-9]{2,4}_[0-9]{1,2}_[0-9]{1,2}$") ~ ymd(x),   # handles values imported as text values in the format "MM/DD/YYYY"
      str_detect(x, "^[0-9]{2,4}-[0-9]{1,2}-[0-9]{1,2}$") ~ ymd(x),   # handles values imported as text values in the format "MM/DD/YYYY"
      str_detect(x, "^[0-9]{5}$")                         ~ x |> as.integer() |> as.Date(origin = as.Date("1899-12-30")),  # handles values imported as numbers expressed as days since 1899-12-30 (Microsoft's convention)
      TRUE                                                ~ NA_Date_  # default case, no applicable date format, returns a missing date value
    )
  }
