library(here)
library(tidyverse)


experiment_files = list.files(here::here("raw_data/image_data")) 
experiment_files <- experiment_files[grep("v5|16Bit|vht|ExpA_Day3",experiment_files)]

for(i in 1:length(experiment_files)){ # first file is a read me
  #print(paste0("Reading file ",i," of ", length(experiment_files)))
  if(i == 1){
    full_data <- read_csv(here(paste0("raw_data/image_data/",experiment_files[i])))
    if(experiment_files[i] == "slad_16Bit.csv"){
      full_data$day = "day6" #recode data according to email from Kai Subject of Re: Yeast data 05/09/25
    }
  }else{
    full_data <- rbind(full_data,
                       read_csv(here(paste0("raw_data/image_data/",experiment_files[i]))))
  }
}
full_data <- full_data %>%
  mutate(mutant_id = as.factor(mutant_id),
         sulfide = as.factor(sulfide),
         nutrient = as.factor(nutrient),
         time_of_exp = as.factor(time_of_exp),
         pre_slad = as.factor(pre_slad),
         post_slad = as.factor(post_slad),
         day = as.factor(day),
         mutant_id = fct_relevel(mutant_id, "parent"))

saveRDS(full_data,here("clean_data/cleaned_image_data.rds"))
