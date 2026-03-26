# this script merges data from the count and image processed 
# datasets of invasive growth 
# 
# Kai Li
# 27 July 2025

# load libraries
library(dplyr)
library(ggplot2)

# read data from rds which has been cleaned by Lauren
cleaned_count_data <- readRDS(here::here("clean_data/cleaned_count_data.rds"))
cleaned_image_data <- readRDS(here::here("clean_data/cleaned_image_data.rds"))

# Rename at each level

# add agar_type in image data to be the same count data (agar_type)
cleaned_image_data_renamed <- cleaned_image_data %>% 
  mutate(agar_type = case_when(
    nutrient == "BD50" ~ "bd_agar",
    nutrient == "BD75" ~ "bd_agar",
    nutrient == "BD100" ~ "bd_agar",
    nutrient == "Ox50" ~ "oxoid_agar",
    nutrient == "Ox75" ~ "oxoid_agar",
    nutrient == "Ox100" ~ "oxoid_agar"
  )
)

# change nutrient in image data to be the same count data (nutrient)
cleaned_image_data_renamed <- cleaned_image_data_renamed %>% 
  mutate(nutrient = case_when(
    nutrient == "BD50" ~ 50,
    nutrient == "BD75" ~ 75,
    nutrient == "BD100" ~ 100,
    nutrient == "Ox50" ~ 50,
    nutrient == "Ox75" ~ 75,
    nutrient == "Ox100" ~ 100
    )
)

# change day in image data to be the same count data (sheet_day)
cleaned_image_data_renamed <- cleaned_image_data_renamed %>% 
  mutate(day = case_when(
    day == "day3" ~ 3,
    day == "day4" ~ 4,
    day == "day6" ~ 6,
    day == "day13" ~ 13
  )
)

# change pre-slad in count data to be the same image data (liquid_starter_culture_medium)
cleaned_count_data_renamed <- cleaned_count_data %>% 
  mutate(liquid_starter_culture_medium = case_when(
    liquid_starter_culture_medium == "2 x SLAD" ~ "2xSLAD",
    # liquid_starter_culture_medium == "2 x SLAD + 200 ug S/L" ~ "2xSLAD", ## ???
    liquid_starter_culture_medium == "1 x SLAD" ~ "1xSLAD",
    TRUE ~ liquid_starter_culture_medium
  )
)

# Q: is "2 x SLAD + 200 ug S/L" ~ "2xSLAD" the same as "2 x SLAD"?
# Jen: No they are different 

# change post-slad in count data to be the same image data (experimental_medium)
cleaned_count_data_renamed <- cleaned_count_data_renamed %>% 
  mutate(experimental_medium = case_when(
    experimental_medium == "2 x SLAD" ~ "2xSLAD",
    experimental_medium == "1 x SLAD" ~ "1xSLAD",
    TRUE ~ experimental_medium
  )
)


# Using rename() from dplyr (Tidyverse) to rename columns 
# rename cleaned_count_data_renamed dataframe to be appropriate names
cleaned_count_data_renamed <- cleaned_count_data_renamed %>% 
  rename(
    pre_slad = liquid_starter_culture_medium,
    post_slad = experimental_medium,
    day = sheet_day,
    sulfide_amount = sodium_sulfide,
    exp_order = experiment,
    mutant_id = strain
)

# change mutant_id in count data to be the same image data (mutant_id)
cleaned_count_data_renamed <- cleaned_count_data_renamed %>% 
  mutate(mutant_id = case_when(
    mutant_id == "796" ~ "parent",
    mutant_id == "796 pma2" ~ "pma",
    mutant_id == "796 put4" ~ "put",
    mutant_id == "796 vht1" ~ "vht",
    mutant_id == "796 sac3" ~ "sac",
    mutant_id == "796 yhl008c J1" ~ "yhl",
    mutant_id == "796 yhl008c J4" ~ NA, ## doesn't have J4 or J1
    mutant_id == "796 tmn3" ~ "tmn3",
    mutant_id == "796 dur3" ~ "dur",
    mutant_id == "796 nrt1" ~ "nrt1",
    mutant_id == "79 alr2" ~ "alr",
    mutant_id == "1278B" ~ "1278b", 
    mutant_id == "L2056" ~ "L2056",
    mutant_id == "796 fat1" ~ "fat1",
    mutant_id == "796 gup1" ~ "gup1",
    mutant_id == "796 skp2" ~ "skp2",
    mutant_id == "796 soa1" ~ "soa1",
    mutant_id == "796 yor1" ~ "yor1",
    mutant_id == "796 cvt16" ~ "cvt1",
    mutant_id == "796 cdh1" ~ "cdh1",
    mutant_id == "796 pep12" ~ "pep1",
    mutant_id == "796 whi3" ~ "whi3",
    # vsp2 in count data is mislabelled and should be vps2 
    mutant_id == "796 vsp28" ~ "vps2", 
    mutant_id == "796 msa1" ~ "msa1",
    mutant_id == "796 ccz1" ~ "ccz1",
    mutant_id == "796 yps8" ~ "rps8", ## Mis-spelling: 'r' instead of 'y'?
    mutant_id == "796 alr2" ~ "alr",
    mutant_id == "796 ato3" ~ "ato",
    mutant_id == "796 fui1" ~ "fui",
    mutant_id == "796 mid1" ~ "mi_",
    mutant_id == "796 msb2" ~ "msb",
    mutant_id == "796 tpo4" ~ "tpo",
    TRUE ~ mutant_id
  )
)

cleaned_count_data_renamed <- cleaned_count_data_renamed %>%
  mutate(exp_order = case_when(
    exp_order == "Exp4" ~ "ExpA",
    TRUE ~ exp_order
  )
)

cleaned_count_data_renamed <- cleaned_count_data_renamed %>%
  mutate(exp_order = case_when(
    exp_order == "ExpA" ~ "E4",
    exp_order == "ExpB" ~ "E5",
    exp_order == "ExpC" ~ "E1",
    exp_order == "ExpD" ~ "E2",
    exp_order == "ExpE" ~ "E3",
    TRUE ~ exp_order
  )
)

# tmn1 does not exist in count data: should it be tmn3 instead of tmn1?


# All of L2056 should have 2xSLAD post_slad instead of 1xSLAD in image data, currently image dataset file names does not contain this information and hence assumed to be 1xSLAD. Present in E2 (Exp D) Day 3
# Moreover, all of E2 (Exp D) Day 3 should be 2xSLAD
cleaned_image_data_renamed$post_slad[cleaned_image_data_renamed$exp_order=="E2"] <- "2xSLAD"
  

# 796_BD+400S_4x_Day3_1-5 are missing the BD level and assume to be causing the NaN
# tmn should be tmn3 for E5 Days 6
# tmn should be tmn3 for E5 Days 4
# nrt should be nrt1 for E5 Days 4

# tmn1 is renamed incorrect in image data, should it be tmn3. 

cleaned_image_data_renamed <- cleaned_image_data_renamed %>% 
  mutate(mutant_id = case_when(
    exp_order == "E5" & mutant_id == "tmn" ~ "tmn3",
    exp_order == "E3" & mutant_id == "tmn1" ~ "tmn3",
    exp_order == "E5" & mutant_id == "nrt" ~ "nrt1",
    exp_order == "E4" & mutant_id == "mi" ~ "mi_",
    TRUE ~ mutant_id
  ))

# tpo day 4 from Exp A is missing from image data. Issue is that slad_expA_day4_v5 is not contained in cleaned_image_data
# Issue resolved: index in clean_image_data.R change from 1 instead of 2.

# convert columns to the same type/class
cleaned_count_data_renamed <- cleaned_count_data_renamed %>% mutate_if(is.character, as.factor)
cleaned_count_data_renamed <- cleaned_count_data_renamed %>% mutate_if(is.numeric, as.factor)
cleaned_image_data_renamed <- cleaned_image_data_renamed %>% mutate_if(is.double, as.factor)

cleaned_count_data_renamed <- cleaned_count_data_renamed %>% group_by(mutant_id, nutrient, pre_slad, post_slad, day, exp_order, sulfide_amount, agar_type) %>%  
  mutate(count_n = n()) %>% 
  ungroup()

cleaned_image_data_renamed <- cleaned_image_data_renamed %>% 
  group_by(mutant_id, nutrient, pre_slad, post_slad, day, exp_order, sulfide_amount, agar_type) %>%  
  mutate(image_n = n()) %>% 
  ungroup()

# remove repeats columns 
# cleaned_image_data_renamed <- cleaned_image_data_renamed %>% 
#   select(-yeast_id, -sulfide)

# remove nan rows from count data as these contain no information
cleaned_count_data_renamed <- cleaned_count_data_renamed[!(is.na(cleaned_count_data_renamed$mutant_id) & is.na(cleaned_count_data_renamed$total_colonies_before_wash)),]

##################################################################
#################### removed vht mutant ##########################
##################################################################

# we decided to remove the vht data because it contains zero for 
# total_invasive_colonies_after_wash as it was too small to observe at day 3/4.
# Hence, missing the washed data for this making analysis infeasible.
# This colony was the only colony grown upto day 13. Hence, we decided to remove 
# it. 

# duplicate rows in vht as there are two replicates in the same starting date
# *grew longer as slow growing mutant 

# which(is.na(cleaned_count_data_renamed$total_invasive_colonies_after_wash))

which((cleaned_count_data_renamed$mutant_id == "vht"))

cleaned_count_data_renamed <- cleaned_count_data_renamed[!(cleaned_count_data_renamed$mutant_id == "vht"),]

# for some reason removing vht adds NaN to the dataset, hence remove
cleaned_count_data_renamed <- cleaned_count_data_renamed[!is.na(cleaned_count_data_renamed$mutant_id),]

cleaned_image_data_renamed <- cleaned_image_data_renamed[cleaned_image_data_renamed$mutant_id != "vht",]

# remove 	pre-slad containing "2 x SLAD + 200 ug S/L" as we do not have image data for the corresponding count data

cleaned_count_data_renamed <- cleaned_count_data_renamed[cleaned_count_data_renamed$pre_slad != "2 x SLAD + 200 ug S/L",]

# remove rows where the nutrient is missing???
cleaned_image_data_renamed <- cleaned_image_data_renamed[!is.na(cleaned_image_data_renamed$nutrient),]

# update count data to deal with correction: Exp 4 (ExpA) Day 4 correction.xlsx
# No longer needed due to second correction

#idx = cleaned_count_data_renamed$mutant_id == "parent" & cleaned_count_data_renamed$pre_slad == "1xSLAD" & cleaned_count_data_renamed$post_slad == "2xSLAD" & 
#  cleaned_count_data_renamed$agar_type == "bd_agar" & cleaned_count_data_renamed$exp_order == "E4"

#cleaned_count_data_renamed[idx,]$total_invasive_colonies_after_wash <- cleaned_count_data_renamed[idx,]$total_colonies_before_wash


df_joined <- cleaned_image_data_renamed %>%  full_join(cleaned_count_data_renamed)

write_rds(cleaned_count_data_renamed, here::here("data/cleaned_count_data_renamed.rds"))
write_rds(cleaned_image_data_renamed, here::here("data/cleaned_image_data_renamed.rds"))


m2m_df <- df_joined[df_joined$count_n >= 2,]

# Specify the columns to check
## Need to view spread sheet 

missing_val_df <- df_joined[rowSums(is.na(df_joined)) > 0,]
missing_df <- missing_val_df[c("mutant_id", "nutrient", "pre_slad", "post_slad", "day", "exp_order", "sulfide_amount", "agar_type", "total_colonies_before_wash", "total_invasive_colonies_after_wash")]

missing_val_df_image <- cleaned_image_data_renamed[rowSums(is.na(cleaned_image_data_renamed)) > 0,]
missing_val_df_count <- cleaned_count_data_renamed[rowSums(is.na(cleaned_count_data_renamed)) > 0,]

# write.csv(missing_df,"missing_image_data_13May2025.csv")

# For each experiment the number of images of each experiment should be less and equal in each count data
df_joined <- df_joined %>%
  group_by(mutant_id, nutrient, pre_slad, post_slad, day, exp_order,
        sulfide_amount, agar_type) %>%  
        mutate(number_of_images = as.factor(n())) %>% 
        ungroup()

sum(as.integer(df_joined$number_of_images) <= as.integer(df_joined$total_colonies_before_wash), na.rm = TRUE)/length(na.omit(df_joined$total_colonies_before_wash))

which(as.integer(df_joined$number_of_images) > as.integer(df_joined$total_colonies_before_wash))

# Further checks 

# create new column called image_total_invaded to count the total number of colonies that have invaded in the image data per experimental condition
df_joined <- df_joined %>%
  mutate(image_is_invaded = case_when(
    area_ratio == 0 ~ "no_invasion",
    TRUE ~ "yes_invasion"
  )) %>% 
  group_by(mutant_id, nutrient, pre_slad, post_slad, day, exp_order,
           sulfide_amount, agar_type) %>%  
  mutate(image_total_invaded = as.factor(sum(image_is_invaded == "yes_invasion"))) %>% 
  ungroup()

washed_count <- as.numeric(as.character(df_joined$total_invasive_colonies_after_wash))

unwashed_count <- as.numeric(as.character(df_joined$total_colonies_before_wash))

washed_image <- as.numeric(as.character(df_joined$image_total_invaded))

total_image <- as.numeric(as.character(df_joined$number_of_images))

# 1. Check full invasive for count and image data 
erroneous_df_1 <- df_joined[setdiff(which(unwashed_count - washed_count == 0),
        which(total_image - washed_image == 0)),]

erroneous_df_1_filtered <- erroneous_df_1 %>% 
  filter(area_ratio == 0)

# 2. Check non-invasive growth (washed == 0) in images and count data
erroneous_df_2 <- df_joined[setdiff(which(unwashed_count - washed_count == unwashed_count),
                                   which(total_image - washed_image == total_image)),]

# 3. Check when washed image is more than washed count
erroneous_df_3 <- df_joined[washed_count < washed_image,]

# Ask specific questions and link to what Jen can see. e.g., contextual: count data example 
# What is in the CSV and 
# What we can learn more from the image data instead of the count data 
# Appendix: write the process of cleaning 



