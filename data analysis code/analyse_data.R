library(tidyverse)
library(here)
library(kableExtra)
library(betareg)
library(ggpubr)
library(modelsummary)
library(brms)
library(skimr)
library(Cairo)

full_data_image <- readRDS("data/cleaned_image_data_renamed.rds")%>%
  mutate(surface_area = as.numeric(as.character(surface_area)),
         washed_area = as.numeric(as.character(washed_area)),
         area_ratio = as.numeric(as.character(area_ratio)),
         mutant_id = fct_recode(mutant_id, alr2 = "alr", 
                                ato3 = "ato",
                                cvt16 = "cvt1",
                                dur3 = "dur",
                                fui1 = "fui",
                                mid1 = "mi_",
                                msb2 = "msb",
                                pep12 = "pep1",
                                put4 = "put",
                                pma2 = "pma",
                                rps8a = "rps8",
                                sac3 = "sac",
                                tpo4 = "tpo",
                                vps28 = "vps2",
                                yhl008c = "yhl"),
         mutant_id = relevel(factor(mutant_id), ref = "parent"))
full_data_count <- readRDS("data/cleaned_count_data_renamed.rds")%>%
  mutate(total_invasive_colonies_after_wash = as.numeric(as.character(total_invasive_colonies_after_wash)),
         total_colonies_before_wash = as.numeric(as.character(total_colonies_before_wash)),
         mutant_id = fct_recode(factor(mutant_id), alr2 = "alr", 
                                ato3 = "ato",
                                cvt16 = "cvt1",
                                dur3 = "dur",
                                fui1 = "fui",
                                mid1 = "mi_",
                                msb2 = "msb",
                                pep12 = "pep1",
                                rps8a = "rps8",
                                sac3 = "sac",
                                tpo4 = "tpo",
                                vps28 = "vps2",
                                yhl008c = "yhl"),
         mutant_id = relevel(factor(mutant_id), ref = "parent"))


#### First Experiment ####

e1_data_image <- full_data_image %>%
  filter(exp_order == "E1")

e1_data_image %>%
  group_by(nutrient, day, sulfide_amount,  post_slad, agar_type,)%>%
  summarise(number_images = n()) %>%
  summary()

e1_data_count <- full_data_count %>%
  filter(exp_order == "E1")

#There are 72 experimental conditions. 
# All parent yeasts on a pre-slad conditon. 
# Half on 1xSlad, half on 2xSlad in post slad condition
# Nitrogen levels of 100, 75 and 50
# Sulfide conditions of 0, 400, 750
# Day conditions of 3 and 6 days
# = a fully factorial design. 
nrow(e1_data_count)
skim(e1_data_count)

e1_data_count <- e1_data_count %>%
  mutate(proportion_invasive_colonies = total_invasive_colonies_after_wash/total_colonies_before_wash,
         cat_invasive_colonies = ifelse(proportion_invasive_colonies == 0, "none",
                                        ifelse(proportion_invasive_colonies == 1,"all",
                                               "some")),
         none_invasive = cat_invasive_colonies == "none",
         all_invasive = cat_invasive_colonies == "all")

#### E1 BD data
count_data_bd <- e1_data_count %>% 
  filter(agar_type == "bd_agar")%>%
  mutate(n_not_invasive = total_colonies_before_wash - total_invasive_colonies_after_wash,
         proportion_invasive_colonies = total_invasive_colonies_after_wash/total_colonies_before_wash)%>%
  mutate(nutrient_amount = factor(paste(nutrient), levels = c("50", "75","100"), ordered = TRUE),
         day = paste("Number of days :", day),
         post_slad = paste("Post Slad: ", post_slad))%>%
  ggplot(., mapping = aes(x = sulfide_amount, y = proportion_invasive_colonies, fill = nutrient_amount, group = nutrient_amount))+
  geom_col(position = "dodge", colour = "darkgrey", width = .7)+
  labs(x = "Sodium Sulfide (\u03BCM)", y = "Presence of invasion",
       fill = "Ammonium Sulfate (\u03BCM)")+
  scale_y_continuous(limits = c(0,1)) +
  facet_grid(agar_type~day+post_slad)+
  viridis::scale_fill_viridis(discrete = TRUE, end = .3, begin = .8, option ="A")+
  theme_bw()+
  theme(legend.position = "bottom",
        strip.text = element_blank())

image_data_bd <- e1_data_image %>% 
  filter(agar_type == "bd_agar")%>%
  mutate(nutrient_amount = factor(paste(nutrient), levels = c("50", "75","100"), ordered = TRUE),
         day = paste("Number of days :", day),
         post_slad = paste("Post Slad: ", post_slad))%>%
  ggplot(., aes(y = area_ratio, x = sulfide_amount, group = nutrient, colour = nutrient))+
  stat_summary(fun=mean, geom="line",  alpha = .7)+
  geom_jitter(width = .1, mapping = aes(colour = nutrient, shape = nutrient))+
  facet_grid(agar_type~day+post_slad)+
  ylab("Degree of invasion")+
  viridis::scale_fill_viridis(discrete = TRUE, end = .3, begin = .8 , option ="A")+
  viridis::scale_colour_viridis(discrete = TRUE, end = .3, begin = .8, option ="A")+
  guides(fill = "none", colour = guide_legend(title="Ammonium Sulfate (\u03BCM)"),
         shape = guide_legend(title="Ammonium Sulfate (\u03BCM)"))+
  ggtitle("BD Agar")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.text.y = element_blank(),
        axis.text.x = element_blank()) 

ggarrange(image_data_bd,count_data_bd, heights = c(.90, 1),
          ncol = 1, nrow = 2, align = "v")

ggsave("images/experiment1_bdagar.pdf", device = cairo_pdf, width = 10, height = 7)

#### E1 Oxoid data
count_data_oxoid <- e1_data_count %>% 
  filter(agar_type == "oxoid_agar")%>%
  mutate(n_not_invasive = total_colonies_before_wash - total_invasive_colonies_after_wash,
         proportion_invasive_colonies = total_invasive_colonies_after_wash/total_colonies_before_wash)%>%
  mutate(nutrient_amount = factor(paste(nutrient), levels = c("50", "75","100"), ordered = TRUE),
         day = paste("Number of days :", day),
         post_slad = paste("Post Slad: ", post_slad))%>%
  ggplot(., mapping = aes(x = sulfide_amount, y = proportion_invasive_colonies, fill = nutrient_amount, group = nutrient_amount))+
  geom_col(position = "dodge", colour = "darkgrey", width = .7)+
  labs(x = "Sodium Sulfide (\u03BCM)", y = "Presence of invasion",
       fill =  "Ammonium Sulfate (\u03BCM)")+
  scale_y_continuous(limits = c(0,1)) +
  facet_grid(agar_type~day+post_slad)+
  viridis::scale_fill_viridis(discrete = TRUE, end = .3, begin = .8, option ="D")+
  theme_bw()+
  theme(legend.position = "bottom",
        strip.text = element_blank())

image_data_oxoid <- e1_data_image %>% 
  filter(agar_type == "oxoid_agar")%>%
  mutate(nutrient_amount = factor(paste(nutrient), levels = c("50", "75","100"), ordered = TRUE),
         day = paste("Number of days :", day),
         post_slad = paste("Post Slad: ", post_slad))%>%
  ggplot(., aes(y = area_ratio, x = sulfide_amount, group = nutrient, colour = nutrient))+
  stat_summary(fun=mean, geom="line",  alpha = .7)+
  geom_jitter(width = .1, mapping = aes(colour = nutrient, shape = nutrient))+
  facet_grid(agar_type~day+post_slad)+
  ylab("Degree of invasion")+
  viridis::scale_fill_viridis(discrete = TRUE, end = .3, begin = .8 , option ="D")+
  viridis::scale_colour_viridis(discrete = TRUE, end = .3, begin = .8, option ="D")+
  guides(fill = "none", colour = guide_legend(title="Ammonium Sulfate (\u03BCM)"),
         shape = guide_legend(title="Ammonium Sulfate (\u03BCM)"))+
  ggtitle("Oxoid Agar")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.text.y = element_blank(),
        axis.text.x = element_blank()) 

ggarrange(image_data_oxoid,count_data_oxoid, heights = c(.90, 1),
          ncol = 1, nrow = 2, align = "v")

ggsave("images/experiment1_oxoidagar.pdf", device = cairo_pdf, width = 10, height = 7)


e1_image_model <- e1_data_image %>%
  mutate(area_ratio = ifelse(area_ratio == 0,0.001,area_ratio)) %>%
  betareg(area_ratio ~ nutrient*sulfide_amount + day*sulfide_amount + post_slad*sulfide_amount + agar_type*sulfide_amount, data = .)
summary(e1_image_model)


e1_interactions_400sulfide <- c(
        'sulfide_amount400:agar_typeoxoid_agar' = 'Oxoid agar',
        'sulfide_amount400:post_slad2xSLAD' = '2xSLAD',
        'sulfide_amount400:day6' = 'Day 6',
        'nutrient75:sulfide_amount400' = '75 \u03BCM Ammonium Sulfate',
        'nutrient100:sulfide_amount400' = '100 \u03BCM Ammonium Sulfate'
        )
e1_interactions_750sulfide <- c(
'sulfide_amount750:agar_typeoxoid_agar' = 'Oxoid agar',
'sulfide_amount750:post_slad2xSLAD' = '2xSLAD',
'sulfide_amount750:day6' = 'Day 6',
'nutrient75:sulfide_amount750' = '75 \u03BCM Ammonium Sulfate',
'nutrient100:sulfide_amount750' = '100 \u03BCM Ammonium Sulfate')

e1_plot_interaction_400sulfide <- modelsummary::modelplot(e1_image_model, coef_map = e1_interactions_400sulfide)+
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
  aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
  xlim(c(-3,4))+
  ylab("Interaction with \n 400 \u03BCM Sodium Sulfide ")+
  scale_color_manual(values = c("black", "orange"))+
  theme_bw()+
  theme(legend.position = "none")

e1_plot_interaction_750sulfide <- modelsummary::modelplot(e1_image_model, coef_map = e1_interactions_750sulfide)+
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
  aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
  xlim(c(-3,4))+
  ylab("Interaction with \n 750 \u03BCM Sodium Sulfide ")+
  scale_color_manual(values = c("black", "orange"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_blank())

e1_maineffects<- c(
  'agar_typeoxoid_agar' = 'Oxoid agar',
  'post_slad2xSLAD' = '2xSLAD',
  'day6' = 'Day 6',
  'nutrient75' = '75 \u03BCM Ammonium Sulfate',
  'nutrient100' = '100 \u03BCM Ammonium Sulfate',
  "sulfide_amount400" = "400 \u03BCM Sodium Sulfide ",
  "sulfide_amount750" = "750 \u03BCM Sodium Sulfide ")

e1_plot_maineffects <- modelsummary::modelplot(e1_image_model, coef_map = e1_maineffects)+
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
  aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
  xlim(c(-3,4))+
  ylab("Main effects")+
  scale_color_manual(values = c( "orange","black"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggarrange(e1_plot_maineffects,e1_plot_interaction_750sulfide,e1_plot_interaction_400sulfide,
          heights = c(7,5,5),
          ncol = 1, align = "v")

all_parameters <-c(
    'sulfide_amount400:agar_typeoxoid_agar' = 'Sodium Sulfide 400 \u03BCM: Oxoid agar',
    'sulfide_amount400:post_slad2xSLAD' = 'Sodium Sulfide 400 \u03BCM: 2xSLAD',
    'sulfide_amount400:day6' = 'Sodium Sulfide 400 \u03BCM: day6',
    'nutrient75:sulfide_amount400' = 'Sodium Sulfide 400 \u03BCM: Ammonium Sulfate 75 \u03BCM',
    'nutrient100:sulfide_amount400' = 'Sodium Sulfide 400 \u03BCM: Ammonium Sulfate 100 \u03BCM',
    'sulfide_amount750:agar_typeoxoid_agar' = 'Sodium Sulfide 750 \u03BCM: Oxoid agar',
    'sulfide_amount750:post_slad2xSLAD' = 'Sodium Sulfide 750 \u03BCM: 2xSLAD',
    'sulfide_amount750:day6' = 'Sodium Sulfide 750 \u03BCM: day6',
    'nutrient75:sulfide_amount750' = 'Sodium Sulfide 750 \u03BCM: Ammonium Sulfate 75 \u03BCM',
    'nutrient100:sulfide_amount750' = 'Sodium Sulfide 750 \u03BCM: Ammonium Sulfate 100 \u03BCM',
    'agar_typeoxoid_agar' = 'Oxoid agar',
    'post_slad2xSLAD' = '2xSLAD',
    'day6' = 'Day 6',
    'nutrient75' = '75 \u03BCM Ammonium Sulfate',
    'nutrient100' = '100 \u03BCM Ammonium Sulfate',
    "sulfide_amount400" = "400 \u03BCM Sodium Sulfide ",
    "sulfide_amount750" = "750 \u03BCM Sodium Sulfide ",
    "(Intercept)" = "Intercept",
    "(phi)" = "$phi$",
    "Log(nu)" = "$\text{log}(nu)$"
  )

options(modelsummary_get = "all")

modelsummary(e1_image_model,
             coef_map = all_parameters,
             shape = term ~ model + statistic,
             statistic = c("std.error", 
                           "statistic",
                           "p.value"),
             gof_map = "all", 
             output = "latex")



ggsave("images/experiment1_model.pdf", device = cairo_pdf, width = 7, height = 7)


#### Second, third and fourth Experiment : mutants ####

e2_data_image <- full_data_image %>%
  filter(exp_order == "E2")%>%
  mutate(experiment_id = "batch1")

e2_data_image %>%
  group_by(nutrient, day, sulfide_amount, mutant_id)%>%
  summarise(number_images = n()) %>%
  summary()

e2_data_count <- full_data_count %>%
  filter(exp_order == "E2")%>%
  mutate(experiment_id = "batch1")

e3_data_image <- full_data_image %>%
  filter(exp_order == "E3")%>%
  mutate(experiment_id = "batch2")

e3_data_image %>%
  group_by(nutrient, day, sulfide_amount, mutant_id)%>%
  summarise(number_images = n()) %>%
  summary()

e3_data_count <- full_data_count %>%
  filter(exp_order == "E3")%>%
  mutate(experiment_id = "batch2")

nrow(e2_data_count)
skim(e2_data_count)
# All parent yeasts on a pre-slad conditon. 
# All on 2xSlad in post slad condition
# Nitrogen levels of 75 and 50
# Sulfide conditions of 0, 400
# Day conditions of 3 and 6 days
# 8 mutant conditions, 1 parent
# mutants are 1278b, fat1, gup1, L2056, bnrt1, skp2, soa1, yor1
# = a fully factorial design. 
nrow(e3_data_count)
skim(e3_data_count)
# 80 conditions 
# 9 mutants, 1 parent
# nutrient at 40, 75
# sulfur at 0 and 400
# day 4 and day 6
# Full design
e4_data_image <- full_data_image %>%
  filter(exp_order == "E4")

e2_data_image %>%
  group_by(nutrient, day, sulfide_amount, mutant_id, pre_slad)%>%
  summarise(number_images = n()) %>%
  summary()

e4_data_count <- full_data_count %>%
  filter(exp_order == "E4")
# 72 conditions
# 64 conditions on preslad, 8  on 2xslad
# 8 2xslad was test for parent - 4 on day 3 and 4 on day 6
# 7 mutants, 1 parent
# nutrient at 50, 75
# sulfur at 0 and 400
# day 3 (parent only), day 4 (28) day 6(32)
nrow(e4_data_count)
skim(e4_data_count)

e4_data_count_pre_1slad <- e4_data_count %>%
  filter(pre_slad == "1xSLAD") %>%
  mutate(experiment_id = "batch3")

e4_data_image_pre_1slad <- e4_data_image %>%
  filter(pre_slad == "1xSLAD") %>%
  mutate(experiment_id = "batch3")


e2_3_4_parent_count_data <- rbind(e2_data_count, e3_data_count, e4_data_count_pre_1slad) %>% 
  group_by(day,nutrient, mutant_id, sulfide_amount,isparent)%>%
  summarise(n_not_invasive = sum(total_colonies_before_wash) - sum(total_invasive_colonies_after_wash),
         proportion_invasive_colonies = sum(total_invasive_colonies_after_wash)/sum(total_colonies_before_wash))%>%
  ungroup()%>%
  mutate(nutrient_amount = factor(paste(nutrient), levels = c("50", "75"), ordered = TRUE),
         day = paste("Number of days :", day),
         sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
  filter(isparent == TRUE)

 e2_3_4_count_figure <- rbind(e2_data_count, e3_data_count, e4_data_count_pre_1slad)%>%
  mutate(n_not_invasive = total_colonies_before_wash - total_invasive_colonies_after_wash,
         proportion_invasive_colonies = total_invasive_colonies_after_wash/total_colonies_before_wash)%>%
  mutate(nutrient_amount = factor(paste(nutrient), levels = c("50", "75"), ordered = TRUE),
         day = paste("Number of days :", day),
         sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
   filter(isparent == FALSE) %>%
  ggplot(., mapping = aes(x = sulfide_amount, y = proportion_invasive_colonies, colour =  mutant_id))+
  geom_point()+
  geom_line()+
  geom_point(data = e2_3_4_parent_count_data, 
             mapping = aes(x = sulfide_amount, y = proportion_invasive_colonies), colour = "darkgreen")+
   geom_line(data = e2_3_4_parent_count_data, 
              mapping = aes(x = sulfide_amount, y = proportion_invasive_colonies), colour = "darkgreen")+
  #geom_col(position = "dodge", colour = "darkgrey", width = .7)+
  labs(x = "Sodium Sulfide (\u03BCM)", y = "Presence of invasion",
       fill = "Mutant ID")+
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = c(0,400)) +
  facet_grid(.~day*nutrient_amount)+
  viridis::scale_colour_viridis(discrete = TRUE, end = 0, begin = .9, option ="A")+
  theme_bw()+
   theme(legend.position = "bottom",
         strip.text = element_blank())
 
 full_palette <- paletteer::paletteer_d("colorBlindness::paletteMartin")[c(3,4,5,7,8,9,11,13,14,15)]
 
 e2_parent_data_image <- rbind(e2_data_image) %>% 
   mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
          day = paste("Number of days :", day),
          post_slad = paste("Post Slad: ", post_slad),
          sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
   filter(mutant_id == "parent")
 
 e2_image_mutant_figure <- rbind(e2_data_image) %>%  
   mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
          day = paste("Number of days :", day),
          post_slad = paste("Post Slad: ", post_slad),
          sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
   filter(!mutant_id %in% c("parent", "1278b","L2056"))%>%
   mutate(mutant_id = paste(mutant_id))%>%
   ggplot(., aes(y = area_ratio, x = sulfide_amount, colour = mutant_id))+
   geom_jitter(width = 50)+
   stat_summary(fun=mean, geom="line")+
   stat_summary(data = e2_parent_data_image, mapping = aes(y = area_ratio, x = sulfide_amount), colour = "black", fun=mean, geom="line")+
   geom_jitter(width = 50, data = e2_parent_data_image, mapping = aes(y = area_ratio, x = sulfide_amount), colour = "black")+
   facet_grid(.~day*nutrient_amount)+ 
   scale_colour_manual(values = full_palette)+
   ggtitle("Mutant experiment batch 1")+
   ylab("Degree of invasion")+
   xlab("Sodium Sulfide (\u03BCM)")+
   guides( colour = guide_legend(title = "Mutant ID"))+
   theme_bw() + 
   theme(#legend.position = "none",
     plot.title = element_text(hjust = 0.5),
     strip.text.y = element_blank()) 
 e2_image_mutant_figure
 

 e3_parent_data_image <- rbind(e3_data_image) %>% 
   mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
          day = paste("Number of days :", day),
          post_slad = paste("Post Slad: ", post_slad),
          sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
   filter(mutant_id == "parent")
 
 e3_image_mutant_figure <- rbind(e3_data_image) %>%  
   mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
          day = paste("Number of days :", day),
          post_slad = paste("Post Slad: ", post_slad),
          sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
   filter(!mutant_id %in% c("parent", "1278b","L2056"))%>%
   mutate(mutant_id = paste( mutant_id))%>%
   ggplot(., aes(y = area_ratio, x = sulfide_amount, colour = mutant_id))+
   geom_jitter(width = 50)+
   stat_summary(fun=mean, geom="line")+
   stat_summary(data = e3_parent_data_image, mapping = aes(y = area_ratio, x = sulfide_amount), colour = "black", fun=mean, geom="line")+
   geom_jitter(width = 50, data = e3_parent_data_image, mapping = aes(y = area_ratio, x = sulfide_amount), colour = "black")+
   facet_grid(.~day*nutrient_amount)+
   scale_colour_manual(values = full_palette)+
   ggtitle("Mutant experiment batch 2")+
   ylab("Degree of invasion")+
   xlab("Sodium Sulfide (\u03BCM)")+
 guides( colour = guide_legend(title = "Mutant ID"))+
   theme_bw() + 
   theme(#legend.position = "none",
     plot.title = element_text(hjust = 0.5),
     strip.text.y = element_blank()) 
 e3_image_mutant_figure
 
 
 e4_parent_data_image <- rbind(e4_data_image) %>% 
   mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
          day = paste("Number of days :", day),
          post_slad = paste("Post Slad: ", post_slad),
          sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
   filter(mutant_id == "parent")
 
 e4_image_mutant_figure <- rbind(e4_data_image) %>%  
   mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
          day = paste("Number of days :", day),
          post_slad = paste("Post Slad: ", post_slad),
          sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
   filter(!mutant_id %in% c("parent", "1278b","L2056"))%>%
   mutate(mutant_id = paste(mutant_id))%>%
   ggplot(., aes(y = area_ratio, x = sulfide_amount, colour = mutant_id))+
   geom_jitter(width = 50)+
   stat_summary(fun=mean, geom="line")+
   stat_summary(data = e4_parent_data_image, mapping = aes(y = area_ratio, x = sulfide_amount), colour = "black", fun=mean, geom="line")+
   geom_jitter(width = 50, data = e4_parent_data_image, mapping = aes(y = area_ratio, x = sulfide_amount), colour = "black")+
   facet_grid(.~day*nutrient_amount)+
   scale_colour_manual(values = full_palette)+
   ggtitle("Mutant experiment batch 3")+
   ylab("Degree of invasion")+
   xlab("Sodium Sulfide (\u03BCM)")+
   guides( colour = guide_legend(title = "Mutant ID"))+
   theme_bw() + 
   theme(#legend.position = "none",
     plot.title = element_text(hjust = 0.5),
     strip.text.y = element_blank()) 
 e4_image_mutant_figure
 
   
 ggarrange(e2_image_mutant_figure,e3_image_mutant_figure,e4_image_mutant_figure,
           ncol = 1, nrow = 3, align = "v")
 
 ggsave("images/experiment2_3_4_mutants_appendix.pdf", device = cairo_pdf, width = 12, height = 12)
 
e2_3_4_image_model <- rbind(e2_data_image,e3_data_image,e4_data_image_pre_1slad) %>%
  mutate(experiment_id = as.factor(experiment_id)) %>%
  mutate(area_ratio = ifelse(area_ratio == 0,0.001,area_ratio)) %>%
  mutate(mutant_id = relevel(mutant_id, ref = "parent"))%>%
  betareg(area_ratio ~ nutrient + experiment_id + day + sulfide_amount*mutant_id, data = .)
  summary(e2_3_4_image_model)
  
  e2_3_4_interactions_400sulfide_mutant <- c(
    'sulfide_amount400:mutant_idyor1' = "yor1",
    "sulfide_amount400:mutant_idalr2" = 'alr2',
    "sulfide_amount400:mutant_idato3" = 'ato3',
    "sulfide_amount400:mutant_idccz1" = 'ccz1',
    "sulfide_amount400:mutant_idcdh1" = 'cdh1',
    "sulfide_amount400:mutant_idcvt16" = 'cvt16',
    "sulfide_amount400:mutant_iddur3" = 'dur3',
    "sulfide_amount400:mutant_idfat1" = 'fat1',
    "sulfide_amount400:mutant_idfui1" = 'fui1',
    "sulfide_amount400:mutant_idgup1" = 'gup1',
    "sulfide_amount400:mutant_idmid1" = 'mid1',
    "sulfide_amount400:mutant_idmsa1" = 'msa1',
    "sulfide_amount400:mutant_idmsb2" = 'msb2',
    "sulfide_amount400:mutant_idnrt1" = 'nrt1',
    "sulfide_amount400:mutant_idpep12" = 'pep12',
    "sulfide_amount400:mutant_idrps8a" = 'rps8a',
    "sulfide_amount400:mutant_idskp2" = 'skp2', 
    "sulfide_amount400:mutant_idsoa1" = 'soa1',
    "sulfide_amount400:mutant_idtmn3" = 'tmn3',
    "sulfide_amount400:mutant_idtpo4" = 'tpo4',  
    "sulfide_amount400:mutant_idvps28" = 'vps28',
    "sulfide_amount400:mutant_idwhi3" = 'whi3')
  
  e2_3_4_plot_interaction_400sulfide_mutant <- modelsummary::modelplot(e2_3_4_image_model, coef_map = e2_3_4_interactions_400sulfide_mutant)+
    geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
    aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
    xlim(c(-4,4))+
    ylab("Interaction with \n 400 \u03BCM Sodium Sulfide")+
    scale_color_manual(values = c("black", "orange"))+
    theme_bw()+
    theme(legend.position = "none")
  e2_3_4_plot_interaction_400sulfide_mutant
  
  
  e2_3_4_interactions_mutant <- c(
    'mutant_idyor1' = "yor1",
    "mutant_idalr2" = 'alr2',
    "mutant_idato3" = 'ato3',
    "mutant_idccz1" = 'ccz1',
    "mutant_idcdh1" = 'cdh1',
    "mutant_idcvt16" = 'cvt16',
    "mutant_iddur3" = 'dur3',
    "mutant_idfat1" = 'fat1',
    "mutant_idfui1" = 'fui1',
    "mutant_idgup1" = 'gup1',
    "mutant_idmid1" = 'mid1',
    "mutant_idmsa1" = 'msa1',
    "mutant_idmsb2" = 'msb2',
    "mutant_idnrt1" = 'nrt1',
    "mutant_idpep12" = 'pep12',
    "mutant_idrps8a" = 'rps8a',
    "mutant_idskp2" = 'skp2', 
    "mutant_idsoa1" = 'soa1',
    "mutant_idtmn3" = 'tmn3',
    "mutant_idtpo4" = 'tpo4',  
    "mutant_idvps28" = 'vps28',
    "mutant_idwhi3" = 'whi3')
  
  e2_3_4_plot_mutant <- modelsummary::modelplot(e2_3_4_image_model, coef_map = e2_3_4_interactions_mutant)+
    geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
    aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
    xlim(c(-4,4))+
    ylab("Main effect of mutatations")+
    scale_color_manual(values = c("black", "orange"))+
    theme_bw()+
    theme(legend.position = "none",
          axis.title.x = element_blank())
  e2_3_4_plot_mutant
  
  ggarrange(e2_3_4_plot_mutant,
            e2_3_4_plot_interaction_400sulfide_mutant,
            heights = c(10,11),
            ncol = 1, align = "v")
  
  ggsave("images/experiment2_3_4_model_mutants.pdf", device = cairo_pdf, width = 7, height = 6)
  

#### Second, third and fourth experiment: strains ####
  
  e2_fig_count_strain <- e2_data_count %>% 
    filter(mutant_id %in% c("parent","L2056", "1278b"))%>%
    mutate(mutant_id = forcats::fct_recode(mutant_id, `AWRI 796` = "parent"))%>%
    mutate(n_not_invasive = total_colonies_before_wash - total_invasive_colonies_after_wash,
           proportion_invasive_colonies = total_invasive_colonies_after_wash/total_colonies_before_wash)%>%
    mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
           day = paste("Number of days :", day),
           sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
    ggplot(., mapping = aes(x = sulfide_amount, y = proportion_invasive_colonies, fill = mutant_id, group = mutant_id))+
    geom_col(position = position_dodge2(preserve = "single"))+
    #geom_col(position = "dodge", colour = "darkgrey", width = .7)+
    labs(x = "Sodium Sulfide (\u03BCM)", y = "Presence of invasion",
         fill = "Strain")+
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(breaks = c(0,400)) +
    facet_grid(.~day*nutrient_amount)+
    viridis::scale_fill_viridis(discrete = TRUE, end = .3, begin = .7, option ="B")+
    theme_bw()+
    theme(legend.position = "bottom",
          strip.text = element_blank())
  
  e2_fig_image_strain <- e2_data_image %>% 
    filter(mutant_id %in% c("parent","L2056", "1278b"))%>%
    mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
           day = paste("Number of days :", day),
           post_slad = paste("Post Slad: ", post_slad),
           sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
    mutate(mutant_id = forcats::fct_recode(mutant_id, Parent = "parent"))%>%
    ggplot(., aes(y = area_ratio, x = sulfide_amount, colour = mutant_id))+
    stat_summary(fun=mean, geom="line")+
    geom_jitter(alpha = .7, width = 5, mapping = aes(colour = mutant_id))+
    facet_grid(.~day*nutrient_amount)+
    ylab("Degree of invasion")+
    viridis::scale_colour_viridis(discrete = TRUE, end = .3, begin = .7, option ="B")+
    guides(fill = "none", colour = guide_legend(title="Ammonium Sulfate (\u03BCM)"),
           shape = guide_legend(title="Ammonium Sulfate (\u03BCM)"))+
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          strip.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) 
  
  ggarrange(e2_fig_image_strain,e2_fig_count_strain, heights = c(.90, 1),
            ncol = 1, nrow = 2, align = "v")
  
  ggsave("images/experiment2_strains.pdf", device = cairo_pdf, width = 12, height = 6)
  


e2_3_4_strain <- c("mutant_id1278b" = '\U03A3 1278b',
  "mutant_idL2056" = 'L2056')

e2_3_4_plot_altparent <- modelsummary::modelplot(e2_3_4_image_model, coef_map = e2_3_4_strain)+
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
  aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
  xlim(c(-4,4))+
  ylab("Main effect of \n alternative strains")+
  scale_color_manual(values = c("orange", "black"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_blank())
e2_3_4_plot_altparent

e2_3_4_strain_interaction <- c("sulfide_amount400:mutant_id1278b" = '\U03A3 1278b',
                 "sulfide_amount400:mutant_idL2056" = 'L2056')

e2_3_4_plot_altparent_interaction <- modelsummary::modelplot(e2_3_4_image_model, coef_map = e2_3_4_strain_interaction)+
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
  aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
  xlim(c(-4,4))+
  ylab("Interaction with \n 400 \u03BCM Sodium Sulfide")+
  scale_color_manual(values = c( "black", "orange"))+
  theme_bw()+
  theme(legend.position = "none")
e2_3_4_plot_altparent_interaction

ggarrange(e2_3_4_plot_altparent,
  e2_3_4_plot_altparent_interaction,
  ncol = 1, align = "v")

ggsave("images/experiment2_3_4_model_alternativestrains.pdf",device = cairo_pdf, width = 7, height = 4)

e2_3_4_main_effects <- c(
  "nutrient75" = "75 \u03BCM Ammonium Sulfate",
  "day6" = "Day 6",
  "day4" = "Day 4",
  "experiment_idbatch3" = "Third batch",
  "experiment_idbatch2" = "Second batch",
  "sulfide_amount400" = "400 \u03BCM Sodium Sulfide "
)

e2_3_4_plot_maineffects <- modelsummary::modelplot(e2_3_4_image_model, coef_map = e2_3_4_main_effects)+
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
  aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
  xlim(c(-2,3))+
  ylab("Other experimental conditions")+
  scale_color_manual(values = c("black", "orange"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_blank())
e2_3_4_plot_maineffects

ggsave("images/experiment2_3_4_model_maineffects.pdf", device = cairo_pdf, width = 7, height = 3)

e2_3_4_all_parameters <-c(
  'sulfide_amount400:mutant_idyor1' = "Sodium Sulfide 400 \u03BCM: yor1",
  "sulfide_amount400:mutant_idalr2" = 'Sodium Sulfide 400 \u03BCM: u0394alr2',
  "sulfide_amount400:mutant_idato3" = 'Sodium Sulfide 400 \u03BCM: ato3',
  "sulfide_amount400:mutant_idccz1" = 'Sodium Sulfide 400 \u03BCM: ccz1',
  "sulfide_amount400:mutant_idcdh1" = 'Sodium Sulfide 400 \u03BCM: cdh1',
  "sulfide_amount400:mutant_idcvt16" = 'Sodium Sulfide 400 \u03BCM: cvt16',
  "sulfide_amount400:mutant_iddur3" = 'Sodium Sulfide 400 \u03BCM: dur3',
  "sulfide_amount400:mutant_idfat1" = 'Sodium Sulfide 400 \u03BCM: fat1',
  "sulfide_amount400:mutant_idfui1" = 'Sodium Sulfide 400 \u03BCM: fui1',
  "sulfide_amount400:mutant_idgup1" = 'Sodium Sulfide 400 \u03BCM: gup1',
  "sulfide_amount400:mutant_idmid1" = 'Sodium Sulfide 400 \u03BCM: mid1',
  "sulfide_amount400:mutant_idmsa1" = 'Sodium Sulfide 400 \u03BCM: msa1',
  "sulfide_amount400:mutant_idmsb2" = 'Sodium Sulfide 400 \u03BCM: msb2',
  "sulfide_amount400:mutant_idnrt1" = 'Sodium Sulfide 400 \u03BCM: nrt1',
  "sulfide_amount400:mutant_idpep12" = 'Sodium Sulfide 400 \u03BCM: pep12',
  "sulfide_amount400:mutant_idrps8a" = 'Sodium Sulfide 400 \u03BCM: rps8a',
  "sulfide_amount400:mutant_idskp2" = 'Sodium Sulfide 400 \u03BCM: skp2', 
  "sulfide_amount400:mutant_idsoa1" = 'Sodium Sulfide 400 \u03BCM: soa1',
  "sulfide_amount400:mutant_idtmn3" = 'Sodium Sulfide 400 \u03BCM: tmn3',
  "sulfide_amount400:mutant_idtpo4" = 'Sodium Sulfide 400 \u03BCM: tpo4',  
  "sulfide_amount400:mutant_idvps28" = 'Sodium Sulfide 400 \u03BCM: vps28',
  "sulfide_amount400:mutant_idwhi3" = 'Sodium Sulfide 400 \u03BCM: whi3',
  "sulfide_amount400:mutant_id1278b" = 'Sodium Sulfide 400 \u03BCM: \U03A3 1278b',
  "sulfide_amount400:mutant_idL2056" = 'Sodium Sulfide 400 \u03BCM: L2056',
  'mutant_idyor1' = "yor1",
  "mutant_idalr2" = 'alr2',
  "mutant_idato3" = 'ato3',
  "mutant_idccz1" = 'ccz1',
  "mutant_idcdh1" = 'cdh1',
  "mutant_idcvt16" = 'cvt16',
  "mutant_iddur3" = 'dur3',
  "mutant_idfat1" = 'fat1',
  "mutant_idfui1" = 'fui1',
  "mutant_idgup1" = 'gup1',
  "mutant_idmid1" = 'mid1',
  "mutant_idmsa1" = 'msa1',
  "mutant_idmsb2" = 'msb2',
  "mutant_idnrt1" = 'nrt1',
  "mutant_idpep12" = 'pep12',
  "mutant_idrps8a" = 'rps8a',
  "mutant_idskp2" = 'skp2', 
  "mutant_idsoa1" = 'soa1',
  "mutant_idtmn3" = 'tmn3',
  "mutant_idtpo4" = 'tpo4',  
  "mutant_idvps28" = 'vps28',
  "mutant_idwhi3" = 'whi3',
  "mutant_id1278b" = '\U03A3 1278b',
  "mutant_idL2056" = 'L2056',
  "nutrient75" = "75 \u03BCM Ammonium Sulfate",
  "day6" = "Day 6",
  "day4" = "Day 4",
  "experiment_idbatch3" = "Third batch",
  "experiment_idbatch2" = "Second batch",
  "sulfide_amount400" = "400 \u03BCM Sodium Sulfide ",
  "(Intercept)" = "Intercept",
  "(phi)" = "\u03C6"
)

options(modelsummary_get = "all")

modelsummary(e2_3_4_image_model,
             coef_map = e2_3_4_all_parameters,
             shape = term ~ model + statistic,
             statistic = c("std.error", 
                           "statistic",
                           "p.value"),
             gof_map = "all", 
             output = "latex")

 


#### Experiment 4 ####
  e4_data_image_parent <- full_data_image %>%
     filter(exp_order == "E4") %>%
     filter(mutant_id == "parent")
   
  e4_data_count_parent <- full_data_count %>%
     filter(exp_order == "E4") %>%
    filter(mutant_id == "parent")
  skim(e4_data_count_parent)
  table(e4_data_count_parent$pre_slad, e4_data_count_parent$day)
  e4_data_count_parent %>%
    filter(day == 4, pre_slad == "1xSLAD") %>%
    View()
   # 72 conditions
   # 64 conditions on preslad, 8  on 2xslad
   # 8 2xslad was test for parent - 4 on day 3 and 4 on day 6
   # 7 mutants, 1 parent
   # nutrient at 50, 75
   # sulfur at 0 and 400
   # day 3 (parent only), day 4 (28) day 6(32)
  
  
  
  
  
  e4_fig_count_parent <- e4_data_count_parent %>% 
     mutate(n_not_invasive = total_colonies_before_wash - total_invasive_colonies_after_wash,
            proportion_invasive_colonies = total_invasive_colonies_after_wash/total_colonies_before_wash)%>%
     mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
            day = paste("Number of days :", day),
            sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
     ggplot(., mapping = aes(x = sulfide_amount, y = proportion_invasive_colonies, fill = pre_slad, group = pre_slad))+
    geom_col(position = position_dodge2(preserve = "single"))+
      #geom_col(position = "dodge", colour = "darkgrey", width = .7)+
     labs(x = "Sodium Sulfide (\u03BCM)", y = "Presence of invasion",
          fill = "Pre Slad")+
     scale_y_continuous(limits = c(0,1)) +
     scale_x_continuous(breaks = c(0,400)) +
     facet_grid(.~day*nutrient_amount)+
     viridis::scale_fill_viridis(discrete = TRUE, end = .3, begin = .7, option ="B")+
     theme_bw()+
     theme(legend.position = "bottom",
           strip.text = element_blank())

  e4_fig_image_parent <- e4_data_image_parent %>% 
     mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
            day = paste("Number of days :", day),
            post_slad = paste("Post Slad: ", post_slad),
            sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%

     ggplot(., aes(y = area_ratio, x = sulfide_amount, colour = pre_slad))+
     stat_summary(fun=mean, geom="line")+
     geom_jitter(alpha = .7, width = 5, mapping = aes(colour = pre_slad))+
     facet_grid(.~day*nutrient_amount)+
     ylab("Degree of invasion")+
     viridis::scale_colour_viridis(discrete = TRUE, end = .3, begin = .7, option ="B")+
     guides(fill = "none", colour = guide_legend(title="Ammonium Sulfate (\u03BCM)"),
            shape = guide_legend(title="Ammonium Sulfate (\u03BCM)"))+
     theme_bw() +
     theme(legend.position = "none",
           plot.title = element_text(hjust = 0.5),
           axis.title.x = element_blank(),
           strip.text.y = element_blank(),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank()) 
   
  ggarrange(e4_fig_image_parent,e4_fig_count_parent, heights = c(.90, 1),
            ncol = 1, nrow = 2, align = "v")
  
  ggsave("images/experiment4.pdf", device = cairo_pdf, width = 12, height = 6)
  
   
   e4_image_model <- e4_data_image_parent %>%
     filter(mutant_id == "parent")%>%
     mutate(area_ratio = ifelse(area_ratio == 0,0.01,area_ratio)) %>%
     betareg(area_ratio ~ nutrient + day + sulfide_amount*pre_slad, data = ., dist = "beta")
   summary(e4_image_model)

   all_parameters_exp4 <-c(
     'sulfide_amount400:pre_slad2xSLAD' = 'Sodium Sulfide 400 \u03BCM with pre-2xSLAD',
     'pre_slad2xSLAD' = 'pre-2xSLAD',
     'day6' = 'Day 6',
     'day4' = "Day 4",
     'nutrient75' = '75 \u03BCM Ammonium Sulfate',
     "sulfide_amount400" = "400 \u03BCM Sodium Sulfide "
   )
   
   
   modelsummary::modelplot(e4_image_model, coef_map = all_parameters_exp4)+
     geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
     aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
     scale_color_manual(values = c("orange","black"))+
     theme_bw()+
     theme(legend.position = "none",
           panel.grid.major.y = element_line(color = "lightgrey", size = 0.5))
   
   ggsave("images/experiment4_model.pdf", device = cairo_pdf, width = 6, height = 3)  
    
   
   all_parameters_exp4 <-c(
     'sulfide_amount400:pre_slad2xSLAD' = 'Sodium Sulfide 750 \u03BCM: pre-2xSLAD',
     'pre_slad2xSLAD' = 'pre-2xSLAD',
     'day6' = 'Day 6',
     'day4' = "Day 4",
     'nutrient75' = '75 \u03BCM Ammonium Sulfate',
     "sulfide_amount400" = "400 \u03BCM Sodium Sulfide ",
     "(Intercept)"= "Intercept",
     "(phi)" = "phi"
   )
   
   
   options(modelsummary_get = "all")
   
   modelsummary(e4_image_model,
                coef_map = all_parameters_exp4,
                shape = term ~ model + statistic,
                statistic = c("std.error", 
                              "statistic",
                              "p.value"),
                gof_map = "all", 
                output = "latex")
   
   
   #### Experiment 5 ####
   e5_data_image <- full_data_image %>%
     filter(exp_order == "E5")
   
   e5_data_image %>%
     group_by(nutrient, day, sulfide_amount, mutant_id)%>%
     summarise(number_images = n()) %>%
     summary()
   
   e5_data_count <- full_data_count %>%
     filter(exp_order == "E5")
   # 72 conditions
   # all conditions on 2xslad
   # 8 mutants, 1 parent
   # nutrient at 50, 75
   # sulfur at 0 and 400
   # day 4 and 6
   # fully factorial
   nrow(e5_data_count)
   skim(e5_data_count)
   
   e5_parent_count_data <- e5_data_count %>% 
     mutate(n_not_invasive = total_colonies_before_wash - total_invasive_colonies_after_wash,
            proportion_invasive_colonies = total_invasive_colonies_after_wash/total_colonies_before_wash)%>%
     mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
            day = paste("Number of days :", day),
            sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
     filter(isparent == TRUE)
   
   e5_count_figure <- e5_data_count %>% 
     mutate(n_not_invasive = total_colonies_before_wash - total_invasive_colonies_after_wash,
            proportion_invasive_colonies = total_invasive_colonies_after_wash/total_colonies_before_wash)%>%
     mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
            day = paste("Number of days :", day),
            sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
     filter(isparent == FALSE) %>%
     ggplot(., mapping = aes(x = sulfide_amount, y = proportion_invasive_colonies, colour =  mutant_id))+
     geom_point(position = position_dodge(width = 100))+
     geom_line(position = position_dodge(width =100))+
     geom_point(data = e5_parent_count_data, 
                mapping = aes(x = sulfide_amount, y = proportion_invasive_colonies), colour = "darkmagenta")+
     geom_line(data = e5_parent_count_data, 
               mapping = aes(x = sulfide_amount, y = proportion_invasive_colonies), colour = "darkmagenta")+
     #geom_col(position = "dodge", colour = "darkgrey", width = .7)+
     labs(x = "Sodium Sulfide (\u03BCM)", y = "Presence of invasion",
          fill = "Mutant ID")+
     scale_y_continuous(limits = c(0,1)) +
     scale_x_continuous(breaks = c(0,400)) +
     facet_grid(.~day*nutrient_amount)+
     viridis::scale_colour_viridis(discrete = TRUE, end = 0, begin = .9, option ="G")+
     theme_bw()+
     theme(legend.position = "bottom",
           strip.text = element_blank()) 
   
   
   e5_parent_data_image <- e5_data_image %>% 
     mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
            day = paste("Number of days :", day),
            post_slad = paste("Post Slad: ", post_slad),
            sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
     filter(mutant_id == "parent")
   
   e5_image_figure <-  rbind(e5_data_image) %>%  
     mutate(nutrient_amount = factor(paste("Ammonium Sulfate (\u03BCM):", nutrient), levels = c("Ammonium Sulfate (\u03BCM): 50", "Ammonium Sulfate (\u03BCM): 75"), ordered = TRUE),
            day = paste("Number of days :", day),
            post_slad = paste("Post Slad: ", post_slad),
            sulfide_amount = as.numeric(as.character(sulfide_amount)))%>%
     filter(!mutant_id %in% c("parent", "1278b","L2056"))%>%
     mutate(mutant_id = paste(mutant_id))%>%
     ggplot(., aes(y = area_ratio, x = sulfide_amount, colour = mutant_id))+
     geom_jitter(width = 50)+
     stat_summary(fun=mean, geom="line")+
     stat_summary(data = e5_parent_data_image, mapping = aes(y = area_ratio, x = sulfide_amount), colour = "black", fun=mean, geom="line")+
     geom_jitter(width = 50, data = e5_parent_data_image, mapping = aes(y = area_ratio, x = sulfide_amount), colour = "black")+
     facet_grid(.~day*nutrient_amount)+ 
     scale_colour_manual(values = full_palette)+
     ylab("Degree of invasion")+
     xlab("Sodium Sulfide (\u03BCM)")+
     guides( colour = guide_legend(title = "Mutant ID"))+
     theme_bw() + 
     theme(#legend.position = "none",
       plot.title = element_text(hjust = 0.5),
       strip.text.y = element_blank()) 
   e5_image_figure
   
   ggsave("images/experiment5.pdf", device = cairo_pdf, width = 12, height = 5)
   
   e5_image_model <- e5_data_image %>%
     mutate(area_ratio = ifelse(area_ratio == 0,0.001,area_ratio)) %>%
     betareg(area_ratio ~ day * nutrient + sulfide_amount*mutant_id, data = .)
   
   
   e5_main_effects <- c(
     "nutrient75" = "75 \u03BCM Ammonium Sulfate",
     "day6" = "Day 6",
     "sulfide_amount400" = "400 \u03BCM Sodium Sulfide "
   )
   e5_plot_maineffects <- modelsummary::modelplot(e5_image_model, coef_map = e5_main_effects)+
     geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
     aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
     xlim(c(-5,2.5))+
     ylab("Main effects")+
     scale_color_manual(values = c("black", "orange"))+
     theme_bw()+
     theme(legend.position = "none",
           axis.title.x = element_blank())
   e5_plot_maineffects
   
   e5_interactions_400sulfide_mutant <- c(
     'sulfide_amount400:mutant_idyhl008c' = "yhl008c",
     "sulfide_amount400:mutant_idtmn3" = 'tmn3',
     "sulfide_amount400:mutant_idput4" = 'put4',
     "sulfide_amount400:mutant_idpma2" = 'pma2',
     "sulfide_amount400:mutant_idnrt1" = 'nrt1',
     "sulfide_amount400:mutant_iddur3" = 'dur3',
     "sulfide_amount400:mutant_idalr2" = 'alr2',
     "sulfide_amount400:mutant_idsac3" = 'sac3')
   
   e5_plot_interaction_400sulfide_mutant <- modelsummary::modelplot(e5_image_model, coef_map = e5_interactions_400sulfide_mutant)+
     geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
     aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
     xlim(c(-5,2.5))+
     ylab("Interaction with \n 400 \u03BCM Sodium Sulfide")+
     scale_color_manual(values = c("black", "orange"))+
     theme_bw()+
     theme(legend.position = "none")
   e5_plot_interaction_400sulfide_mutant
   
   
   e5_mutant <-  c(
     'mutant_idyhl008c' = "yhl008c",
     "mutant_idtmn3" = 'tmn3',
     "mutant_idput4" = 'put4',
     "mutant_idpma2" = 'pma2',
     "mutant_idnrt1" = 'nrt1',
     "mutant_iddur3" = 'dur3',
     "mutant_idalr2" = 'alr2',
     "mutant_idsac3" = 'sac3')
   
   e5_plot_mutant <- modelsummary::modelplot(e5_image_model, coef_map = e5_mutant)+
     geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
     aes(color = ifelse(p.value < 0.05, "Significant at 5%", "Not significant")) +
     xlim(c(-5,2.5))+
     ylab("Main effect of mutatations")+
     scale_color_manual(values = c("black", "orange"))+
     theme_bw()+
     theme(legend.position = "none",
           axis.title.x = element_blank())
   e5_plot_mutant

   ggarrange(e5_plot_maineffects,
             e5_plot_mutant,
             e5_plot_interaction_400sulfide_mutant,
             heights = c(3,8,8),
             ncol = 1, align = "v")

   ggsave("images/experiment5_model.pdf", device = cairo_pdf, width = 7, height = 7)  
   
   
   e5_all_parameters <-c(
     'sulfide_amount400:mutant_idyhl008c' = "Sodium Sulfide 400 \u03BCM: yhl008c",
     "sulfide_amount400:mutant_idtmn3" = 'Sodium Sulfide 400 \u03BCM: u0394tmn3',
     "sulfide_amount400:mutant_idput4" = 'Sodium Sulfide 400 \u03BCM: put4',
     "sulfide_amount400:mutant_idpma2" = 'Sodium Sulfide 400 \u03BCM: pma2',
     "sulfide_amount400:mutant_idnrt1" = 'Sodium Sulfide 400 \u03BCM: nrt1',
     "sulfide_amount400:mutant_iddur3" = 'Sodium Sulfide 400 \u03BCM: dur3',
     "sulfide_amount400:mutant_idalr2" = 'Sodium Sulfide 400 \u03BCM: alr2',
     "sulfide_amount400:mutant_idsac3" = 'Sodium Sulfide 400 \u03BCM: sac3',
     'mutant_idyhl008c' = "yhl008c",
     "mutant_idtmn3" = 'u0394tmn3',
     "mutant_idput4" = 'put4',
     "mutant_idpma2" = 'pma2',
     "mutant_idnrt1" = 'nrt1',
     "mutant_iddur3" = 'dur3',
     "mutant_idalr2" = 'alr2',
     "mutant_idsac3" = 'sac3',
     "nutrient75" = "75 \u03BCM Ammonium Sulfate",
     "day6" = "Day 6",
     "sulfide_amount400" = "400 \u03BCM Sodium Sulfide ",
     "(Intercept)" = "Intercept",
     "(phi)" = "\u03C6"
   )
   
   options(modelsummary_get = "all")
   
   modelsummary(e5_image_model,
                coef_map = e5_all_parameters,
                shape = term ~ model + statistic,
                statistic = c("std.error", 
                              "statistic",
                              "p.value"),
                gof_map = "all", 
                output = "latex")
   
  