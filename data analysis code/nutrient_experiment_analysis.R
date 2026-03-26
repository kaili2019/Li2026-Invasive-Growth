library(tidyverse)
library(here)
nutrient_dataset <- read_csv(here("data/filamentous_yeast_v5.csv"))%>%
  mutate(nutrient_level = extract_numeric(nutrient))

image_figure <- nutrient_dataset %>%
  mutate(nutrient_level = ordered(nutrient_level))%>%
  ggplot(., aes(x= nutrient_level, y = area_ratio, colour = nutrient_level))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_jitter()+
  xlab("Nutrient Level")+
  ylim(c(0,1))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())


count_figure<-  nutrient_dataset %>%
  drop_na() %>%
  mutate(nutrient_level = ordered(nutrient_level))%>%
  group_by(nutrient_level)%>%
  mutate(n_not_invasive = sum(area_ratio == 0),
         proportion_invasive_colonies = sum(area_ratio != 0, na.rm = T)/n()) %>%
  ggplot(., mapping = aes(x = nutrient_level, y = proportion_invasive_colonies, fill = nutrient_level))+
  geom_col(position = "dodge", colour = "darkgrey", width = .7)+
  labs(x = "Nutrient Level", y = "Proportion of colonies with \n any invasive growth")+
  scale_y_continuous(limits = c(0,1)) +
  theme_bw()+
  theme(legend.position = "none",
        strip.text = element_blank())

ggarrange(count_figure,image_figure,
          ncol = 2, nrow = 1, widths = c(1, .8))

ggsave("images/experiment0_nutrient.png", width = 10, height = 7)
