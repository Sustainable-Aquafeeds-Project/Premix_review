library(tidyverse)
library(janitor)
library(here)
library(ggpubr)
library(waffle)




full_data <-  
  read_csv("Article review details - Accepted papers- incl trace mineral premix_2_v24-09-04.csv") %>%  
  clean_names() %>% 
  mutate(animal_type = case_when(animal_type == "pig" ~ "Pig",
                                 TRUE ~ animal_type)) %>%  
  mutate(animal_type = case_when(is.na(animal_type) & animal == "Rainbow trout" ~ "Fish/Crustaceans",
                                 animal == "Dairy Cattle" ~ "Cattle",
                                 is.na(animal_type) ~ animal,
                                 TRUE ~ animal_type)) %>% 
  mutate(mineral_premix_included_y_n = case_when(mineral_premix_included_y_n == "?" ~ "N",
                                                 is.na(mineral_premix_included_y_n) ~ "N",
                                                 TRUE ~ mineral_premix_included_y_n)) %>% 
  mutate(inclusion = case_when(mineral_premix_included_y_n == "Y" & proxy_y_n == "N" ~ "Included",
                               mineral_premix_included_y_n == "Y" & proxy_y_n == "Y" ~  "Included as proxy",
                               TRUE ~ "Omitted"))

#Check unique animal types
(animal_types <- full_data %>%  pull(animal_type) %>%  unique())


#check for NAs
full_data %>% 
  filter(is.na(animal_type))


summary_by_spp <- 
  full_data %>% 
  group_by(animal_type, mineral_premix_included_y_n) %>% 
  count()


ggplot(data = summary_by_spp)+
  aes(x = animal_type, y = n, fill = mineral_premix_included_y_n )+
  xlab("Animal class")+
  ylab("Number of studies")+
  scale_fill_discrete(name = "Is mineral premix included?", labels = c("No", "Yes"))+
  geom_col()+
  theme_bw()


summary_by_inclusion <- 
  full_data |> 
  group_by(animal_type, inclusion) |> 
  count() |> 
  ungroup() |> 
  group_by(animal_type) |> 
  nest() |> 
  mutate(total = map(data, ~(sum(.$n)))) |> 
  unnest(c(data, total)) |> 
  ungroup() |> 
  mutate(prop = n/total)


##This plot shows what proportion of studies for each animal type uses a proxy, includes the premix fully or omits the premix
ggplot(data = summary_by_inclusion |> mutate(inclusion = factor(inclusion, levels = )))+
  aes(x = inclusion, y = prop, fill = animal_type )+
  ylab("Proportion")+
  xlab("Inclusion")+
  scale_fill_discrete(name = "Animal type", labels = c("Cattle", "Chicken", "Fish/Crustaceans", "Pig"))+
  geom_col(position = "dodge")+
  theme_bw()

##########################

just_proxy2<-subset(full_data,proxy_y_n=="Y")

colnames(just_proxy2)

summary_by_proxy <- 
  just_proxy2 |> 
  mutate(proxy_name = fct_collapse(proxy_name,
                                   'Sodium chloride'='Salt (sodium chloride)',
                                   'Calcium carbonate'=c('Limestone','Limestone (calcium carbonate)', 'Limestone is assumed to be a proxy for mineral premix listed in feed composition data'))
  )|>
  group_by(proxy_name) |> 
  count()
         
         
 

  unique(summary_by_proxy$proxy_name)


##This makes a waffle plot showing the proxies used
ggplot(data=summary_by_proxy,
aes(fill=proxy_name, values=n)) +
geom_waffle(n_rows=5) +
theme_void()+
  labs(fill="Proxy")





