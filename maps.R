library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(DescTools)
library(sf)
library(tmap)


### data import
df = read_csv("output_data/clean_mig_pop.csv")
geo_bezirk = st_read("output_data/geo_bez.geojson")
geo_zählbezirk = st_read("output_data/geo_zbez.geojson")



#add migration as percentage of current population

year_bezirk_sex_nat = df %>% group_by(year, bezirk, sex, nationality) %>% 
  summarize(net = sum(net), ext_net = sum(ext_net), int_net = sum(int_net), local_net = sum(local_net), local_internal = sum(local_internal),
            net_p = sum(net_p), ext_net_p = sum(ext_net_p), int_net_p = sum(int_net_p), local_net_p = sum(local_net_p), local_internal_p = sum(local_internal_p))
head(year_bezirk_sex_nat)

#average yearly net migration from 2007 to 2023 per nationality
bezirk_nat = year_bezirk_sex_nat %>% group_by(bezirk, nationality) %>% 
  summarize(net = mean(net), ext_net = mean(ext_net), int_net = mean(int_net), local_net = mean(local_net), local_internal = mean(local_internal),
            net_p = mean(net_p), ext_net_p = mean(ext_net_p), int_net_p = mean(int_net_p), local_net_p = mean(local_net_p), local_internal_p = mean(local_internal_p))

bezirk_nat$bezirk = as.factor(bezirk_nat$bezirk)
head(bezirk_nat)

bezirk_election = bezirk_nat %>% inner_join(election, by = c("bezirk" = "bezirk"))
head(bezirk_election)

aut_mig = bezirk_election %>% filter(nationality == "Austrian")
for_mig = bezirk_election %>% filter(nationality == "Foreign")
head(aut_mig)
head(for_mig)

### maps

tot_mig = bezirk_election %>% inner_join(geo_bezirk, by = c("bezirk" = "bezirk"))
aut_mig = aut_mig %>% inner_join(geo_bezirk, by = c("bezirk" = "bezirk"))
for_mig = for_mig %>% inner_join(geo_bezirk, by = c("bezirk" = "bezirk"))
tot_mig = st_as_sf(tot_mig)
aut_mig = st_as_sf(aut_mig)
for_mig = st_as_sf(for_mig)



## migration as pure number
ggplot(tot_mig) + 
  geom_sf(aes(fill = net), color = "black") +  # Fill polygons by 'net' and outline with black
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                         name = "Net Migration") +  # Color gradient for negative/positive values
    labs(title = "Average Yearly Net Migration by Bezirk from 2007 to 2023") +
    theme_minimal() + 
    theme(axis.text = element_blank(),  
          axis.ticks = element_blank(), 
          panel.grid = element_blank())

ggplot(aut_mig) +
  geom_sf(aes(fill = net), color = "black") +  # Fill polygons by 'net' and outline with black
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Net Migration") +  # Color gradient for negative/positive values
  labs(title = "Average Yearly Net Austrian Migration by Bezirk from 2007 to 2023") +
  theme_minimal() + 
  theme(axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        panel.grid = element_blank())

ggplot(for_mig) +
  geom_sf(aes(fill = net), color = "black") +  # Fill polygons by 'net' and outline with black
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Net Migration") +  # Color gradient for negative/positive values
  labs(title = "Average Yearly Net Foreign Migration by Bezirk from 2007 to 2023") +
  theme_minimal() + 
  theme(axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        panel.grid = element_blank())

ggplot(tot_mig) +
  geom_sf(aes(fill = local_net), color = "black") +  # Fill polygons by 'net' and outline with black
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Net Migration") +  # Color gradient for negative/positive values
  labs(title = "Average Yearly Net Between Bezirk Migration from 2007 to 2023") +
  theme_minimal() + 
  theme(axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        panel.grid = element_blank())

ggplot(aut_mig) +
  geom_sf(aes(fill = local_net), color = "black") +  # Fill polygons by 'net' and outline with black
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Net Migration") +  # Color gradient for negative/positive values
  labs(title = "Average Yearly Austrian Net Between Bezirk Migration in Vienna from 2007 to 2023") +
  theme_minimal() + 
  theme(axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        panel.grid = element_blank())

ggplot(for_mig) +
  geom_sf(aes(fill = local_net), color = "black") +  # Fill polygons by 'net' and outline with black
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Net Migration") +  # Color gradient for negative/positive values
  labs(title = "Average Yearly Foreign Between Bezirk Migration in Vienna from 2007 to 2023") +
  theme_minimal() + 
  theme(axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        panel.grid = element_blank())


## migration as percentage
ggplot(tot_mig) +
  geom_sf(aes(fill = net_p), color = "black") + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Percent Net Migration") +  
  labs(title = "Average yearly percent net migration 2007 - 2023") +
  theme_minimal() + 
  theme(axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        panel.grid = element_blank())


ggplot(aut_mig) +
  geom_sf(aes(fill = net_p), color = "black") + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Net Migration (%)") +  
  labs(title = "Average yearly Austrian net migration") +
  theme_minimal() + 
  theme(axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        panel.grid = element_blank())


ggplot(for_mig) +
  geom_sf(aes(fill = net_p), color = "black") + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Net Migration (%)") +  
  labs(title = "Average yearly net Foreign migration") +
  theme_minimal() + 
  theme(axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        panel.grid = element_blank())


ggplot(tot_mig) +
  geom_sf(aes(fill = local_net_p), color = "black") + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Net Migration (%)") +  
  labs(title = "Average yearly between Bezirk net migration") +
  theme_minimal() + 
  theme(axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        panel.grid = element_blank())


ggplot(aut_mig) +
  geom_sf(aes(fill = local_net_p), color = "black") + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Percent Net Migration") +  
  labs(title = "Average yearly Austrian percent between Bezirk net migration 2007 - 2023") +
  theme_minimal() + 
  theme(axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        panel.grid = element_blank())


ggplot(for_mig) +
  geom_sf(aes(fill = local_net_p), color = "black") + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Percent Net Migration") +  
  labs(title = "Average yearly Foreign percent between Bezirk net migration 2007 - 2023") +
  theme_minimal() + 
  theme(axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        panel.grid = element_blank())