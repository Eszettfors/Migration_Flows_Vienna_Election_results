library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(DescTools)
library(sf)
library(tmap)
library(corrplot)

df = read_delim(
  "data/vie-zbz-pop-sex-nat-geo2-mig-2007f.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
) #must remove the header from the csv file in text editor before importing

nrow(df) #18564 data points
ncol(df) #18 variables

### preping dataset ----
#remove unnecessary columns and rename columns
df = subset(df, select = -c(NUTS, REF_DATE))
colnames(df) = c(
  "bezirk",
  "zählbezirk",
  "year",
  "sex",
  'nationality',
  'net',
  'ext_net',
  'ext_in',
  'ext_out',
  'int_net',
  'int_in',
  'int_out',
  'local_net',
  'local_in',
  'local_out',
  'local_internal'
)

#adjust datatypes
df = df %>% mutate(
  bezirk = as.character(bezirk),
  zählbezirk = as.character(zählbezirk),
  sex = as.character(sex)
)

#adjust values for interpretability
df = df %>% mutate(
  bezirk = substr(bezirk, 2, 3),
  zählbezirk = substr(zählbezirk, 2, 5),
  sex = case_when(sex == 1 ~ "male",
                  sex == 2 ~ "female"),
  nationality = case_when(nationality == "AUT" ~ "Austrian",
                          nationality == "FOR" ~ "Foreign")
)

#setting categorical variable as factors
df = df %>% mutate(
  bezirk = as.factor(as.numeric(bezirk)),
  zählbezirk = as.factor(as.numeric(zählbezirk)),
  sex = as.factor(sex),
  nationality = as.factor(nationality)
)


##sanity check ----

attach(df)
na_tab = table(is.na(df))
print(na_tab) #no missing values

unique(bezirk) # 23 bezirks, checks out
unique(zählbezirk) # zählbezriks from 01 -> 32; 99???? - also wasn't present in geodat

#total net should equal sum of internal and external net
table(net == ext_net + int_net) #checks out

#net external and internal migration should equal in - out
table(ext_net == ext_in - ext_out) # checks out
table(int_net == int_in - int_out) # checks out
table(local_net == local_in - local_out) # checks out


##adding population data ----
pop = read_delim(
  "data/vie-bez-pop-sex-stk-1869f.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
pop = pop %>% select(DISTRICT_CODE, REF_YEAR, POP_TOTAL)
colnames(pop) = c("bezirk", "year", "bezirk_pop")
pop$bezirk = as.factor(as.numeric(substr(as.character(pop$bezirk), 2, 3)))
pop = pop %>% filter(year > 2006)

df = df %>% inner_join(pop, by = c("bezirk" = "bezirk", "year" = "year"))
df = df %>% mutate(
  net_p = net * 100 / bezirk_pop,
  ext_net_p = ext_net * 100 / bezirk_pop,
  ext_in_p = ext_in * 100 / bezirk_pop,
  ext_out_p = ext_out * 100 / bezirk_pop,
  int_net_p = int_net * 100 / bezirk_pop,
  int_in_p = int_in * 100 / bezirk_pop,
  int_out_p = int_out * 100 / bezirk_pop,
  local_net_p = local_net * 100 / bezirk_pop,
  local_in_p = local_in * 100 / bezirk_pop,
  local_out_p = local_out * 100 / bezirk_pop,
  local_internal_p = local_internal * 100 / bezirk_pop
)


##export
write.csv(df, "output_data/clean_mig_pop.csv", row.names = FALSE)


##adding geo data -----
geo_bezirk = st_read("data/BEZIRKSGRENZEOGDPolygon.shp")
head(geo_bezirk)
geo_bezirk$BEZNR = as.factor(geo_bezirk$BEZNR)
geo_bezirk = geo_bezirk %>% select(BEZNR, FLAECHE, geometry) %>% rename(bezirk = BEZNR,
                                                                        bezirk_area = FLAECHE,
                                                                        bezirk_geometry = geometry)
head(geo_bezirk)


df = df %>% left_join(geo_bezirk, by = c("bezirk" = "bezirk"))
head(df)

geo_zählbezirk = st_read("data/ZAEHLBEZIRKOGDPolygon.shp")
head(geo_zählbezirk)
geo_zählbezirk$ZBEZNR = geo_zählbezirk$BEZNR * 100 + geo_zählbezirk$ZBEZNR
geo_zählbezirk$ZBEZNR = as.factor(geo_zählbezirk$ZBEZNR)
geo_zählbezirk = geo_zählbezirk %>% select(ZBEZNR, FLAECHE, geometry) %>% rename(
  zählbezirk = ZBEZNR,
  zählbezirk_area = FLAECHE,
  zählbezirk_geometry = geometry
)
head(geo_zählbezirk)
df = df %>% left_join(geo_zählbezirk, by = c("zählbezirk" = "zählbezirk"))
head(df)


## export geo data
head(df)
#st_write(geo_bezirk, "output_data/geo_bez.geojson", row.names = FALSE)
#st_write(geo_zählbezirk, "output_data/geo_zbez.geojson", row.names = FALSE)

###summary statstics -----

#introduce the number of zählbezirke as a measure of the size of the bezirk
bezirk_year = df %>% group_by(year, bezirk) %>% summarize(
  net = sum(net),
  ext_net = sum(ext_net),
  int_net = sum(int_net),
  local_net = sum(local_net),
  size = length(unique(zählbezirk))
)
bezirk = bezirk_year %>% group_by(bezirk) %>% summarize(
  net_mean = mean(net),
  ext_net_mean = mean(ext_net),
  int_net_mean = mean(int_net),
  local_net_mean = mean(local_net),
  size = unique(size)
)

ggplot(
  data = bezirk %>% pivot_longer(
    cols = c(ext_net_mean, int_net_mean, local_net_mean),
    names_to = "means",
    values_to = "migrants"
  ),
  aes(y = migrants, x = bezirk, fill = means)
) + geom_bar(stat = 'identity') +
  labs(title = "average migration for the period 2007 - 2023")

ggplot(data = bezirk,
       aes(y = net_mean, x = bezirk, fill = bezirk)) + geom_bar(stat = 'identity') +
  labs(title = "average net migration for the period 2007 - 2023")


#is there a relationship between size of districs and total movement?
bezirk = bezirk %>% mutate(total_mig_mean = abs(ext_net_mean) + abs(int_net_mean) + abs(local_net_mean))
ggplot(data = bezirk, aes(y = total_mig_mean, x = size)) +
  geom_point()
cor.test(bezirk$total_mig_mean, bezirk$size) #yes, highly correlated


### Visualizations ------

# boxplots
ggplot(data = df, aes(y = net, x = bezirk, fill = bezirk)) +
  geom_boxplot() + theme(legend.position  = "none") +
  labs(y = "migration", title = "netto migration per bezirk to different zählbezirke")

head(df %>% arrange(desc(net)))
#outliers of 9th bezirk = foreign male and female 2015 -> refugee crisis

ggplot(
  data = df %>% group_by(year, bezirk) %>% summarise(net = sum(net)),
  map = aes(y = net, x = bezirk, fill = bezirk)
) +
  geom_boxplot() + theme(legend.position  = "none") +
  labs(y = "migration", title = "net migration aggregated by bezirk")


ggplot(
  data = df %>% group_by(year, bezirk, nationality) %>% summarise(net = sum(net)),
  map = aes(y = net, x = bezirk, fill = nationality)
) +
  geom_boxplot() +
  labs(y = "migration", title = "net migration aggregated by year and bezirk by nationality of migrants")

#correlation between austrian and foreign migration?

aust = df %>% filter(nationality == "Austrian") %>% group_by(year, bezirk) %>% summarize(net = sum(net))
foreign = df %>% filter(nationality == "Foreign") %>% group_by(year, bezirk) %>% summarize(net = sum(net))
ggplot(data = data.frame(aust$net, foreign$net),
       aes(y = aust.net, x = foreign.net)) +
  geom_point() + labs(title = "foreign vs austrian migration aggregated by year and bezirk")
cor.test(aust$net, foreign$net, method = "pearson") #- 0.30
cor.test(aust$net, foreign$net, method = "spearman") #- 0.27


ggplot(
  data = df %>% group_by(year, bezirk, sex) %>% summarise(net = sum(net)),
  map = aes(y = net, x = bezirk, fill = sex)
) +
  geom_boxplot() +
  labs(y = "migration", title = "net migration aggregated by year and bezirk by sex of migrants")

### time series
ggplot(
  data = df %>% group_by(year) %>% summarise(net = sum(net)),
  map = aes(y = net, x = year)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = min(year):max(year)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position  = "none") +
  labs(y = "migration", title = "yearly net migration Vienna")



ggplot(
  data = df %>% group_by(year, sex) %>% summarise(net = sum(net)),
  map = aes(
    y = net,
    x = year,
    fill = sex,
    color = sex
  )
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = min(year):max(year)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme() +
  labs(y = "migration", title = "yearly net migration Vienna by sex")
df %>% group_by(year) %>% summarise(ext_net = sum(ext_net), int_net = sum(int_net)) %>% pivot_longer(
  cols = c(ext_net, int_net),
  names_to = "type",
  values_to = "net"
)


ggplot(
  data = df %>% group_by(year, bezirk) %>% summarise(net = sum(net)),
  map = aes(
    y = net,
    x = year,
    fill = bezirk,
    color = bezirk
  )
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = min(year):max(year)) +
  theme() +
  labs(y = "migration", title = "yearly net migration Vienna by Bezirk")


ggplot(
  data = df %>% group_by(year, nationality) %>% summarise(net = sum(net)),
  map = aes(
    y = net,
    x = year,
    fill = nationality,
    color = nationality
  )
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = min(year):max(year)) +
  theme() +
  labs(y = "migration", title = "yearly net migration Vienna by nationality")
#Austrian migration is net negative and has been going down since 2012

ggplot(
  data = df %>% group_by(year) %>% summarise(ext_net = sum(ext_in), int_net = sum(int_net)) %>% pivot_longer(
    cols = c(ext_net, int_net),
    names_to = "type",
    values_to = "net"
  ),
  map = aes(
    y = net,
    x = year,
    fill = type,
    color = type
  )
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = min(year):max(year)) +
  scale_y_continuous() +
  theme() +
  labs(y = "migration", title = "yearly net migration Vienna External vs Internal migration")
# External migration seems to be correlated with migration of foreigners.
# Austrian and internal migraition seems correlated untill 2018, after that there is a trend break where internal migration goes up but austrian migration goes down.

ggplot(
  data = df %>% group_by(year) %>%
    summarise(
      ext_out = sum(ext_out),
      int_out = sum(int_out),
      ext_in = sum(ext_in),
      int_in = sum(int_in)
    ) %>%
    pivot_longer(
      cols = c(ext_out, int_out, ext_in, int_in),
      names_to = "type",
      values_to = "net"
    ),
  map = aes(
    y = net,
    x = year,
    fill = type,
    color = type
  )
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = min(year):max(year)) +
  scale_y_continuous() +
  theme() +
  labs(y = "migration", title = "yearly net migration Vienna External vs Internal migration")



#### maps
bezirk_year = df %>% group_by(year, bezirk) %>% summarize(
  net = sum(net),
  ext_net = sum(ext_net),
  int_net = sum(int_net),
  local_net = sum(local_net),
  local_internal = sum(local_internal),
  size = length(unique(zählbezirk))
)
bezirk = bezirk_year %>% group_by(bezirk) %>% summarize(
  net_mean = mean(net),
  ext_net_mean = mean(ext_net),
  int_net_mean = mean(int_net),
  local_net_mean = mean(local_net),
  local_internal_mean = mean(local_internal),
  size = unique(size)
)
head(bezirk)
bezirk_year_nat = df %>% group_by(year, bezirk, nationality) %>% summarize(
  net = sum(net),
  ext_net = sum(ext_net),
  int_net = sum(int_net),
  local_net = sum(local_net)
)
#average migration over the years 2007 to 2023
bezirk_nat = bezirk_year_nat %>% group_by(bezirk, nationality) %>% summarize(
  net = mean(net),
  ext_net = mean(ext_net),
  int_net = mean(int_net),
  local_net = mean(local_net)
)


#add geometry
bezirk_nat = bezirk_nat %>% left_join(geo_bezirk, by = c("bezirk" = "bezirk"))
bezirk = bezirk %>% left_join(geo_bezirk, by = c("bezirk" = "bezirk"))

bezirk_aut = bezirk_nat %>% filter(nationality == "Austrian") %>% select(!nationality)
bezirk_for = bezirk_nat %>% filter(nationality == "Foreign") %>% select(!nationality)

bezirk = st_as_sf(bezirk)
bezirk_aut = st_as_sf(bezirk_aut)
bezirk_for = st_as_sf(bezirk_for)

ggplot(bezirk) +
  geom_sf(aes(fill = net_mean), color = "black") +  # Fill polygons by 'net' and outline with black
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    name = "Net Migration"
  ) +  # Color gradient for negative/positive values
  labs(title = "Average Yearly Net Migration by Bezirk from 2007 to 2023") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggplot(bezirk_aut) +
  geom_sf(aes(fill = net), color = "black") +  # Fill polygons by 'net' and outline with black
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    name = "Net Migration"
  ) +  # Color gradient for negative/positive values
  labs(title = "Average Yearly Net Austrian Migration by Bezirk from 2007 to 2023") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggplot(bezirk_for) +
  geom_sf(aes(fill = net), color = "black") +  # Fill polygons by 'net' and outline with black
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    name = "Net Migration"
  ) +  # Color gradient for negative/positive values
  labs(title = "Average Yearly Net Foreign Migration by Bezirk from 2007 to 2023") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggplot(bezirk) +
  geom_sf(aes(fill = local_net_mean), color = "black") +  # Fill polygons by 'net' and outline with black
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    name = "Net Migration"
  ) +  # Color gradient for negative/positive values
  labs(title = "Average Yearly Net Between Bezirk Migration from 2007 to 2023") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggplot(bezirk_aut) +
  geom_sf(aes(fill = local_net), color = "black") +  # Fill polygons by 'net' and outline with black
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    name = "Net Migration"
  ) +  # Color gradient for negative/positive values
  labs(title = "Average Yearly Austrian Net Between Bezirk Migration in Vienna from 2007 to 2023") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggplot(bezirk_for) +
  geom_sf(aes(fill = local_net), color = "black") +  # Fill polygons by 'net' and outline with black
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    name = "Net Migration"
  ) +  # Color gradient for negative/positive values
  labs(title = "Average Yearly Foreign Between Bezirk Migration in Vienna from 2007 to 2023") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )




