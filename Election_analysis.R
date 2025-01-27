library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(DescTools)
library(sf)
library(tmap)
library(corrplot)
library(data.table)
library(purrr)
library(car)
library(Metrics)
library(rsq)
library(jtools)

## data import -----
election = read_delim(
  "data/wahl_20241003_214746.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
df = read_csv("output_data/clean_mig_pop.csv")
geo_bezirk = st_read("output_data/geo_bez.geojson")
geo_zählbezirk = st_read("output_data/geo_zbez.geojson")


## election data -----
head(election)

election = election %>% filter(substr(NUTS, 1, 2) == "G9" &
                                 Wahlberechtigte > 0 &
                                 substr(NUTS, 5, 6) == "00")
election = election[9:nrow(election),]
election$bezirk = 1:23
election = subset(
  election,
  select = -c(
    NUTS,
    Gebietsname,
    Wahlberechtigte,
    Abgegebene,
    `Ung?ltige`,
    `G?ltige`,
    ...19,
    BGE
  )
)
head(election)

colnames(election) = c(
  "ÖVP",
  "SPÖ",
  "FPÖ",
  "GRÜNE",
  "NEOS",
  'BIER',
  "MFG",
  "LMP",
  "GAZA",
  "KPÖ",
  "KEINE",
  "bezirk"
)
parliament = c("SPÖ", "ÖVP", "FPÖ", "GRÜNE", "NEOS")

head(election)
election$bezirk = as.factor(election$bezirk)

# turn election results into percentage
perc = function(vec) {
  vec = vec / sum(vec)
}

election = apply(election %>% select(!bezirk),
                 FUN = perc,
                 MARGIN = 1)
election = t(as.data.frame(election))
head(election)
rownames(election) = 1:nrow(election)
election = as.data.frame(election)
election$bezirk = as.factor(1:nrow(election))
head(election)


## bezirks data -----
head(df)

#add migration as percentage of current population

year_bezirk_sex_nat = df %>% group_by(year, bezirk, sex, nationality) %>%
  summarize(
    net = sum(net),
    ext_net = sum(ext_net),
    int_net = sum(int_net),
    local_net = sum(local_net),
    local_internal = sum(local_internal),
    net_p = sum(net_p),
    ext_net_p = sum(ext_net_p),
    int_net_p = sum(int_net_p),
    local_net_p = sum(local_net_p),
    local_internal_p = sum(local_internal_p)
  )
head(year_bezirk_sex_nat)

#average yearly net migration from 2007 to 2023 per nationality
bezirk_nat = year_bezirk_sex_nat %>% group_by(bezirk, nationality) %>%
  summarize(
    net = mean(net),
    ext_net = mean(ext_net),
    int_net = mean(int_net),
    local_net = mean(local_net),
    local_internal = mean(local_internal),
    net_p = mean(net_p),
    ext_net_p = mean(ext_net_p),
    int_net_p = mean(int_net_p),
    local_net_p = mean(local_net_p),
    local_internal_p = mean(local_internal_p)
  )

bezirk_nat$bezirk = as.factor(bezirk_nat$bezirk)
head(bezirk_nat)

bezirk_election = bezirk_nat %>% inner_join(election, by = c("bezirk" = "bezirk"))
head(bezirk_election)

tot_mig = as.data.frame(bezirk_election) %>% group_by(bezirk) %>% summarize(
  net = sum(net),
  ext_net = sum(ext_net),
  int_net = sum(int_net),
  local_net = sum(local_net),
  local_internal = sum(local_internal),
  net_p = sum(net_p),
  ext_net_p = sum(ext_net_p),
  int_net_p = sum(int_net_p),
  local_net_p = sum(local_net_p),
  local_internal_p = sum(local_internal_p)
)
tot_mig = as.data.frame(tot_mig) %>%
  inner_join(election, by = c("bezirk" = "bezirk"))

aut_mig = as.data.frame(bezirk_election) %>% filter(nationality == "Austrian")
for_mig = as.data.frame(bezirk_election) %>% filter(nationality == "Foreign")
head(aut_mig)
head(for_mig)


# Analysis -----
##What is the relationship between recent migration and 2024 election results?

plot(x = aut_mig$net, y = aut_mig$FPÖ)
cor.test(aut_mig$net, aut_mig$FPÖ) #r = - 0.9458. The larger the negative net migration of Austrians, the larger percentage of FPÖ votes. Districts which sees a loss in austrians see an increase in FPÖ voters
plot(x = aut_mig$net_p, y = aut_mig$FPÖ)
cor.test(x = aut_mig$net_p,
         y = aut_mig$FPÖ,
         method = "spearman")

cors = as.data.frame(aut_mig) %>% select(!c(bezirk, nationality))
cm = cor(cors)
corrplot(cm)

get_correlations = function(data, covar, parties) {
  #takes a dataframe with data of party election results and a correlated variable, and returns a dataframe
  #with the correlation coefficient between the correlated variable and the election result of each party
  #with a 95% CI
  r = c()
  upper_ci = c()
  lower_ci = c()
  covar = data[, covar]
  for (party in parties) {
    part_results = data[, party]
    correlation = cor.test(covar, part_results)
    r = c(r, correlation$estimate)
    upper_ci = c(upper_ci, correlation$conf.int[1])
    lower_ci = c(lower_ci, correlation$conf.int[2])
  }
  df_out = data.frame(parties, r, upper_ci, lower_ci)
  return(df_out)
}



### net number migration -----
corrs_aut = get_correlations(aut_mig, "net", parliament)
head(corrs_aut)

ggplot(data = corrs_aut, aes (y = r, x = parties, fill = parties)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  labs(title = "Corrrelation coefficients with 95% CI 2024 election results and austrian migration to Vienna (average yearly net number of migrants)", y = "Pearson's r")

corrs_for = get_correlations(for_mig, "net", parliament)
head(corrs_for)

ggplot(data = corrs_for, aes (y = r, x = parties, fill = parties)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  labs(title = "Corrrelation coefficients with 95% CI for voting patterns and foreign migration (average yearly net number of migrants)", y = "Pearson's r")
#Districts which more foreign immigration tends to vote FPÖ

corrs_tot = get_correlations(tot_mig, "net", parliament)
head(corrs_tot)
ggplot(data = corrs_tot, aes (y = r, x = parties, fill = parties)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  labs(title = "Corrrelation coefficients with 95% CI for voting patterns and migration (average yearly net number of migrants", y = "Pearson's r")


### net percentage migration ----
corrs_aut = get_correlations(aut_mig, "net_p", parliament)
head(corrs_aut)

ggplot(data = corrs_aut, aes (y = r, x = parties, fill = parties)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  labs(title = "Corrrelation coefficients with 95% CI 2024 election results and austrian migration to Vienna (average yearly percent)", y = "Pearson's r")

corrs_for = get_correlations(for_mig, "net_p", parliament)
head(corrs_for)

ggplot(data = corrs_for, aes (y = r, x = parties, fill = parties)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  labs(title = "Corrrelation coefficients with 95% CI for voting patterns and foreign migration (average yearly percent)", y = "Pearson's r")


corrs_tot = get_correlations(tot_mig, "net_p", parliament)
head(corrs_tot)
ggplot(data = corrs_tot, aes (y = r, x = parties, fill = parties)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  labs(title = "Corrrelation coefficients with 95% CI for voting patterns and migration (average yearly percent)", y = "Pearson's r")

### local migration ----

corrs_aut = get_correlations(aut_mig, "local_net_p", parliament)
head(corrs_aut)

ggplot(data = corrs_aut, aes (y = r, x = parties, fill = parties)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  labs(title = "Corrrelation coefficients with 95% CI 2024 election results and austrian migration within Vienna (average yearly percent)", y = "Pearson's r")

corrs_for = get_correlations(for_mig, "local_net_p", parliament)
head(corrs_for)

ggplot(data = corrs_for, aes (y = r, x = parties, fill = parties)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  labs(title = "Corrrelation coefficients with 95% CI for voting patterns and foreign migration (average yearly percent)", y = "Pearson's r")

corrs_tot = get_correlations(tot_mig, "local_net_p", parliament)
head(corrs_tot)
ggplot(data = corrs_tot, aes (y = r, x = parties, fill = parties)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  labs(title = "Corrrelation coefficients with 95% CI for voting patterns and migration (average yearly net number of migrants", y = "Pearson's r")




# modeling -----

## add socioeconomic indicators

#average income 2021 https://www.data.gv.at/katalog/dataset/d76c0e8b-c599-4700-8a88-29d0d87e563d
income = read_delim(
  "data/vie-bez-biz-ecn-inc-sex-2002f.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
income = income %>%
  filter(REF_YEAR == 2021) %>%
  select(DISTRICT_CODE, INC_TOT_VALUE)
colnames(income) = c("bezirk", "income")
income = income[2:23, ]
income$bezirk = as.factor(as.numeric(substr(income$bezirk, 2, 3)))
head(income)

## add demographic indicator https://www.data.gv.at/katalog/dataset/9ecf5866-dbe8-4cb2-b156-5097c7eec01f

age = read_delim(
  "data/vie-bez-biz-pop-age-2002f.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
age = age %>% filter(REF_YEAR == 2024) %>% select(DISTRICT_CODE, AGE_AVE)
colnames(age) = c("bezirk", "age")
age = age[2:23, ]
age$age = age$age / 100
age$bezirk = as.factor(as.numeric(substr(age$bezirk, 2, 3)))
head(age)


sex = read_delim(
  "data/vie-bez-pop-sex-stk-1869f.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

sex = sex %>% filter(REF_YEAR == 2024) %>% mutate(perc_fem = POP_FEM / POP_TOTAL * 100) %>% select(DISTRICT_CODE, perc_fem)
colnames(sex) = c("bezirk", "perc_fem")
sex$bezirk = as.factor(as.numeric(substr(sex$bezirk, 2, 3)))
head(sex)


## mdl_df

aut_mig_var = as.data.frame(aut_mig) %>%
  select(bezirk, net_p, local_net_p, local_internal_p) %>%
  rename_with( ~ paste0(., "_aut"), c(net_p, local_net_p, local_internal_p))

for_mig_var = as.data.frame(for_mig) %>%
  select(bezirk, net_p, local_net_p, local_internal_p) %>%
  rename_with( ~ paste0(., "_for"), c(net_p, local_net_p, local_internal_p))

election

list_df = list(age, sex, income, aut_mig_var, for_mig_var, election[c(parliament, "bezirk")])

mdl_df = reduce(list_df, ~ inner_join(.x, .y, by = "bezirk"))

head(mdl_df)
mdl_df = as.data.frame(mdl_df %>% select(!bezirk))


## modeling

#to improve models, remove non significant factor with high VIF and check for improvement of AIC and VIF and residuals
attach(mdl_df)
cm = cor(mdl_df)
corrplot(cm, method = "pie")
corrplot(cm, method = "number")

## FPÖ ----
head(mdl_df)
df_mdl_FPÖ = subset(mdl_df, select = -c(SPÖ, ÖVP, GRÜNE, NEOS))

mdl_FPÖ = lm(data = df_mdl_FPÖ, FPÖ  ~ .)
summary(mdl_FPÖ)
rsq(mdl_FPÖ) #0.953
AIC(mdl_FPÖ) # 15.98
RMSE(mdl_FPÖ) #0.031, good as we are trying to predict a value between 0 and 1
vif(mdl_FPÖ) # quite high multicolinearity
res = resid(mdl_FPÖ)
qqnorm(res)
qqline(res) # normally distributed residuals

mdl_FPÖ_scale = lm(data = as.data.frame(apply(
  df_mdl_FPÖ, MARGIN = 2, FUN = scale
)), FPÖ  ~ .)
summary(mdl_FPÖ_scale)
plot_coefs(mdl_FPÖ_scale) + theme_apa()



df_mdl_FPÖ_2 = subset(df_mdl_FPÖ,
                      select = -c(local_net_p_aut, age, local_internal_p_for))

mdl_FPÖ_2 = lm(data = df_mdl_FPÖ_2, FPÖ ~ .)
summary(mdl_FPÖ_2)
rsq(mdl_FPÖ_2) #0.943, small loss
RMSE(mdl_FPÖ_2) #0.015 twice as small error
AIC(mdl_FPÖ_2) #-106.20 improvement
vif(mdl_FPÖ_2) # no VIF above 10 -> great!
res = resid(mdl_FPÖ_2)
qqnorm(res)
qqline(res)
shapiro.test(res) #no significant support for deviation from normality

mdl_FPÖ_2_scale = lm(data = as.data.frame(apply(
  df_mdl_FPÖ_2, MARGIN = 2, FUN = scale
)), FPÖ  ~ .)
summary(mdl_FPÖ_2_scale)
plot_coefs(mdl_FPÖ_2_scale, colors = "darkblue") + theme_apa() + ggtitle("Beta coefficients for linear regression model, FPÖ results as dependent variable")

best_mdl_FPÖ = mdl_FPÖ_2_scale

## GRÜNE-----head(mdl_df)
df_mdl_GRN = subset(mdl_df, select = -c(SPÖ, ÖVP, FPÖ, NEOS))

mdl_GRN = lm(data = df_mdl_GRN, GRÜNE ~ .)
summary(mdl_GRN)
rsq(mdl_GRN) #0.952
AIC(mdl_GRN) #-118
RMSE(mdl_GRN) #0.01
vif(mdl_GRN)
res = resid(mdl_GRN)
qqnorm(res)
qqline(res)
shapiro.test(res) # no support for deviation from normality

df_mdl_GRN_2 = subset(
  df_mdl_GRN,
  select = -c(
    local_internal_p_aut,
    local_internal_p_for,
    local_net_p_aut,
    age
  )
)

mdl_GRN_2 = lm(data = df_mdl_GRN_2, GRÜNE ~ .)
summary(mdl_GRN_2)
rsq(mdl_GRN_2) #0.947
AIC(mdl_GRN_2) #-123.54
RMSE(mdl_GRN) #0.01
vif(mdl_GRN_2) # no VID above 10
res = resid(mdl_GRN_2)
qqnorm(res)
qqline(res)
shapiro.test(res)

mdl_GRN_2_scale = lm(data = as.data.frame(scale(df_mdl_GRN_2)), GRÜNE ~ .)
summary(mdl_GRN_2_scale)

plot_coefs(mdl_GRN_2_scale, colors = "green") + theme_apa() + ggtitle("Beta coefficients for linear regression model, GRÜNE results as dependent variable")

best_mdl_GRN = mdl_GRN_2_scale

## ÖVP

head(mdl_df)
df_mdl_ÖVP = subset(mdl_df, select = -c(SPÖ, GRÜNE, FPÖ, NEOS))

mdl_ÖVP = lm(data = df_mdl_ÖVP, ÖVP ~ .)
summary(mdl_ÖVP)
rsq(mdl_ÖVP) #0.97
AIC(mdl_ÖVP) #-128
RMSE(mdl_ÖVP) #0.0070
vif(mdl_ÖVP) #
res = resid(mdl_ÖVP)
qqnorm(res)
qqline(res)
shapiro.test(res) # no support for deviation from normality


df_mdl_ÖVP_2 = subset(
  df_mdl_ÖVP,
  select = -c(
    income,
    local_internal_p_aut,
    local_internal_p_aut,
    local_internal_p_for,
    local_net_p_aut
  )
)

mdl_ÖVP_2 = lm(data = df_mdl_ÖVP_2, ÖVP ~ .)
summary(mdl_ÖVP_2)
rsq(mdl_ÖVP_2) #0.97
AIC(mdl_ÖVP_2) #-133 #improvement
RMSE(mdl_ÖVP_2) #0.0085 #small error increase
vif(mdl_ÖVP_2) # local_net_p_for = 10.3
res = resid(mdl_ÖVP_2)
qqnorm(res)
qqline(res)
shapiro.test(res) # no support for deviation from normality

mdl_ÖVP_2_scale = lm(data = as.data.frame(scale(df_mdl_ÖVP_2)), ÖVP ~ .)
summary(mdl_ÖVP_2_scale)

plot_coefs(mdl_ÖVP_2_scale, colors = "black") + theme_apa() + ggtitle("Beta coefficients for linear regression model, ÖVP results as dependent variable")

best_mdl_ÖVP = mdl_ÖVP_2_scale



### SPÖ

head(mdl_df)
df_mdl_SPÖ = subset(mdl_df, select = -c(ÖVP, GRÜNE, FPÖ, NEOS))

mdl_SPÖ = lm(data = df_mdl_SPÖ, SPÖ ~ .)
summary(mdl_SPÖ)
rsq(mdl_SPÖ) #0.950
AIC(mdl_SPÖ) #-121.52
RMSE(mdl_SPÖ) #0.0093
vif(mdl_SPÖ) #
res = resid(mdl_SPÖ)
qqnorm(res)
qqline(res)
shapiro.test(res) # no support for deviation from normality


df_mdl_SPÖ_2 = subset(df_mdl_SPÖ,
                      select = -c(income, local_net_p_for, local_internal_p_for))

mdl_SPÖ_2 = lm(data = df_mdl_SPÖ_2, SPÖ ~ .)
summary(mdl_SPÖ_2)
rsq(mdl_SPÖ_2) #0.935
AIC(mdl_SPÖ_2) #-121.65 #improvement
RMSE(mdl_SPÖ_2) #0.0106 #small error increase
vif(mdl_SPÖ_2) # no VIF > 10
res = resid(mdl_SPÖ_2)
qqnorm(res)
qqline(res)
shapiro.test(res) # no support for deviation from normality

mdl_SPÖ_2_scale = lm(data = as.data.frame(scale(df_mdl_SPÖ_2)), SPÖ ~ .)
summary(mdl_SPÖ_2_scale)

plot_coefs(mdl_SPÖ_2_scale, colors = "red") + theme_apa() + ggtitle("Beta coefficients for linear regression model, SPÖ results as dependent variable")

best_mdl_SPÖ = mdl_SPÖ_2_scale


### NEOS


head(mdl_df)
df_mdl_NEOS = subset(mdl_df, select = -c(ÖVP, GRÜNE, FPÖ, SPÖ))

mdl_NEOS = lm(data = df_mdl_NEOS, NEOS ~ .)
summary(mdl_NEOS)
rsq(mdl_NEOS) #0.96
AIC(mdl_NEOS) #-132.46
RMSE(mdl_NEOS) #0.0072
vif(mdl_NEOS) #
res = resid(mdl_NEOS)
qqnorm(res)
qqline(res)
shapiro.test(res) # no support for deviation from normality


df_mdl_NEOS_2 = subset(
  df_mdl_NEOS,
  select = -c(
    age,
    local_net_p_aut,
    local_internal_p_aut,
    local_internal_p_for
  )
)

mdl_NEOS_2 = lm(data = df_mdl_NEOS_2, NEOS ~ .)
summary(mdl_NEOS_2)
rsq(mdl_NEOS_2) #0.957
AIC(mdl_NEOS_2) #-138.40 #improvement
RMSE(mdl_NEOS_2) #0.076 #small error increase
vif(mdl_NEOS_2) # no VIF > 10
res = resid(mdl_NEOS_2)
qqnorm(res)
qqline(res)
shapiro.test(res) # no support for deviation from normality

mdl_NEOS_2_scale = lm(data = as.data.frame(scale(df_mdl_NEOS_2)), NEOS ~ .)
summary(mdl_NEOS_2_scale)

plot_coefs(mdl_NEOS_2_scale, colors = "pink") + theme_apa() + ggtitle("Beta coefficients for linear regression model, NEOS results as dependent variable")

best_mdl_NEOS = mdl_NEOS_2_scale

###sum plot

plot_summs(
  best_mdl_FPÖ,
  best_mdl_GRN,
  best_mdl_ÖVP,
  best_mdl_SPÖ,
  best_mdl_NEOS,
  colors = c("blue", 'green', 'black', 'red', 'pink'),
  model.names = c("FPÖ", 'GRÜNE', 'ÖVP', "SPÖ", 'NEOS')
) +
  theme_apa() + ggtitle("Comparison of beta coefficients for modelling party support in Vienna")



###vis mdl
library(ggrepel)

df_mdl_FPÖ$Bezirk = 1:nrow(df_mdl_FPÖ)
df_mdl_FPÖ$FPÖ = df_mdl_FPÖ$FPÖ * 100


ggplot(data = df_mdl_FPÖ, 
       aes(y = FPÖ, x = net_p_aut)) + 
  geom_point() + 
  geom_label_repel(aes(label = Bezirk)) +
  labs(y = "FPÖ votes (%)",
       x = "Avrg Austrian migration (%)") + 
  theme_light()
  

head(df_mdl_FPÖ)
