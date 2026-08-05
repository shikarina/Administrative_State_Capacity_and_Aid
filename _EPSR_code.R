# Author: Karina Shyrokykh
# Date: August 5, 2026
# Administrative State Capacity and the Effect of EU Democracy Assistance on Levels of Democracy: A Time-Series Cross-Sectional Analysis, 2007–2021  
# Journal: European Political Science Review

rm(list = ls())

# install.packages("readxl")

library(pastecs)
library(stargazer)
library(MASS)
library(tseries)
library(lmtest)
library(nlme)
library(lme4)
library(plm)
library(Formula)
library(arm) 
library(pcse)
library(tseries)
library(nlme)
library(carData)
library(pastecs)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(ggh4x) 
library(gplots)

### download the embrace data from zenodo https://zenodo.org/records/17203623

library(readxl)
data <- read_excel("embrace_wp3_data-2.xlsx")
View(data) 

df <- as.data.frame(data) # save the data as a data frame
head(df)

# data has a panel data structure
library(plm)
pdf <- plm::pdata.frame(df, index = c("country", "year"))

pdf <- pdf %>%
  mutate(across(everything(), ~ replace(., . == -999, NA)))
head(pdf)

# examine the data
head(pdf$government_and_civil_society_total)
tail(pdf$government_and_civil_society_total)

# set v2x_libdem as last col for comparison
pdf <- pdf %>%
  relocate(v2x_libdem, .after = last_col())

# create lags of the DV
pdf$lagged_v2x_libdem <- plm::lag(pdf$v2x_libdem, k = 1)
pdf$lagged2_v2x_libdem <- plm::lag(pdf$v2x_libdem, k = 2)
pdf$lagged3_v2x_libdem <- plm::lag(pdf$v2x_libdem, k = 3)
pdf$lagged4_v2x_libdem <- plm::lag(pdf$v2x_libdem, k = 4)
pdf$lagged5_v2x_libdem <- plm::lag(pdf$v2x_libdem, k = 5)


head(pdf[c("country", "year", "v2x_libdem", "lagged_v2x_libdem", "lagged2_v2x_libdem", "lagged3_v2x_libdem", "lagged4_v2x_libdem")])
tail(pdf[c("country", "year", "v2x_libdem", "lagged_v2x_libdem", "lagged2_v2x_libdem", "lagged3_v2x_libdem", "lagged4_v2x_libdem")])

# DV 2 v2x_polyarchy (for robustness check)
pdf$lagged_v2x_polyarchy <- plm::lag(pdf$v2x_polyarchy, k = 1)
pdf$lagged2_v2x_polyarchy <- plm::lag(pdf$v2x_polyarchy, k = 2)
pdf$lagged3_v2x_polyarchy <- plm::lag(pdf$v2x_polyarchy, k = 3)
pdf$lagged4_v2x_polyarchy <- plm::lag(pdf$v2x_polyarchy, k = 4)
pdf$lagged5_v2x_polyarchy <- plm::lag(pdf$v2x_polyarchy, k = 5)

head(pdf[c("country", "year","lagged2_v2x_polyarchy")])
head(pdf[c("country", "year","lagged3_v2x_polyarchy")])
head(pdf[c("country", "year","lagged4_v2x_polyarchy")])
head(pdf[c("country", "year","lagged5_v2x_polyarchy")])

# lag IVs
pdf$lagged_government_and_civil_society_total <- plm::lag(pdf$government_and_civil_society_total, k = 1)
pdf$lagged2_government_and_civil_society_total <- plm::lag(pdf$government_and_civil_society_total, k = 2)
pdf$lagged3_government_and_civil_society_total <- plm::lag(pdf$government_and_civil_society_total, k = 3)
pdf$lagged4_government_and_civil_society_total <- plm::lag(pdf$government_and_civil_society_total, k = 4)
pdf$lagged5_government_and_civil_society_total <- plm::lag(pdf$government_and_civil_society_total, k = 5)


### Reduce the years to 2007:2021 because those are complete observations.
### Before 2007 and after 2021 data is incomplete for aid and some other variables

df2 <- subset(pdf, df$government_and_civil_society_total !='NA')

summary (df2$government_and_civil_society_total)
summary (df2$lagged2_government_and_civil_society_total)
summary (df2$lagged3_government_and_civil_society_total)

range(df2$memb_status)
unique(df2$country[df2$memb_status == 4])

### I add lagged variables of democracy_assistance and democracy levels
### with n = 1 I assign a lag of 1 year, can be changed according to needs


### Create log of the IV

# +1 before log
df2["new_government_and_civil_society_total"] <- df2$government_and_civil_society_total + 0.1
head(df2$new_government_and_civil_society_total)

# create log values 
df2["log_new_government_and_civil_society_total"] <- log(df2$new_government_and_civil_society_total)
df2$log_new_government_and_civil_society_total

hist(df2$log_new_government_and_civil_society_total)


# lag 1
df2["new_lagged_government_and_civil_society_total"] <- df2$lagged_government_and_civil_society_total + 0.1
head(df2$new_lagged_government_and_civil_society_total)
df2["log_new_lagged_government_and_civil_society_total"] <- log(df2$new_lagged_government_and_civil_society_total)
df2$log_new_lagged_government_and_civil_society_total

hist (df2$log_new_lagged_government_and_civil_society_total)

# lag 2
df2["new_lagged2_government_and_civil_society_total"] <- df2$lagged2_government_and_civil_society_total + 0.1
head(df2$new_lagged2_government_and_civil_society_total)
df2["log_new_lagged2_government_and_civil_society_total"] <- log(df2$new_lagged2_government_and_civil_society_total)
df2$log_new_lagged2_government_and_civil_society_total

# lag 3
df2["new_lagged3_government_and_civil_society_total"] <- df2$lagged3_government_and_civil_society_total + 0.1
head(df2$new_lagged3_government_and_civil_society_total)
df2["log_new_lagged3_government_and_civil_society_total"] <- log(df2$new_lagged3_government_and_civil_society_total)
df2$log_new_lagged3_government_and_civil_society_total

# lag 4
df2["new_lagged4_government_and_civil_society_total"] <- df2$lagged4_government_and_civil_society_total + 0.1
head(df2$new_lagged4_government_and_civil_society_total)
df2["log_new_lagged4_government_and_civil_society_total"] <- log(df2$new_lagged4_government_and_civil_society_total)
df2$log_new_lagged4_government_and_civil_society_total

# lag 5
df2["new_lagged5_government_and_civil_society_total"] <- df2$lagged5_government_and_civil_society_total + 0.1
head(df2$new_lagged5_government_and_civil_society_total)
df2["log_new_lagged5_government_and_civil_society_total"] <- log(df2$new_lagged5_government_and_civil_society_total)
df2$log_new_lagged5_government_and_civil_society_total

### correlation tests for democracy_assistance

acf(na.omit(df2$government_and_civil_society_total), lag.max = 15)
pacf(na.omit(df2$government_and_civil_society_total), lag.max = 15)

cor(df2$v2x_polyarchy, df2$lagged_v2x_polyarchy, method = "pearson", use = "complete.obs")
# serial correlation

# create regions 
df2["wb_region"] <- ifelse(df2$ccodealp == c("ALB","BIH", "XKX", "MNE", "MKD", "SRB", "TUR"), 1, 0)
head(df2$wb_region)

# 
df2["eap_region"] <- ifelse(df2$ccodealp == c("UKR","MDA", "GEO", "BLR", "AZE", "ARM"), 1, 0)
head(df2$eap_region)

#
df2["mena_region"] <- ifelse(df2$ccodealp == c("DZA","EGY", "ISR", "JOR", "LBN", "LBY", "MAR", "PSE", "SYR", "TUN"), 1, 0)
head(df2$mena_region)

# eu_inst_com_tot_curr +1
df2["new_eu_inst_com_tot_curr"] <- df2$eu_inst_com_tot_curr + 0.1
head(df2$new_eu_inst_com_tot_curr)

# create log values 
df2["log_new_eu_inst_com_tot_curr"] <- log(df2$new_eu_inst_com_tot_curr)
df2$log_new_eu_inst_com_tot_curr


# eu_inst_com_tot_curr +1
df2["new_eu_inst_com_tot_curr"] <- df2$eu_inst_com_tot_curr + 0.1
head(df2$new_eu_inst_com_tot_curr)

# create log values 
df2["log_new_eu_inst_com_tot_curr"] <- log(df2$new_eu_inst_com_tot_curr)
df2$log_new_eu_inst_com_tot_curr

df2 %>% 
  select(mena_region, wb_region , eap_region, econ_agreement) %>%
  cor()


selected <- c( "log_new_government_and_civil_society_total", "gdp_per_capita", "eu_relations", "type_of_conflict_2", "type_of_conflict_3", "state_capacity_worldbank", "population", "log_new_eu_inst_com_tot_curr", "econ_agreement", "wb_region, eap_region",  "mena_region")
selected <-as.numeric(selected)
selected <-as.matrix(selected)
res <- cor(selected)
res

a=df2 %>% 
  select(log_new_government_and_civil_society_total, gdp_per_capita, eu_relations, type_of_conflict_2, type_of_conflict_3, state_capacity_worldbank, log_new_eu_inst_com_tot_curr, econ_agreement, wb_region, eap_region,  mena_region) %>%
  cor()

b <- format(round(a, 3), digits = 3)

write.csv(b,file="cor_table_2.csv") 


#### #### #### #### #### #### #### #### #### #### #### 
#### New main results- Table 1  #### 
#### #### #### #### #### #### #### #### #### #### #### 

#t-1
m2 <- plm(v2x_polyarchy ~ log_new_lagged_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
          index = c("ccode", "year"))

coeftest(m2, vcov=vcovSCC(m2))
summary(m2, vcov=vcovSCC)

#t-2
m3 <- plm(v2x_polyarchy ~ log_new_lagged2_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
          index = c("ccode", "year"))

coeftest(m3, vcov=vcovSCC(m3))
summary(m3, vcov=vcovSCC)

#t-3
m4 <- plm(v2x_polyarchy ~ log_new_lagged3_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
          index = c("ccode", "year"))

coeftest(m4, vcov=vcovSCC(m4))
summary(m4, vcov=vcovSCC)

#t-4
m5 <- plm(v2x_polyarchy ~ log_new_lagged4_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
          index = c("ccode", "year"))

coeftest(m5, vcov=vcovSCC(m5))
summary(m5, vcov=vcovSCC)


#t-5
m6 <- plm(v2x_polyarchy ~ log_new_lagged5_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
          index = c("ccode", "year"))

coeftest(m6, vcov=vcovSCC(m6))
summary(m6, vcov=vcovSCC)


table2 <- stargazer::stargazer(m2,
                               m3,
                               m4,
                               m5,
                               m6,
                               type="text", column.labels=c("t-1","t-2","t-3", "t-4","t-5"), 
                               dep.var.labels = c("Dependent variable: Electoral democracy"), 
                               out="_Main Table 1.html", single.row=F)


#### #### #### #### #### #### #### #### #### #### #### 
#### additional results aid/population #### 
#### #### #### #### #### #### #### #### #### #### #### 

# aid is measured in millions of usd

# aid in usd
df2$aid <- df2$new_government_and_civil_society_total*1000000
min(df2$aid, na.rm = TRUE)
max(df2$aid, na.rm = TRUE)

df2$aid_pop <- df2$aid / df2$population
min(df2$aid_pop, na.rm = TRUE)
max(df2$aid_pop, na.rm = TRUE)


df2$lagged_aid_pop <- c(NA, head(df2$aid_pop, -1))
df2$lagged2_aid_pop <- c(NA, NA, head(df2$aid_pop, -2))
df2$lagged3_aid_pop <- c(NA, NA, NA, head(df2$aid_pop, -3))
df2$lagged4_aid_pop <- c(NA, NA, NA, NA, head(df2$aid_pop, -4))
df2$lagged5_aid_pop <- c(NA, NA, NA, NA, NA, head(df2$aid_pop, -5))

head(df2$lagged_aid_pop)
head(df2$lagged2_aid_pop)
head(df2$lagged3_aid_pop)
head(df2$lagged4_aid_pop)
head(df2$lagged5_aid_pop)



library(dplyr)

min(df2$aid_pop, na.rm = TRUE)
max(df2$aid_pop, na.rm = TRUE)

# values are too small for this var to be used in regression
# > min(df2$aid_pop, na.rm = TRUE)
# [1] 1.067076e-08
# > max(df2$aid_pop, na.rm = TRUE)
# [1] 0.0001755567

#t-1
plm_fe_dv1 <- plm(v2x_polyarchy ~ log(lagged_aid_pop) + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) +  wb_region + eap_region + mena_region, data = df2, model = "within", 
                  index = c("ccode", "year"))

coeftest(plm_fe_dv1, vcov=vcovSCC(plm_fe_dv1))
summary(plm_fe_dv1, vcov=vcovSCC)

#t-2
plm_fe_dv1_2 <- plm(v2x_polyarchy ~ log(lagged2_aid_pop) + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_2, vcov=vcovSCC(plm_fe_dv1_2))
summary(plm_fe_dv1_2, vcov=vcovSCC)

#t-3
plm_fe_dv1_3 <- plm(v2x_polyarchy ~ log(lagged3_aid_pop) + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank  + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_3, vcov=vcovSCC(plm_fe_dv1_3))
summary(plm_fe_dv1_3, vcov=vcovSCC)

#t-4
plm_fe_dv1_4 <- plm(v2x_polyarchy ~ log(lagged4_aid_pop) + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank  + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_4, vcov=vcovSCC(plm_fe_dv1_4))
summary(plm_fe_dv1_4, vcov=vcovSCC)


#t-5
plm_fe_dv1_5 <- plm(v2x_polyarchy ~ log(lagged5_aid_pop) + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank  + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_5, vcov=vcovSCC(plm_fe_dv1_5))
summary(plm_fe_dv1_5, vcov=vcovSCC)


table2 <- stargazer::stargazer(plm_fe_dv1,
                               plm_fe_dv1_2,
                               plm_fe_dv1_3,
                               plm_fe_dv1_4,
                               plm_fe_dv1_5,
                               type="text", column.labels=c("t-1","t-2","t-3", "t-4","t-5"), 
                               dep.var.labels = c("Dependent variable: Electoral democracy"), 
                               out="_Appendix Table 5.html", single.row=F)


#### #### #### #### #### #### #### #### #### #### #### 
#### #### #### #### #### #### #### #### #### #### #### 
## Appendix Table 6
#### #### #### #### #### #### #### #### #### #### #### 

#t-1
plm_fe_dv1 <- plm(v2x_libdem ~ log_new_lagged_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                  index = c("ccode", "year"))

coeftest(plm_fe_dv1, vcov=vcovSCC(plm_fe_dv1))
summary(plm_fe_dv1, vcov=vcovSCC)

#t-2
plm_fe_dv1_2 <- plm(v2x_libdem ~ log_new_lagged2_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_2, vcov=vcovSCC(plm_fe_dv1_2))
summary(plm_fe_dv1_2, vcov=vcovSCC)

#t-3
plm_fe_dv1_3 <- plm(v2x_libdem ~ log_new_lagged3_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_3, vcov=vcovSCC(plm_fe_dv1_3))
summary(plm_fe_dv1_3, vcov=vcovSCC)

#t-4
plm_fe_dv1_4 <- plm(v2x_libdem ~ log_new_lagged4_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_4, vcov=vcovSCC(plm_fe_dv1_4))
summary(plm_fe_dv1_4, vcov=vcovSCC)

plm_fe_dv1_5 <- plm(v2x_libdem ~ log_new_lagged5_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_5, vcov=vcovSCC(plm_fe_dv1_5))
summary(plm_fe_dv1_5, vcov=vcovSCC)


table2 <- stargazer::stargazer(plm_fe_dv1,
                               plm_fe_dv1_2,
                               plm_fe_dv1_3,
                               plm_fe_dv1_4,
                               plm_fe_dv1_5,
                               type="text", column.labels=c("t-1","t-2","t-3", "t-4","t-5"), 
                               dep.var.labels = c("Dependent variable: Liberal democracy"), 
                               out="_Appendix Table 6.html", single.row=F)



###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### Appendix - Table 2 
###### ###### ###### ###### ###### ###### ###### ###### ###### 


#t-1
plm_fe_dv1 <- plm(v2x_polyarchy ~ log_new_lagged_government_and_civil_society_total*state_capacity_worldbank + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                  index = c("ccode", "year"))

coeftest(plm_fe_dv1, vcov=vcovSCC(plm_fe_dv1))
summary(plm_fe_dv1, vcov=vcovSCC)

#t-2
plm_fe_dv1_2 <- plm(v2x_polyarchy ~ log_new_lagged2_government_and_civil_society_total*state_capacity_worldbank + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_2, vcov=vcovSCC(plm_fe_dv1_2))
summary(plm_fe_dv1_2, vcov=vcovSCC)

#t-3
plm_fe_dv1_3 <- plm(v2x_polyarchy ~ log_new_lagged3_government_and_civil_society_total*state_capacity_worldbank + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + log(population) +  wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_3, vcov=vcovSCC(plm_fe_dv1_3))
summary(plm_fe_dv1_3, vcov=vcovSCC)

#t-4
plm_fe_dv1_4 <- plm(v2x_polyarchy ~ log_new_lagged4_government_and_civil_society_total*state_capacity_worldbank + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + log(population) +  wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_4, vcov=vcovSCC(plm_fe_dv1_4))
summary(plm_fe_dv1_4, vcov=vcovSCC)


#t-5
plm_fe_dv1_5 <- plm(v2x_polyarchy ~ log_new_lagged5_government_and_civil_society_total*state_capacity_worldbank + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + log(population) +  wb_region + eap_region + mena_region, data = df2, model = "within", 
                    index = c("ccode", "year"))

coeftest(plm_fe_dv1_5, vcov=vcovSCC(plm_fe_dv1_5))
summary(plm_fe_dv1_5, vcov=vcovSCC)


table2 <- stargazer::stargazer(plm_fe_dv1,
                               plm_fe_dv1_2,
                               plm_fe_dv1_3,
                               plm_fe_dv1_4,
                               plm_fe_dv1_5,
                               type="text", column.labels=c("t-1","t-2","t-3"), 
                               dep.var.labels = c("Dependent variable: Democracy"), 
                               out="_Main Table 2.html", single.row=F)



###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### Appendix - Marginal effects plots 
###### ###### ###### ###### ###### ###### ###### ###### ###### 

install.packages("marginaleffects")  

library(marginaleffects)
library(ggplot2)

plm_fe_dv1 <- plm(v2x_polyarchy ~ log_new_lagged_government_and_civil_society_total*state_capacity_worldbank + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
                  index = c("ccode", "year"))

coeftest(plm_fe_dv1, vcov=vcovSCC(plm_fe_dv1))
summary(plm_fe_dv1, vcov=vcovSCC)

range(df2$state_capacity_worldbank)

# Country-clustered covariance matrix
V_country <- plm::vcovHC(
  plm_fe_dv1,
  method  = "arellano",
  type    = "HC1",
  cluster = "group"
)

p_me <- plot_slopes(
  plm_fe_dv1,
  variables = "log_new_lagged_government_and_civil_society_total",
  condition = list(
    state_capacity_worldbank = seq(-1.8, 1.3, by = 0.01)
  ),
  vcov = V_country
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey40"
  ) +
  labs(
    x = "Administrative capacity",
    y = "Marginal effect of democracy assistance",
    caption = "Points and shading represent estimates and 95% confidence intervals."
  ) +
  theme_classic(base_size = 12)

p_me


capacity_range <- range(df2$state_capacity_worldbank, na.rm = TRUE)

hist(df2$state_capacity_worldbank,
     breaks = 20,
     xlim = capacity_range,
     xaxt = "n",
     xlab = "Administrative state capacity",
     main = "")

axis(
  side = 1,
  at = seq(
    floor(capacity_range[1]),
    ceiling(capacity_range[2]),
    by = 0.5
  )
)

###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### Appendix - robustness check -- controls 
###### ###### ###### ###### ###### ###### ###### ###### ###### 

unique(df2[df2$econ_agreement == 4, c("country", "year")])

# recode turkey from 4 to 3
df2$econ_agreement[df2$country == "Türkiye" & df2$econ_agreement == 4] <- 3

# show coding of the var 
unique(df2[df2$econ_agreement == 3, c("country")])

# Additional controls for robustness check of table 1
plm_fe <- plm(v2x_polyarchy ~ log_new_government_and_civil_society_total + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
              index = c("ccode", "year"))

coeftest(plm_fe, vcov=vcovSCC(plm_fe))
summary(plm_fe, vcov=vcovSCC)

#t-1
plm_fe1 <- plm(v2x_polyarchy ~ log_new_lagged_government_and_civil_society_total + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", index = c("ccode", "year"))

coeftest(plm_fe1, vcov=vcovSCC(plm_fe1))
summary(plm_fe1, vcov=vcovSCC)

#t-2
plm_fe2 <- plm(v2x_polyarchy ~ log_new_lagged2_government_and_civil_society_total + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", index = c("ccode", "year"))

coeftest(plm_fe2, vcov=vcovSCC(plm_fe2))
summary(plm_fe2, vcov=vcovSCC)

#t-3
plm_fe3 <- plm(v2x_polyarchy ~ log_new_lagged3_government_and_civil_society_total + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", index = c("ccode", "year"))

coeftest(plm_fe3, vcov=vcovSCC(plm_fe3))
summary(plm_fe3, vcov=vcovSCC)

#t-4
plm_fe4 <- plm(v2x_polyarchy ~ log_new_lagged4_government_and_civil_society_total + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", index = c("ccode", "year"))

coeftest(plm_fe4, vcov=vcovSCC(plm_fe4))
summary(plm_fe4, vcov=vcovSCC)

#t-5
plm_fe5 <- plm(v2x_polyarchy ~ log_new_lagged5_government_and_civil_society_total + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", index = c("ccode", "year"))

coeftest(plm_fe5, vcov=vcovSCC(plm_fe5))
summary(plm_fe5, vcov=vcovSCC)

### Table 

table1 <- stargazer::stargazer(plm_fe1,
                               plm_fe2,
                               plm_fe3,
                               plm_fe4,
                               plm_fe5, type="text", column.labels=c("t-1","t-2","t-3", "t-4", "t-5"), 
                               dep.var.labels = c("Dependent variable: Electoral democracy"), 
                               out="_Appendix Table 2.html", single.row=F)

range(df2$memb_status)

# interaction term
plm_fe <- plm(v2x_polyarchy ~ log_government_and_civil_society_total*state_capacity_worldbank + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", 
              index = c("ccode", "year"))

coeftest(plm_fe, vcov=vcovSCC(plm_fe))
summary(plm_fe, vcov=vcovSCC)

#t-1
plm_fe1_2 <- plm(v2x_polyarchy ~ log_new_lagged_government_and_civil_society_total*state_capacity_worldbank + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", index = c("ccode", "year"))

coeftest(plm_fe1_2, vcov=vcovSCC(plm_fe1_2))
summary(plm_fe1_2, vcov=vcovSCC)

#t-2
plm_fe2_2 <- plm(v2x_polyarchy ~ log_new_lagged2_government_and_civil_society_total*state_capacity_worldbank + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", index = c("ccode", "year"))

coeftest(plm_fe2_2, vcov=vcovSCC(plm_fe2_2))
summary(plm_fe2_2, vcov=vcovSCC)

#t-3
plm_fe3_2 <- plm(v2x_polyarchy ~ log_new_lagged3_government_and_civil_society_total*state_capacity_worldbank + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", index = c("ccode", "year"))

coeftest(plm_fe3_2, vcov=vcovSCC(plm_fe3_2))
summary(plm_fe3_2, vcov=vcovSCC)

#t-4
plm_fe4_2 <- plm(v2x_polyarchy ~ log_new_lagged4_government_and_civil_society_total*state_capacity_worldbank + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", index = c("ccode", "year"))

coeftest(plm_fe4_2, vcov=vcovSCC(plm_fe4_2))
summary(plm_fe4_2, vcov=vcovSCC)

#t-5
plm_fe5_2 <- plm(v2x_polyarchy ~ log_new_lagged5_government_and_civil_society_total*state_capacity_worldbank + log(gdp_per_capita) + memb_status  + type_of_conflict_2 + type_of_conflict_3 + log_new_eu_inst_com_tot_curr + econ_agreement + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", index = c("ccode", "year"))

coeftest(plm_fe5_2, vcov=vcovSCC(plm_fe5_2))
summary(plm_fe5_2, vcov=vcovSCC)

### Table 2

table2 <- stargazer::stargazer(plm_fe1_2,
                               plm_fe2_2,
                               plm_fe3_2, 
                               plm_fe4_2,
                               plm_fe5_2, type="text", column.labels=c("t-1","t-2","t-3","t-4","t-5"), 
                               dep.var.labels = c("Dependent variable: Electoral democracy"), 
                               out="_Appendix Table 3.html", single.row=F)

###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### Appendix - robustness check --  Table 4 TWFE 
###### ###### ###### ###### ###### ###### ###### ###### ######


#DV1

m1 <- plm(v2x_polyarchy ~ log_new_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank  + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", effect = "twoways",
          index = c("ccode", "year"))

coeftest(m1, vcov=vcovSCC(m1))
summary(m1, vcov=vcovSCC)

#t-1
m2 <- plm(v2x_polyarchy ~ log_new_lagged_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", effect = "twoways",
          index = c("ccode", "year"))

coeftest(m2, vcov=vcovSCC(m2))
summary(m2, vcov=vcovSCC)

#t-2
m3 <- plm(v2x_polyarchy ~ log_new_lagged2_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", effect = "twoways",
          index = c("ccode", "year"))

coeftest(m3, vcov=vcovSCC(m3))
summary(m3, vcov=vcovSCC)

#t-3
m4 <- plm(v2x_polyarchy ~ log_new_lagged3_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", effect = "twoways",
          index = c("ccode", "year"))

coeftest(m4, vcov=vcovSCC(m4))
summary(m4, vcov=vcovSCC)

#t-4
m5 <- plm(v2x_polyarchy ~ log_new_lagged4_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", effect = "twoways",
          index = c("ccode", "year"))

coeftest(m5, vcov=vcovSCC(m5))
summary(m5, vcov=vcovSCC)


#t-5
m6 <- plm(v2x_polyarchy ~ log_new_lagged5_government_and_civil_society_total + memb_status  + log(gdp_per_capita) + type_of_conflict_2 + type_of_conflict_3 + state_capacity_worldbank + log(population) + wb_region + eap_region + mena_region, data = df2, model = "within", effect = "twoways",
          index = c("ccode", "year"))

coeftest(m6, vcov=vcovSCC(m6))
summary(m6, vcov=vcovSCC)


table2 <- stargazer::stargazer(m2,
                               m3,
                               m4,
                               m5,
                               m6,
                               type="text", column.labels=c("t-1","t-2","t-3", "t-4","t-5"), 
                               dep.var.labels = c("Dependent variable: Electoral democracy"), 
                               out="_Appendics Table 4.html", single.row=F)

###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### Appendix - robustness check -- 2SLS 
###### ###### ###### ###### ###### ###### ###### ###### ###### 
library(plm)
library(lmtest)
library(sandwich)
library(modelsummary)

# Declare panel
df2 <- pdata.frame(df2, index = c("ccode", "year"))

# Create lagged instruments for aid
df2$aid_lag1 <- lag(df2$log_new_government_and_civil_society_total, 1)
df2$aid_lag2 <- lag(df2$log_new_government_and_civil_society_total, 2)

# -------------------------
# 1. First stage
# -------------------------
first_stage <- plm(
  log_new_government_and_civil_society_total ~
    aid_lag1 +
    aid_lag2 +
    memb_status +
    log(gdp_per_capita) +
    type_of_conflict_2 +
    type_of_conflict_3 +
    state_capacity_worldbank +
    log(population),
  data = df2,
  model = "within",
  effect = "twoways"
)

coeftest(first_stage, vcov = vcovSCC(first_stage))

# Second-stage IV / 2SLS model

iv_model <- plm(
  v2x_polyarchy ~
    log_new_government_and_civil_society_total +
    memb_status +
    log(gdp_per_capita) +
    type_of_conflict_2 +
    type_of_conflict_3 +
    state_capacity_worldbank +
    log(population)
  |
    aid_lag1 +
    aid_lag2 +
    memb_status +
    log(gdp_per_capita) +
    type_of_conflict_2 +
    type_of_conflict_3 +
    state_capacity_worldbank +
    log(population),
  data = df2,
  model = "within",
  effect = "twoways"
)

coeftest(iv_model, vcov = vcovSCC(iv_model))




# Model summary

install.packages("pandoc")
library(pandoc)

modelsummary(
  list(
    "First stage: Aid" = first_stage,
    "Second stage: Polyarchy" = iv_model
  ),
  vcov = list(
    vcovSCC(first_stage),
    vcovSCC(iv_model)
  ),
  coef_map = c(
    "aid_lag1" = "Aid, t-1",
    "aid_lag2" = "Aid, t-2",
    "log_new_government_and_civil_society_total" = "Instrumented democracy assistance",
    "memb_status" = "Membership status",
    "log(gdp_per_capita)" = "GDP per capita (log)",
    "type_of_conflict_2" = "Conflict type 2",
    "type_of_conflict_3" = "Conflict type 3",
    "state_capacity_worldbank" = "State capacity",
    "log(population)" = "Population (log)"
  ),
  gof_map = c("nobs", "r.squared"),
  stars = TRUE,
  output = "markdown"
)

## ## ## ## ## ## ## ## ## ## 
## Table 7, SLS ## ## ## ## ## 
## ## ## ## ## ## ## ## ## ## 
#in word ument, Reported in Table 7

modelsummary(
  list(
    "First stage: Aid" = first_stage,
    "Second stage: Polyarchy" = iv_model
  ),
  vcov = list(
    vcovSCC(first_stage),
    vcovSCC(iv_model)
  ),
  coef_map = c(
    "aid_lag1" = "Aid, t-1",
    "aid_lag2" = "Aid, t-2",
    "log_new_government_and_civil_society_total" = "Instrumented democracy assistance",
    "memb_status" = "Membership status",
    "log(gdp_per_capita)" = "GDP per capita (log)",
    "type_of_conflict_2" = "Conflict type 2",
    "type_of_conflict_3" = "Conflict type 3",
    "state_capacity_worldbank" = "State capacity",
    "log(population)" = "Population (log)"
  ),
  stars = TRUE,
  output = "_2sls_results.docx"
)



# R&R 1
unique(df[df$government_and_civil_society_total == 0, c("year", "country")])
# only israel did not recive aid since 2007

range(df$government_and_civil_society_total, na.rm = TRUE)

unique(df[df$government_and_civil_society_total <5 , c("year", "country")])

library(ggplot2)
ggplot(df, aes(x = year, y = government_and_civil_society_total)) +
  geom_line(color = "black") +
  facet_wrap(~ country, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Year",
    y = "Government & Civil Society Total"
  )

library(ggplot2)

# Appendix Figure 3
ggplot(df, aes(x = year, y = government_and_civil_society_total)) +
  geom_line(color = "black") +
  facet_wrap(~ country) +  # same y-axis across panels
  theme_bw() +
  labs(
    x = "Year",
    y = "Democracy assistance (in mil USD)"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5)
  )

# Appendix Figure 4
ggplot(df2, aes(x = year, y = v2x_polyarchy)) +
  geom_line(color = "black") +
  facet_wrap(~ country) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Electoral democracy index"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(2007, 2021)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5)
  )

library(dplyr)
library(ggplot2)

df %>%
  filter(country == "Belarus") %>%
  ggplot(aes(x = year, y = government_and_civil_society_total)) +
  geom_line(color = "black") +
  theme_bw() +
  labs(
    x = "Year",
    y = "Government & Civil Society Total",
    title = "Belarus"
  )
