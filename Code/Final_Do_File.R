#Purpose: Code for Water Justice: Empirical analysis of U.S. patterns and Regional heterogeneity
#Author: Zoey Reed-Spitzer
#Date Created: 10/10/2024
#Date Updated: 3/22/2026

# ---- Setup ----
setwd("C:/Users/zoeys/Documents/EJ-Paper-Summer-2024")

#install packages
#install.packages("plm")
#install.packages("lme4")
#install.packages("lmtest")
#install.packages("zoo")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("sandwich")
#install.packages("ggplot2")
#install.packages("ggforce")
#install.packages("stargazer")
#install.packages("broom")
#install.packages("forestplot")
#install.packages("writexl")
#install.packages("stringr")
#install.packages("ggcorrplot")
#install.packages("car")
#install.packages("reshape2")
#install.packages("sf")
#install.packages("tigris")
#install.packages("purrr")
#install.packages("units")

#load packages
library(readxl)
library(readr)
library(plm)
library(lme4)
library(lmtest)
library(zoo)
library(dplyr)
library(tidyr)
library(sandwich)
library(ggplot2)
library(ggforce)
library(stargazer)
library(broom)
library(forestplot)
library(writexl)
library(stringr)
library(ggcorrplot)
library(car)
library(reshape2)
library(sf)
library(tigris)
library(purrr)
library(units)
library(emmeans)


#load data
df_sj <- read_xlsx("Data/Raw_Data/1.0-communities.xlsx",
                   sheet = "Data")
df_mi <- read_xlsx("Data/Raw_Data/US_Mean_Income_2019/ACSST5Y2019.S1902-Data.xlsx",
                   sheet = "Sheet1")
df_pci <- read_xlsx("Data/Raw_Data/US_PerCap_Income_2019/ACSDT5Y2019.B19301-Data.xlsx",
                    sheet = "Sheet1")
df_are <- read_xlsx("Data/Raw_Data/Area/cb_2019_us_tract_AllStates.xlsx",
                    sheet = "cb_2019_us_tract_500k.shp")
#df_crb <- read_xlsx("Raw_Data/Tracts_50inCRB.xlsx", #changed since 5/16 w/ more restrictive tract choice
#                    sheet = "Sheet1")
df_re <- read_xlsx("Data/Raw_Data/US_Race&Ethnicity/ACSDT5Y2019.B03002-Data.xlsx",
                   sheet = "Sheet1")
df_hh <- read_xlsx("Data/Raw_Data/US_Households/ACSDT5Y2019.B25001-Data.xlsx",
                   sheet = "Sheet1")

# ---- Data Cleaning ----
# rename census tract variable (changing name to crb for temp convenience)
df_sj_crb <- df_sj %>% 
  rename(GEOID10 = "Census tract 2010 ID")

# merge df_sj with tracts in CRB
#df_sj_crb <- merge(df_crb,df_sj,by="GEOID10")
# 7764 observations (matches number of tracts in CRB)

df_sj_crb <- df_sj_crb %>%  #adding air pollution and asthma incidence
  rename(pct_black = "Percent Black or African American alone",
         pct_indian = "Percent American Indian / Alaska Native",
         pct_asian = "Percent Asian",
         pct_haw = "Percent Native Hawaiian or Pacific",
         pct_2race = "Percent two or more races",
         pct_white = "Percent White",
         pct_hisp = "Percent Hispanic or Latino",
         pct_other = "Percent other races",
         pct_disadv = "Percentage of tract that is disadvantaged by area",
         exp_ag_loss = "Expected agricultural loss rate (Natural Hazards Risk Index)",
         flood_risk = "Share of properties at risk of flood in 30 years",
         fire_risk = "Share of properties at risk of fire in 30 years",
         energy_burd = "Energy burden",
         no_plumb = "Share of homes with no kitchen or indoor plumbing (percent)",
         no_plumb_norm = "Share of homes with no kitchen or indoor plumbing (percentile)",
         waste_water = "Wastewater discharge",
         leak_ust = "Leaky underground storage tanks",
         leak_ust_norm = "Leaky underground storage tanks (percentile)",
         imperv_surface = "Share of the tract's land area that is covered by impervious surface or cropland as a percent",
         imperv_surface_norm = "Share of the tract's land area that is covered by impervious surface or cropland as a percent (percentile)",
         pm2 = "PM2.5 in the air",
         pm2_norm = "PM2.5 in the air (percentile)",
         asthma_norm = "Current asthma among adults aged greater than or equal to 18 years (percentile)",
         life_exp = "Life expectancy (years)",
         med_inc = "Median household income as a percent of area median income",
         pct_unemp = "Unemployment (percent)",
         pov_sub100 = "Percent of individuals < 100% Federal Poverty Line",
         no_hs_deg = "Percent individuals age 25 or over with less than high school degree",
         pct_tribal_area = "Percent of the Census tract that is within Tribal areas",
         population = "Total population") %>% 
  select(-`County Name`)

#df_sj_crb <- replace(df_sj_crb, is.na(df_sj_crb), 0) #replacing NA's with 0
# Function to replace NA with 0 only if the column is numeric
#replace_na_numeric <- function(x) {
#  if (is.numeric(x)) {
#    replace(x, is.na(x), 0)
#  } else {
#    x
#  }
#}

# Apply the function to each column of the dataframe
#df_sj_crb <- data.frame(lapply(df_sj_crb, replace_na_numeric))


# combining other variables
## Area ##
df_are <- df_are %>%
  rename(GEOID10 = "GEOID",
         ALAND10 = "ALAND",
         AWATER10 = "AWATER") %>% 
  select(GEOID10,ALAND10,AWATER10) # Units of area are square meters

## Income ##
## #fix geo ID format of per cap and med income df's
df_mi$GEO_ID <- substr(df_mi$GEO_ID,start = 10,stop = nchar(df_mi$GEO_ID)) #cutting off beginning numbers
df_pci$GEO_ID <- substr(df_pci$GEO_ID,start = 10,stop = nchar(df_pci$GEO_ID))

#renaming ID variable
df_mi <- df_mi %>% 
  rename(GEOID10 = "GEO_ID",
         Mean_HH_Income = "S1902_C03_001E") %>% #Need to rename other data sets too (3/18)
  select(GEOID10,Mean_HH_Income)
df_pci <- df_pci %>% 
  rename(GEOID10 = "GEO_ID",
         PerCap_Income = "B19301_001E") %>% 
  select(GEOID10,PerCap_Income)

## Race & Ethnicity ##
# rename tract id
df_re <- df_re %>% 
  rename(GEOID10 = "GEO_ID")

#fixing geo id format 
df_re$GEOID10 <- substr(df_re$GEOID10,start = 10,stop = nchar(df_re$GEOID10))

#creating race & ethnicity vars of interest (counts)
df_re <- df_re %>% 
  mutate(hisp_nw = B03002_012E-B03002_013E,
         ind_hnh = B03002_005E+B03002_015E,
         white_hnh = B03002_003E+B03002_013E,
         other_nh = B03002_006E+B03002_007E+B03002_008E+B03002_009E,
         other_h = B03002_016E+B03002_017E+B03002_018E+B03002_019E) %>% 
  rename(hisp_wnw = "B03002_012E",
         white_nh = "B03002_003E",
         ind_nh = "B03002_005E",
         black_nh = "B03002_004E",
         white_h = "B03002_013E",
         ind_h = "B03002_015E",
         black_h = "B03002_014E") %>%
  select(-NAME)

# comining df's
#merge with sj df
df_sj_crb_1 <- merge(df_sj_crb,df_are,by="GEOID10") # 
df_sj_crb_2 <- merge(df_sj_crb_1,df_mi,by="GEOID10") # Following three do not have territories
df_sj_crb_3 <- merge(df_sj_crb_2,df_pci,by="GEOID10") # 
df_sj_crb_4 <- merge(df_sj_crb_3,df_re,by="GEOID10")

# lost observations from tracts with zero population
test <- anti_join(df_sj_crb,df_sj_crb_4,by="GEOID10")
test2 <- anti_join(df_sj_crb,df_sj_crb_1)
test21 <- anti_join(df_are,df_sj_crb_1)
test3 <- anti_join(df_sj_crb_1,df_sj_crb_2)
test31 <- anti_join(df_mi,df_sj_crb_2)
test32 <- anti_join(df_mi,df_sj_crb_1)
# these obs have no population but are near "neighbors" who are disadvantaged and have expected building or ag loss

# convert area from square meters to acres 
df_sj_crb_4 <- df_sj_crb_4 %>% 
  mutate(Total_sqm = ALAND10+AWATER10,
         Total_Acres = Total_sqm*0.000247105, #converting squ meters to acres
         imperv_acres = imperv_surface*0.000247105, #converting squ meters to acres
         imperv_surface_pct = imperv_acres/Total_Acres) #creating percent imperv surface


# ---- Creating pop density variable ----
df_sj_crb_4 <- df_sj_crb_4 %>% 
  mutate(pop_dens = population/Total_Acres)

# ---- Final Data Set ----
#creating three race variables: white, Indian, and people of color
df0 <- df_sj_crb_4 %>% 
  mutate(pct_RestRace = pct_asian+pct_haw+pct_2race+pct_other) %>% 
  select("GEOID10","State/Territory",
         "population","Total_Acres","Total_Acres","pct_RestRace","pct_hisp","pct_white","pct_indian", #white assumed to be white regardless of hispanic ethnicity
         "no_hs_deg","imperv_surface","imperv_acres","imperv_surface_pct","imperv_surface_norm","pm2","pm2_norm",
         "exp_ag_loss","flood_risk",
         "fire_risk","energy_burd","no_plumb","no_plumb_norm",
         "leak_ust","leak_ust_norm","pct_tribal_area","Mean_HH_Income","PerCap_Income",
         "pop_dens","hisp_nw","ind_hnh","white_hnh","hisp_wnw",
         "white_nh","ind_nh","black_nh","other_nh","hisp_nw","white_h","black_h","ind_h","other_h") %>% 
  mutate(Mean_HH_Income = as.numeric(Mean_HH_Income),
         PerCap_Income = as.numeric(PerCap_Income),
         hisp_wnw_pct = hisp_wnw/population,
         ind_nh_pct = ind_nh/population,
         black_nh_pct = black_nh/population,
         white_nh_pct = white_nh/population,
         other_nh_pct = other_nh/population,
         hisp_nw_pct = hisp_nw/population,
         white_h_pct = white_h/population,
         black_h_pct = black_h/population,
         ind_h_pct = ind_h/population,
         other_h_pct = other_h/population)

df0 <- replace(df0, is.na(df0), "AAA") #replacing NA's with 0

#dropping census tracts with zero pop
df_empty <- df0 %>% 
  filter(population == 0)
df1 <- anti_join(df0,df_empty)

#counting how many "missings" in income vars in tracts with pop =/ 0
AAA_pci <- df1 %>% 
  filter(PerCap_Income == "AAA") #48 obs with pop missing
AAA_mi <- df1 %>% 
  filter(Mean_HH_Income == "AAA") #309 obs with pop missing
#both seem random all throughout country wit dif mixes of race. 
#For now, as of 4/3/2025, I will drop 48 obs and use pci instead of mi
df_sub_napci <- anti_join(df1,AAA_pci)
#now changing rest of variable AAA to 0
df_sub_napci[df_sub_napci == "AAA"] <- 0

df <- df_sub_napci %>%
  mutate(across(where(is.character) & !all_of("State/Territory") & !all_of("GEOID10"), as.numeric))

# Scaling race and ethnicity variables
df <- df %>% 
  mutate(hisp_wnw_pct_sc = hisp_wnw_pct*100,
         ind_nh_pct_sc = ind_nh_pct*100,
         black_nh_pct_sc = black_nh_pct*100)

# Adding Census Division variable
df <- df %>%
  mutate(census_division = case_when(
    `State/Territory` %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont") ~ "New England",
    `State/Territory` %in% c("New Jersey", "New York", "Pennsylvania") ~ "Middle Atlantic",
    `State/Territory` %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin") ~ "East North Central",
    `State/Territory` %in% c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota") ~ "West North Central",
    `State/Territory` %in% c("Delaware", "District of Columbia", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia") ~ "South Atlantic",
    `State/Territory` %in% c("Alabama", "Kentucky", "Mississippi", "Tennessee") ~ "East South Central",
    `State/Territory` %in% c("Arkansas", "Louisiana", "Oklahoma", "Texas") ~ "West South Central",
    `State/Territory` %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming") ~ "Mountain",
    `State/Territory` %in% c("Alaska", "California", "Hawaii", "Oregon", "Washington") ~ "Pacific",
    TRUE ~ "Other"  # Handles any unexpected values
  ))

#checking work that i have nine divisions accounted for
unique_divisions_count <- df %>%
  distinct(census_division) %>%
  nrow()

unique_divisions_count

# ----
# ---- Correcting Green Space Var ----
# Convert numbers to characters and pad with leading zeros
# Use mutate and str_pad to pad the numbers
df <- df %>%
  mutate(imperv_surface_padded = str_pad(imperv_surface, width = 4, pad = "0"),
         imperv_surface_pct = paste0(substr(imperv_surface_padded, 1, 2), ".", substr(imperv_surface_padded, 3, 4)),
         imperv_surface_pct = as.numeric(imperv_surface_pct))

# ---- Scaling Per Cap Income and Pop Density ----
df <- df %>% 
  mutate(scaled_MeanIncome = Mean_HH_Income / 10,000,
         scaled_PerCap_Income = PerCap_Income / 10,000,
         scaled_PopDens = pop_dens / 1000)

# ---- Converting Plumb variable to percent ----
df <- df %>% 
  mutate(no_plumb_pct = 100*no_plumb)

# ----
# ---- Load Final DF for Analysis (shortcut) ----
df <- read_csv("Data/Clean/final_regression_df.csv") # this includes urban tract info
# ----
# ---- Summary Statistics ----
sumstats <- df %>% 
  summarise(green_min = min(imperv_surface_pct),
            green_mean = mean(imperv_surface_pct),
            green_med = median(imperv_surface_pct),
            green_max = max(imperv_surface_pct),
            green_sd = sd(imperv_surface_pct),
            plumb_min = min(no_plumb_pct),
            plumb_mean = mean(no_plumb_pct),
            plumb_med = median(no_plumb_pct),
            plumb_max = max(no_plumb_pct),
            plumb_sd = sd(no_plumb_pct),
            ust_min = min(leak_ust),
            ust_mean = mean(leak_ust),
            ust_med = median(leak_ust),
            ust_max = max(leak_ust),
            ust_sd = sd(leak_ust),
            pm2_min = min(pm2),
            pm2_mean = mean(pm2),
            pm2_med = median(pm2),
            pm2_max = max(pm2),
            pm2_sd = sd(pm2),
            hisp_min = min(hisp_wnw_pct),
            hisp_mean = mean(hisp_wnw_pct),
            hisp_med = median(hisp_wnw_pct),
            hisp_max = max(hisp_wnw_pct),
            hisp_sd = sd(hisp_wnw_pct),
            ind_min = min(ind_nh_pct),
            ind_mean = mean(ind_nh_pct),
            ind_med = median(ind_nh_pct),
            ind_max = max(ind_nh_pct),
            ind_sd = sd(ind_nh_pct),
            black_min = min(black_nh_pct),
            black_mean = mean(black_nh_pct),
            black_med = median(black_nh_pct),
            black_max = max(black_nh_pct),
            black_sd = sd(black_nh_pct),
            hs_min = min(no_hs_deg),
            hs_mean = mean(no_hs_deg),
            hs_med = median(no_hs_deg),
            hs_max = max(no_hs_deg),
            hs_sd = sd(no_hs_deg),
            inc_min = min(PerCap_Income),
            inc_mean = mean(PerCap_Income),
            inc_med = median(PerCap_Income),
            inc_max = max(PerCap_Income),
            inc_sd = sd(PerCap_Income),
            pop_min = min(population),
            pop_mean = mean(population),
            pop_med = median(population),
            pop_max = max(population),
            pop_sd = sd(population),
            urb_min = min(urban_tract),
            urb_mean = mean(urban_tract),
            urb_med = median(urban_tract),
            urb_max = max(urban_tract),
            urb_sd = sd(urban_tract)) 

sumstats_long <- sumstats %>%
  pivot_longer(
    cols = everything(),            # Pivot all columns
    names_to = c(".value", "variable"),  # Separate column names into two parts
    names_pattern = "^(\\w+)_(\\w+)$"     # Regex pattern to capture column names
  ) %>% 
  select(-variable)

#exporting to excel
write_xlsx(sumstats_long, path = "Output/Tables/sumstats_Revisions_3-20.xlsx")


# ---- Correlation Matrix ----
df_corr <- df %>% #using per capita income because I dropped 48 weird ones
  rename(`Acres of Artificial Surface` = "imperv_surface_pct",
         `IHWA` = "no_plumb",
         `Leaky UST` = "leak_ust",
         `DPIA` = "pm2",
         Hispanic = "hisp_wnw_pct",
         `White (Non-Hisp)` = "white_nh_pct",
         `American Indian (Non-Hisp)` = "ind_nh_pct",
         `Black (Non-Hisp)` = "black_nh_pct",
         `Per Capita Income` = "PerCap_Income",
         #`Mean Income` = "Mean_HH_Income",
         `Without High School Degree` = "no_hs_deg",
         `Population Density` = "pop_dens") %>% 
  select("Hispanic",`American Indian (Non-Hisp)`,`Black (Non-Hisp)`,`White (Non-Hisp)`,
         `Per Capita Income`,`Without High School Degree`,
         `Acres of Artificial Surface`,`IHWA`,`Leaky UST`,
         `DPIA`,
         `Population Density`)

#creating correlation matrix (running with no grouping variables) @ tract level
corr <- round(cor(df_corr), 2)
head(corr[, 1:6])

corr_formatted <- sprintf("%.2f", corr)
corr_formatted <- matrix(corr_formatted, nrow = nrow(corr), dimnames = dimnames(corr))


#Visualizing
ggcorrplot(corr,
           lab = TRUE,
           digits = 2,
           type = "lower",
           outline.color = "white",
           insig = "blank")

corrplot(corr,
         method = "color",  # Use color to represent correlations
         type = "lower",    # Lower triangle
         addCoef.col = "black",  # Color of correlation coefficients
         number.digits = 2,      # Ensures two decimal places (e.g., 0.30)
         number.cex = 0.9,       # Adjust text size
         tl.col = "black",       # Text label color
         tl.srt = 45,            # Rotate labels for readability
         diag = FALSE)           # Exclude diagonal

# ---- Correlation Matrix: Hispanic breakdown ----
df_corr_hisp_div <- df %>% 
  rename(`White (Hisp)` = "white_h_pct",
         `American Indian (hisp)` = "ind_h_pct",
         `Black (Hisp)` = "black_h_pct",
         `Other (Hisp)` = "other_h_pct") %>% 
  mutate(`New England` = ifelse(census_division == "New England",1,0),
         `Middle Atlantic` = ifelse(census_division == "Middle Atlantic",1,0),
         `East North Central` = ifelse(census_division == "East North Central",1,0),
         `West North Central` = ifelse(census_division == "West North Central",1,0),
         `South Atlantic` = ifelse(census_division == "South Atlantic",1,0),
         `East South Central` = ifelse(census_division == "East South Central",1,0),
         `West South Central` = ifelse(census_division == "West South Central",1,0),
         `Mountain` = ifelse(census_division == "Mountain",1,0),
         `Pacific` = ifelse(census_division == "Pacific",1,0))%>% 
  select(`White (Hisp)`, `American Indian (hisp)`, `Black (Hisp)`, `Other (Hisp)`,
         `New England`, `Middle Atlantic`, `East North Central`, `West North Central`, `South Atlantic`,
         `East South Central`, `West South Central`, `Mountain`, `Pacific`)

#creating correlation matrix (running with no grouping variables) @ tract level
corr_hisp_div <- round(cor(df_corr_hisp_div), 2)
head(corr_hisp_div[, 1:6])

#Visualizing
ggcorrplot(corr_hisp_div,
           lab = TRUE,
           type = "lower",
           outline.color = "white",
           insig = "blank")

#### Hispanic race breakdown with env burdens and other vars
df_corr_hisp_env <- df %>% 
  rename(`White (Hisp)` = "white_h_pct",
         `American Indian (hisp)` = "ind_h_pct",
         `Black (Hisp)` = "black_h_pct",
         `Other (Hisp)` = "other_h_pct",
         `Acres of Artificial Surface` = "imperv_surface_pct",
         `IHWA` = "no_plumb",
         `Leaky UST` = "leak_ust",
         `DPIA` = "pm2",
         `Per Capita Income` = "PerCap_Income",
         `Without High School Degree` = "no_hs_deg",
         `Population Density` = "pop_dens") %>% 
  select(`White (Hisp)`, `American Indian (hisp)`, `Black (Hisp)`, `Other (Hisp)`,
         `Per Capita Income`,`Without High School Degree`,
         `Acres of Artificial Surface`,`IHWA`,`Leaky UST`,
         `DPIA`,
         `Population Density`)

#creating correlation matrix (running with no grouping variables) @ tract level
corr_hisp_env <- round(cor(df_corr_hisp_env), 2)
head(corr_hisp_env[, 1:6])

#Visualizing
ggcorrplot(corr_hisp_env,
           lab = TRUE,
           type = "lower",
           outline.color = "white",
           insig = "blank")

# ---- Total US Percentages ----
## Race percentages ##
df_percent_race <- df %>% 
  summarise(total_pop = sum(population),
            total_white_nh = sum(white_nh),
            total_black_nh = sum(black_nh),
            total_ind_nh = sum(ind_nh),
            total_hisp_wnw = sum(hisp_wnw),
            total_hisp_nw = sum(hisp_nw),
            total_white_h = sum(white_h),
            total_black_h = sum(black_h),
            total_ind_h = sum(ind_h),
            total_other_nh = sum(other_nh),
            total_other_h = sum(other_h)) %>% 
  mutate(pct_white_nh = total_white_nh/total_pop,
         pct_black_nh = total_black_nh/total_pop,
         pct_ind_nh = total_ind_nh/total_pop,
         pct_hispanic_wnw = total_hisp_wnw/total_pop,
         pct_hispanic_nw = total_hisp_nw/total_pop,
         pct_white_h = total_white_h/total_pop,
         pct_black_h = total_black_h/total_pop,
         pct_ind_h = total_ind_h/total_pop) %>% 
  mutate(total_other_hnh = total_pop - (total_white_nh+total_black_nh+total_ind_nh+total_hisp_wnw),
         pct_other_hnh = total_other_hnh/total_pop,
         pct_other_nh = total_other_nh/total_pop,
         pct_other_h = total_other_h/total_pop)

write_xlsx(df_percent_race,"output/race_percents_US_4.9.25.xlsx")

#quick irrigated acres count:


## Plumbing percentages ##
df_hh <- df_hh %>% 
  rename(GEOID10 = "GEO_ID")

df_hh$GEOID10 <- substr(df_hh$GEOID10,start = 10,stop = nchar(df_hh$GEOID10))

df_plumb <- merge(df,df_hh,by="GEOID10")

df_plumb <- df_plumb %>% 
  mutate(no_plumb_hh = B25001_001E * (no_plumb))

## How many households have no plumbing in US? 
ct_hh_no_plumb_total_us <- sum(df_plumb$no_plumb_hh)

ct_hh_total_US <- sum(df_plumb$B25001_001E)

pct_hh_no_plumb_total_us <- ct_hh_no_plumb_total_us/ct_hh_total_US
print(pct_hh_no_plumb_total_us)

# ---- Figures: nonwhite percentages ----
df <- df %>% #changing race pop sums because new df is already counts so dont need to backhand percents
  mutate(non_white_pop = hisp_nw+ind_nh+black_nh+other_nh) # Need to add the non-Hispanic "other" categories as non white (10/6/24)

df_fig <- df %>% 
  mutate(nonwhite_pct = non_white_pop/population) %>% 
  select(GEOID10,nonwhite_pct)

#exporting to excel to import in Arc GIS
write_xlsx(df_fig,"Output/Tables/Nonwhite_GIS_Figure_Data_US_4.15.25.xlsx")

# ----
# ---- Model: DPIA ----

# Model with pop dens
pm2 <- lm(pm2 ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct  #excluding white (Non-Hisp) and other groups
          + scaled_PerCap_Income + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(pm2)

bptest(pm2)
pm2_se <- coeftest(pm2, vcov = vcovHC(pm2, type = "HC3"))
coeftest(pm2, vcov = vcovHC(pm2, type = "HC3"))

#model with rural indicator
pm2_rural <- lm(pm2 ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct  #excluding white (Non-Hisp) and other groups
                + scaled_PerCap_Income + rural_tract + factor(`State/Territory`), data = df) #including state FE
summary(pm2_rural)

bptest(pm2_rural)
pm2_rural_se <- coeftest(pm2_rural, vcov = vcovHC(pm2_rural, type = "HC3"))
coeftest(pm2_rural, vcov = vcovHC(pm2_rural, type = "HC3"))

#calculating VIF
vif_pm2 <- vif(pm2_rural)
vif_pm2

# Alternative VIF calc without car package
calculate_vif <- function(pm2) {
  # Extract model matrix
  model_matrix <- model.matrix(pm2)
  
  # Calculate VIF for each variable
  vif_values <- numeric(ncol(model_matrix))
  for (i in 2:ncol(model_matrix)) {
    vif_model <- lm(model_matrix[,i] ~ model_matrix[,-i])
    vif_values[i] <- 1 / (1 - summary(vif_model)$r.squared)
  }
  
  # Name the VIF values
  names(vif_values) <- colnames(model_matrix)
  
  # Remove intercept VIF
  vif_values <- vif_values[-1]
  
  return(vif_values)
}

# Assuming you have a model called 'my_model'
vif_results <- calculate_vif(pm2)
print(vif_results)

# ---- Model: Leaky UST ----

# Model
ust <- lm(leak_ust ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct #ecluding white (Non-Hisp) and other groups
          + scaled_PerCap_Income + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(ust)

bptest(ust)
ust_se <- coeftest(ust, vcov = vcovHC(ust, type = "HC3"))
coeftest(ust, vcov = vcovHC(ust, type = "HC3"))

# model with rural indicator
ust_rural <- lm(leak_ust ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct #ecluding white (Non-Hisp) and other groups
          + scaled_PerCap_Income + rural_tract + factor(`State/Territory`), data = df) #including state FE
summary(ust_rural)

bptest(ust_rural)
ust_rural_se <- coeftest(ust_rural, vcov = vcovHC(ust_rural, type = "HC3"))
coeftest(ust_rural, vcov = vcovHC(ust_rural, type = "HC3"))


#calculating VIF
vif_ust <- vif(ust)
vif_ust

# ---- Model: % Acres Artificial Surface ----

# Model
green <- lm(imperv_surface_pct ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct  #ecluding white (Non-Hisp) and other groups
            + scaled_PerCap_Income + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(green)

bptest(green)
green_se <- coeftest(green, vcov = vcovHC(green, type = "HC3"))
coeftest(green, vcov = vcovHC(green, type = "HC3"))

# model with rural indicator
green_rural <- lm(imperv_surface_pct ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct  #ecluding white (Non-Hisp) and other groups
            + scaled_PerCap_Income + rural_tract + factor(`State/Territory`), data = df) #including state FE
summary(green_rural)

bptest(green_rural)
green_rural_se <- coeftest(green_rural, vcov = vcovHC(green_rural, type = "HC3"))
coeftest(green_rural, vcov = vcovHC(green_rural, type = "HC3"))


#calculating VIF
vif_green <- vif(green)
vif_green

# ---- Model: IHWA ----

# Model
plumb <- lm(no_plumb_pct ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct #ecluding white (Non-Hisp) and other groups
            + scaled_PerCap_Income + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(plumb)

bptest(plumb)
plumb_se <- coeftest(plumb, vcov = vcovHC(plumb, type = "HC3"))
coeftest(plumb, vcov = vcovHC(plumb, type = "HC3"))

# model with rural indicator
plumb_rural <- lm(no_plumb_pct ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct #ecluding white (Non-Hisp) and other groups
            + scaled_PerCap_Income + rural_tract + factor(`State/Territory`), data = df) #including state FE
summary(plumb_rural)

bptest(plumb_rural)
plumb_rural_se <- coeftest(plumb_rural, vcov = vcovHC(plumb_rural, type = "HC3"))
coeftest(plumb_rural, vcov = vcovHC(plumb_rural, type = "HC3"))


#calculating VIF
vif_plumb <- vif(plumb)
vif_plumb

# ---- Heterogeneity by region: Setup ----
# Create census divisions
df <- df %>%
  mutate(
    census_region = case_when(
      census_division %in% c("New England", "Middle Atlantic") ~ "Northeast",
      census_division %in% c("East North Central", "West North Central") ~ "Midwest",
      census_division %in% c("South Atlantic", "East South Central", "West South Central") ~ "South",
      census_division %in% c("Mountain", "Pacific") ~ "West",
      TRUE ~ NA_character_
    ),
    census_region = factor(
      census_region,
      levels = c("Northeast", "Midwest", "South", "West")
    )
  )

table(df$census_division, df$census_region, useNA = "ifany")
table(df$census_region, useNA = "ifany")
# ---- Heterogeneity by region: DPIA ----
# model (with pop dens)
pm2_region_int <- lm(
  pm2 ~ (hisp_wnw_pct + ind_nh_pct + black_nh_pct) * census_region +
    scaled_PerCap_Income + scaled_PopDens,
  data = df
)

coeftest(pm2_region_int, vcov = vcovHC(pm2_region_int, type = "HC3"))

# Are all demographic-region interactions jointly zero?
linearHypothesis(
  pm2_region_int,
  c(
    "hisp_wnw_pct:census_regionMidwest = 0",
    "hisp_wnw_pct:census_regionSouth = 0",
    "hisp_wnw_pct:census_regionWest = 0",
    "ind_nh_pct:census_regionMidwest = 0",
    "ind_nh_pct:census_regionSouth = 0",
    "ind_nh_pct:census_regionWest = 0",
    "black_nh_pct:census_regionMidwest = 0",
    "black_nh_pct:census_regionSouth = 0",
    "black_nh_pct:census_regionWest = 0"
  ),
  vcov. = vcovHC(pm2_region_int, type = "HC3"),
  test = "F"
)

# F stat is 383.59 and Pr(>F) < 2.2e-16
# An F-test rejects/passes the null that demographic slopes are equal across Census regions.

# Recover region-specific marginal slopes
emtrends(pm2_region_int, ~ census_region, var = "hisp_wnw_pct")
emtrends(pm2_region_int, ~ census_region, var = "ind_nh_pct")
emtrends(pm2_region_int, ~ census_region, var = "black_nh_pct")

# ---- Heterogeneity by region: LUST ----
# model (with pop dens)
ust_region_int <- lm(
  leak_ust ~ (hisp_wnw_pct + ind_nh_pct + black_nh_pct) * census_region +
    scaled_PerCap_Income + scaled_PopDens,
  data = df
)

coeftest(ust_region_int, vcov = vcovHC(ust_region_int, type = "HC3"))

# Are all demographic-region interactions jointly zero?
linearHypothesis(
  ust_region_int,
  c(
    "hisp_wnw_pct:census_regionMidwest = 0",
    "hisp_wnw_pct:census_regionSouth = 0",
    "hisp_wnw_pct:census_regionWest = 0",
    "ind_nh_pct:census_regionMidwest = 0",
    "ind_nh_pct:census_regionSouth = 0",
    "ind_nh_pct:census_regionWest = 0",
    "black_nh_pct:census_regionMidwest = 0",
    "black_nh_pct:census_regionSouth = 0",
    "black_nh_pct:census_regionWest = 0"
  ),
  vcov. = vcovHC(ust_region_int, type = "HC3"),
  test = "F"
)

# F stat is 9162.15 and Pr(>F) < 2.2e-16
# An F-test rejects/passes the null that demographic slopes are equal across Census regions.

# Recover region-specific marginal slopes
emtrends(ust_region_int, ~ census_region, var = "hisp_wnw_pct")
emtrends(ust_region_int, ~ census_region, var = "ind_nh_pct")
emtrends(ust_region_int, ~ census_region, var = "black_nh_pct")
# ---- Heterogeneity by region: Green ----
# model (with pop dens)
green_region_int <- lm(
  imperv_surface_pct ~ (hisp_wnw_pct + ind_nh_pct + black_nh_pct) * census_region +
    scaled_PerCap_Income + scaled_PopDens,
  data = df
)

coeftest(green_region_int, vcov = vcovHC(green_region_int, type = "HC3"))

# Are all demographic-region interactions jointly zero?
linearHypothesis(
  green_region_int,
  c(
    "hisp_wnw_pct:census_regionMidwest = 0",
    "hisp_wnw_pct:census_regionSouth = 0",
    "hisp_wnw_pct:census_regionWest = 0",
    "ind_nh_pct:census_regionMidwest = 0",
    "ind_nh_pct:census_regionSouth = 0",
    "ind_nh_pct:census_regionWest = 0",
    "black_nh_pct:census_regionMidwest = 0",
    "black_nh_pct:census_regionSouth = 0",
    "black_nh_pct:census_regionWest = 0"
  ),
  vcov. = vcovHC(green_region_int, type = "HC3"),
  test = "F"
)

# F stat is 993.18 and Pr(>F) < 2.2e-16
# An F-test rejects/passes the null that demographic slopes are equal across Census regions.

# Recover region-specific marginal slopes
emtrends(green_region_int, ~ census_region, var = "hisp_wnw_pct")
emtrends(green_region_int, ~ census_region, var = "ind_nh_pct")
emtrends(green_region_int, ~ census_region, var = "black_nh_pct")
# ---- Heterogeneity by region: IHWA ----
# model (with pop dens)
plumb_region_int <- lm(
  no_plumb_pct ~ (hisp_wnw_pct + ind_nh_pct + black_nh_pct) * census_region +
    scaled_PerCap_Income + scaled_PopDens,
  data = df
)

coeftest(plumb_region_int, vcov = vcovHC(plumb_region_int, type = "HC3"))

# Are all demographic-region interactions jointly zero?
linearHypothesis(
  plumb_region_int,
  c(
    "hisp_wnw_pct:census_regionMidwest = 0",
    "hisp_wnw_pct:census_regionSouth = 0",
    "hisp_wnw_pct:census_regionWest = 0",
    "ind_nh_pct:census_regionMidwest = 0",
    "ind_nh_pct:census_regionSouth = 0",
    "ind_nh_pct:census_regionWest = 0",
    "black_nh_pct:census_regionMidwest = 0",
    "black_nh_pct:census_regionSouth = 0",
    "black_nh_pct:census_regionWest = 0"
  ),
  vcov. = vcovHC(plumb_region_int, type = "HC3"),
  test = "F"
)

# F stat is 932.193 and Pr(>F) < 2.2e-16
# An F-test rejects/passes the null that demographic slopes are equal across Census regions.

# Recover region-specific marginal slopes
emtrends(plumb_region_int, ~ census_region, var = "hisp_wnw_pct")
emtrends(plumb_region_int, ~ census_region, var = "ind_nh_pct")
emtrends(plumb_region_int, ~ census_region, var = "black_nh_pct")
# ---- Heterogeneity by Rural/urban ----
# without pop dens and scaled race to interpret as 1% change
pm2_urban_int_nodens <- lm(
  pm2 ~ (hisp_wnw_pct + ind_nh_pct + black_nh_pct) * urban_tract +
    scaled_PerCap_Income +
    factor(`State/Territory`),
  data = df
)

coeftest(pm2_urban_int_nodens, vcov = vcovHC(pm2_urban_int_nodens, type = "HC3"))

# with popdens
pm2_urban_int_dens <- lm(
  pm2 ~ (hisp_wnw_pct + ind_nh_pct + black_nh_pct) * urban_tract +
    scaled_PerCap_Income + scaled_PopDens +
    factor(`State/Territory`),
  data = df
)
coeftest(pm2_urban_int_dens, vcov = vcovHC(pm2_urban_int_dens, type = "HC3"))


#ftest 
linearHypothesis(
  pm2_urban_int,
  c(
    "hisp_wnw_pct:urban_tract = 0",
    "ind_nh_pct:urban_tract = 0",
    "black_nh_pct:urban_tract = 0"
  ),
  vcov. = vcovHC(pm2_urban_int, type = "HC3"),
  test = "F"
)



# ----
# ---- Exporting Models to Excel Table ----
tidy_green_se <- tidy(green_se)
write_xlsx(tidy_green_se, path = "Output/green_model_final_US.xlsx")

tidy_plumb_se <- tidy(plumb_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model_final_US.xlsx")

tidy_ust_se <- tidy(ust_se)
write_xlsx(tidy_ust_se, path = "Output/ust_model_final_US.xlsx")

tidy_pm2_se <- tidy(pm2_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model_final_US.xlsx")
# ---- Forest Plots ----
## DPIA model
pm2_tidy <- tidy(pm2_se, conf.int = T)
pm2_tidy <- pm2_tidy[1:7, ]

# Prepare data for forest plot
coef_data <- data.frame(
  label = c("(Intercept", "Hispanic", "American Indian", "Black",
            "No HS Degree", "Mean Income (Scaled $10,000)", 
            "Population Density (Scaled 100 acres)"),
  mean  = pm2_tidy$estimate,
  lower = pm2_tidy$conf.low,
  upper = pm2_tidy$conf.high
)

# Custom function to round to two decimal places only if non-zero
custom_format <- function(x) {
  if (abs(x) < 0.01 && x != 0) {
    return(format(round(x, digits = 4), nsmall = 4, scientific = FALSE))
  } else {
    return(format(round(x, 2), nsmall = 2))
  }
}

# Function to add stars to significant estimates
add_stars <- function(mean, lower, upper) {
  if (lower > 0 | upper < 0) {
    return(paste0(custom_format(mean), "***"))
  } else {
    return(custom_format(mean))
  }
}

# Update labels with significance stars
coef_data$label_with_stars <- mapply(add_stars, coef_data$mean, coef_data$lower, coef_data$upper)

# Plotting with ggplot2 (flipped axes)
ggplot(coef_data, aes(x = mean, y = label)) +
  geom_point(size = 3, color = "#1E90FF") +  # Point estimates
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .1, size = .75, color = "#000080") +  # Horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Zero reference line
  geom_text(aes(label = label_with_stars, x = mean, y = label), vjust = -1.35, hjust = -0.21, size = 5, color = "black") +  # Labels with stars
  labs(x = "Coefficient Estimate", y = "Variable",
       title = "Dependent Variable: DPIA") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0, size = 12),  # Adjust y-axis label alignment and size
        axis.text.x = element_text(size = 12),             # Adjust x-axis text size
        axis.title = element_text(size = 14),              # Adjust axis titles size
        plot.title = element_text(size = 18, face = "bold"),  # Adjust plot title size and make it bold
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        plot.margin = unit(c(2, 1, 10, 1), "lines"))  # Adjust y-axis label alignment             

## Leaky UST Model ##
ust_tidy <- tidy(ust_se, conf.int = T)
ust_tidy <- ust_tidy[1:7, ]

# Prepare data for forest plot
coef_data <- data.frame(
  label = c("(Intercept", "Hispanic", "American Indian", "Black",
            "No HS Degree", "Mean Income (Scaled $10,000)", 
            "Population Density (Scaled 100 acres)"),
  mean  = ust_tidy$estimate,
  lower = ust_tidy$conf.low,
  upper = ust_tidy$conf.high
)

# Custom function to round to two decimal places only if non-zero
custom_format <- function(x) {
  if (abs(x) < 0.01 && x != 0) {
    return(format(round(x, digits = 4), nsmall = 4, scientific = FALSE))
  } else {
    return(format(round(x, 2), nsmall = 2))
  }
}

# Function to add stars to significant estimates
add_stars <- function(mean, lower, upper) {
  if (lower > 0 | upper < 0) {
    return(paste0(custom_format(mean), "***"))
  } else {
    return(custom_format(mean))
  }
}

# Update labels with significance stars
coef_data$label_with_stars <- mapply(add_stars, coef_data$mean, coef_data$lower, coef_data$upper)

# Plotting with ggplot2 (flipped axes)
ggplot(coef_data, aes(x = mean, y = label)) +
  geom_point(size = 3, color = "#1E90FF") +  # Point estimates
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .1, size = .75, color = "#000080") +  # Horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Zero reference line
  geom_text(aes(label = label_with_stars, x = mean, y = label), vjust = -1.35, hjust = -0.21, size = 5, color = "black") +  # Labels with stars
  labs(x = "Coefficient Estimate", y = "Variable",
       title = "Dependent Variable: Leaky USTs") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0, size = 12),  # Adjust y-axis label alignment and size
        axis.text.x = element_text(size = 12),             # Adjust x-axis text size
        axis.title = element_text(size = 14),              # Adjust axis titles size
        plot.title = element_text(size = 18, face = "bold"),  # Adjust plot title size and make it bold
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        plot.margin = unit(c(2, 1, 10, 1), "lines"))  # Adjust y-axis label alignment             

## Acres Artificial Surface Model ##
green_tidy <- tidy(green_se, conf.int = T)
green_tidy <- green_tidy[1:7, ]

# Prepare data for forest plot
coef_data <- data.frame(
  label = c("(Intercept", "Hispanic", "American Indian", "Black",
            "No HS Degree", "Mean Income (Scaled $10,000)", 
            "Population Density (Scaled 100 acres)"),
  mean  = green_tidy$estimate,
  lower = green_tidy$conf.low,
  upper = green_tidy$conf.high
)

# Custom function to round to two decimal places only if non-zero
custom_format <- function(x) {
  if (abs(x) < 0.01 && x != 0) {
    return(format(round(x, digits = 4), nsmall = 4, scientific = FALSE))
  } else {
    return(format(round(x, 2), nsmall = 2))
  }
}

# Function to add stars to significant estimates
add_stars <- function(mean, lower, upper) {
  if (lower > 0 | upper < 0) {
    return(paste0(custom_format(mean), "***"))
  } else {
    return(custom_format(mean))
  }
}

# Update labels with significance stars
coef_data$label_with_stars <- mapply(add_stars, coef_data$mean, coef_data$lower, coef_data$upper)

# Plotting with ggplot2 (flipped axes)
ggplot(coef_data, aes(x = mean, y = label)) +
  geom_point(size = 3, color = "#1E90FF") +  # Point estimates
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .1, size = .75, color = "#000080") +  # Horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Zero reference line
  geom_text(aes(label = label_with_stars, x = mean, y = label), vjust = -1.35, hjust = -0.21, size = 5, color = "black") +  # Labels with stars
  labs(x = "Coefficient Estimate", y = "Variable",
       title = "Dependent Variable: Lack Green Space") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0, size = 12),  # Adjust y-axis label alignment and size
        axis.text.x = element_text(size = 12),             # Adjust x-axis text size
        axis.title = element_text(size = 14),              # Adjust axis titles size
        plot.title = element_text(size = 18, face = "bold"),  # Adjust plot title size and make it bold
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        plot.margin = unit(c(2, 1, 10, 1), "lines"))  # Adjust y-axis label alignment             

## IHWA Model ##
plumb_tidy <- tidy(plumb_se, conf.int = T)
plumb_tidy <- plumb_tidy[1:7, ]

# Prepare data for forest plot
coef_data <- data.frame(
  label = c("(Intercept", "Hispanic", "American Indian", "Black",
            "No HS Degree", "Mean Income (Scaled $10,000)", 
            "Population Density (Scaled 100 acres)"),
  mean  = plumb_tidy$estimate,
  lower = plumb_tidy$conf.low,
  upper = plumb_tidy$conf.high
)

# Custom function to round to two decimal places only if non-zero
custom_format <- function(x) {
  if (abs(x) < 0.01 && x != 0) {
    return(format(round(x, digits = 4), nsmall = 4, scientific = FALSE))
  } else {
    return(format(round(x, 2), nsmall = 2))
  }
}

# Function to add stars to significant estimates
add_stars <- function(mean, lower, upper) {
  if (lower > 0 | upper < 0) {
    return(paste0(custom_format(mean), "***"))
  } else {
    return(custom_format(mean))
  }
}

# Update labels with significance stars
coef_data$label_with_stars <- mapply(add_stars, coef_data$mean, coef_data$lower, coef_data$upper)

# Plotting with ggplot2 (flipped axes)
ggplot(coef_data, aes(x = mean, y = label)) +
  geom_point(size = 3, color = "#1E90FF") +  # Point estimates
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .1, linewidth = .75, color = "#000080") +  # Horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Zero reference line
  geom_text(aes(label = label_with_stars, x = mean, y = label), vjust = -1.35, hjust = -0.21, size = 5, color = "black") +  # Labels with stars
  labs(x = "Coefficient Estimate", y = "Variable",
       title = "Dependent Variable: IHWA") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0, size = 12),  # Adjust y-axis label alignment and size
        axis.text.x = element_text(size = 12),             # Adjust x-axis text size
        axis.title = element_text(size = 14),              # Adjust axis titles size
        plot.title = element_text(size = 18, face = "bold"),  # Adjust plot title size and make it bold
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        plot.margin = unit(c(2, 1, 10, 1), "lines"))       # Adjust plot margins (top, right, bottom, left))             






# ---- Creating new maps (3.4 revisions) ----
options(tigris_use_cache = TRUE)

# add nonwhite percent
df <- df %>%
  mutate(
    nonwhite_pct  = ifelse(population > 0, non_white_pop / population, NA_real_)
  )

# download 2019 tract geometries
st_info <- tigris::states(cb = TRUE, year = 2019, class = "sf") %>%
  st_drop_geometry() %>%
  select(STATEFP, STUSPS, NAME) %>%
  filter(!STUSPS %in% c("AS", "GU", "MP", "VI"))  # drop small territories

tracts_2019 <- purrr::map_dfr(st_info$STATEFP, function(fp) {
  message("Downloading tracts for STATEFP=", fp)
  tigris::tracts(state = fp, year = 2019, cb = TRUE, class = "sf")
}) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(GEOID, STATEFP, COUNTYFP, TRACTCE, NAME, geometry)

# download urban areas
ua <- tigris::urban_areas(cb = TRUE, year = 2019, class = "sf") %>%
  st_make_valid()

# For correct area calculations, project to an equal-area CRS (US National Atlas Equal Area)
ea_crs <- 9311

# Make geometry valid *before* transforming (often reduces intersection issues)
tracts_ea <- tracts_2019 %>%
  st_make_valid() %>%
  st_transform(ea_crs)

ua_ea <- ua %>%
  st_make_valid() %>%
  st_transform(ea_crs)

# Compute tract areas (in m^2 in this CRS)
tracts_ea <- tracts_ea %>%
  mutate(tract_area_m2 = st_area(geometry))

#Intersect and compute urban area within each tract
#    (use only needed columns; ua has UACE10/NAME10)
int <- suppressWarnings(
  st_intersection(
    tracts_ea %>% select(GEOID),
    ua_ea %>% select(UACE10, NAME10)
  )
)

urban_area_by_tract <- int %>%
  mutate(int_area_m2 = st_area(geometry)) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(urban_area_m2 = sum(int_area_m2), .groups = "drop")

# Join back and compute urban share + indicator (if urban block covers >=50% of tract area, I consider tract urban)
# block classified as urban: must contain 2000 housing units and at least 5000 people.
tracts_classified <- tracts_ea %>%
  left_join(urban_area_by_tract, by = "GEOID") %>%
  mutate(
    urban_area_m2 = ifelse(is.na(urban_area_m2), set_units(0, "m^2"), urban_area_m2),
    urban_share_land = as.numeric(urban_area_m2 / tract_area_m2),
    urban_tract = as.integer(urban_share_land >= 0.5),
    rural_tract = 1L - urban_tract
  ) %>%
  select(GEOID, urban_share_land, urban_tract, rural_tract, geometry)

# Transform back to a GIS-friendly lon/lat CRS if you want (ArcGIS can handle either)
tracts_classified <- st_transform(tracts_classified, 4326)

# join to clean df
tracts_joined <- tracts_classified %>%
  left_join(df, by = c("GEOID" = "GEOID10"))

tracts_joined2 <- tracts_classified %>%
  left_join(df, by = c("GEOID" = "GEOID10"))

out_gpkg <- "Output/GIS/tracts_2019_EJ_with_urban_rural.gpkg"
dir.create(dirname(out_gpkg), recursive = TRUE, showWarnings = FALSE)

st_write(tracts_joined2, out_gpkg, layer = "tracts_20192", delete_layer = TRUE)
