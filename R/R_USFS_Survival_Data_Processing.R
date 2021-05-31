## Cleaning up and maanging data for Survival Analysis 

#load necessary packages:
library(tidyverse) ## Loads a suite of packages that focus on data tidying, visualization, and organization...
library(survminer)
library(survival)
library(lubridate)
library(rstanarm)
library(tidybayes)
library(stringr)

## PART 1: Import & Tidy data #######

# 1.1) Load your data:

# tidyverse includes the "readr" package, which includes commands for importing spreadsheets (.csv, .xls files) into R.

setwd("~/Desktop/Survival_Analysis/Oct_presentation//")

##load required data after setting your working directory.
## Load the USFS_PALS_MYTR data from <link here>
pals <- read_csv("~/Desktop/Survival_Analysis/DATA/USFS_PALS_MYTR_adjusted_3-2019.csv") 
head(pals)  

## Load the adjusted by admin region timber harvest data. Last downloaded July 2019
th_r01 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TH_c20201019_survana_reg01.csv")
th_r01 <- th_r01 %>% select(-ADMIN_FO_1)
head(th_r01)

th_r02 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TH_c20201019_survana_reg02.csv")
th_r02 <- th_r02 %>% select(-ADMIN_FO_1)
head(th_r02)

th_r03 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TH_c20201019_survana_reg03.csv")
th_r03 <- th_r03 %>% select(-ADMIN_FO_1)
head(th_r03)

th_r04 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TH_c20201019_survana_reg04.csv")
th_r04 <- th_r04 %>% select(-ADMIN_FO_1)
head(th_r04)

th_r05 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TH_c20201019_survana_reg05.csv")
th_r05 <- th_r05 %>% select(-ADMIN_FO_1)
head(th_r05)

th_r06 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TH_c20201019_survana_reg06.csv")
th_r06 <- th_r06 %>% select(-ADMIN_FO_1)
head(th_r06)

## Load the hazardous fuels treatment by region

hf_r01 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_HF_c20201019_survana_reg01.csv")
#hf_r01 <- hf_r01 %>% select(-ADMIN_FORE_1)
head(hf_r01)

hf_r02 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_HF_c20201019_survana_reg02.csv")
#hf_r02 <- hf_r02 %>% select(-ADMIN_FORE_1)
head(hf_r02)

hf_r03 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_HF_c20201019_survana_reg03.csv")
#hf_r03 <- hf_r03 %>% select(-ADMIN_FORE_1)
head(hf_r03)

hf_r04 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_HF_c20201019_survana_reg04.csv")
#hf_r04 <- hf_r04 %>% select(-ADMIN_FORE_1)
head(hf_r04)

hf_r05 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_HF_c20201019_survana_reg05.csv")
#hf_r05 <- hf_r05 %>% select(-ADMIN_FORE_1)
head(hf_r05)

hf_r06 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_HF_c20201019_survana_reg06.csv")
#hf_r06 <- hf_r06 %>% select(-ADMIN_FORE_1)
head(hf_r06)

## Load the reforestation treatment by region (2018 data with NEPA project values)

rf_r01 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_RF_c20201019_survana_reg01.csv")
head(rf_r01)

rf_r02 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_RF_c20201019_survana_reg02.csv")
head(rf_r02)

rf_r03 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_RF_c20201019_survana_reg03.csv")
head(rf_r03)

rf_r04 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_RF_c20201019_survana_reg04.csv")
head(rf_r04)

rf_r05 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_RF_c20201019_survana_reg05.csv")
head(rf_r05)

rf_r06 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_RF_c20201019_survana_reg06.csv")
head(rf_r06)

## Load the timber stand improvement treatment by region (2018 data with NEPA project values)

tsi_r01 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TSI_c20201019_survana_reg01.csv")
#tsi_r01 <- tsi_r01 %>% select(-ADMIN_FORE_1)
head(tsi_r01)

tsi_r02 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TSI_c20201019_survana_reg02.csv")
#tsi_r02 <- tsi_r02 %>% select(-ADMIN_FORE_1)
head(tsi_r02)

tsi_r03 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TSI_c20201019_survana_reg03.csv")
#tsi_r03 <- tsi_r03 %>% select(-ADMIN_FORE_1)
head(tsi_r03)

tsi_r04 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TSI_c20201019_survana_reg04.csv")
#tsi_r04 <- tsi_r04 %>% select(-ADMIN_FORE_1)
head(tsi_r04)

tsi_r05 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TSI_c20201019_survana_reg05.csv")
#tsi_r05 <- tsi_r05 %>% select(-ADMIN_FORE_1)
head(tsi_r05)

tsi_r06 <- read_csv("~/Desktop/Survival_Analysis/DATA/ID_TSI_c20201019_survana_reg06.csv")
#tsi_r06 <- tsi_r06 %>% select(-ADMIN_FORE_1)
head(tsi_r06)


## Subset the USFS_PALS_MYTR data
pals_sub <- pals %>% 
  filter(REGION_ID == 01 | REGION_ID == 02 | REGION_ID == 03 | REGION_ID == 04 | REGION_ID == 05 | REGION_ID == 06)

pals_sub <- pals_sub %>% 
  select(`PROJECT NUMBER`, `PROJECT NAME`,  `REGION_ID`,  `PROJECT STATUS`, `INITIATION DATE`, `DECISION SIGNED`, analysisType, `APPEALED OR OBJECTED?`,
         `LITIGATED?`, `ELAPSED DAYS`, DECISION_LEVEL)

## Rename columns
pals_sub <- rename(pals_sub, PROJECT_NUMBER = `PROJECT NUMBER`, NEPA_NAME = `PROJECT NAME`, REGION = `REGION_ID`, NEPA_STATUS =  `PROJECT STATUS`,
                   INIT_DATE = `INITIATION DATE`, DECISION_DATE = `DECISION SIGNED`, NEPA_TYPE = analysisType, APPEALED = `APPEALED OR OBJECTED?`, 
                   LITIGATED = `LITIGATED?`, ELAPSED_DAYS = `ELAPSED DAYS`)
head(pals_sub)



## Remove specific projects by number (will remove some projects by name later in the code) from the Pals data

# Creating this vector doesn't actually do anything. It was helpful as a reference when running the susequent code to remove projects by number. 
bad.proj.num <- c(16397, 16397, 26667, 26675, 26830, 30107,32001, 32002, 40302,
                  40303, 10314, 10548, 10799, 12521, 23718, 24228, 24244, 24248,
                  26659, 26661, 26668, 26693, 26814, 26874, 26874, 27043, 27059,
                  27258, 27334, 30403, 40311, 46101, 10529, 23943, 24007, 24055,
                  24255, 26478, 26562, 26596, 26599, 26648, 26764, 26842, 26883,
                  27112, 27130, 27162, 27252, 35604, 40314, 55024, 23995, 24190,
                  24240, 26634, 26672, 26822, 26837, 26878, 26957, 27086, 27140,
                  30206, 30500, 49023, 49030, 52409, 55005)



pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 16397 & PROJECT_NUMBER != 26667 & PROJECT_NUMBER != 26675 & PROJECT_NUMBER != 26830 & PROJECT_NUMBER != 30107 & PROJECT_NUMBER != 32001)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 32002 & PROJECT_NUMBER != 40302 & PROJECT_NUMBER != 40303 & PROJECT_NUMBER != 10314 & PROJECT_NUMBER != 10548 & PROJECT_NUMBER != 10799)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 12521 & PROJECT_NUMBER != 23718 & PROJECT_NUMBER != 24228 & PROJECT_NUMBER != 24244 & PROJECT_NUMBER != 24248)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 26659 & PROJECT_NUMBER != 26661 & PROJECT_NUMBER != 26668 & PROJECT_NUMBER != 26693 & PROJECT_NUMBER != 26814 & PROJECT_NUMBER != 26874) 

pals_sub <- pals_sub %>% 
  filter(PROJECT_NUMBER != 26874 & PROJECT_NUMBER != 27043 & PROJECT_NUMBER != 27059 & PROJECT_NUMBER != 27258 & PROJECT_NUMBER != 27334 & PROJECT_NUMBER != 30403)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 40311 & PROJECT_NUMBER != 46101 & PROJECT_NUMBER != 10529 & PROJECT_NUMBER != 23943 & PROJECT_NUMBER != 24007 & PROJECT_NUMBER != 24055)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 24255 & PROJECT_NUMBER != 26478 & PROJECT_NUMBER != 26562 & PROJECT_NUMBER != 26596 & PROJECT_NUMBER != 26599 & PROJECT_NUMBER != 26648) 

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 26764 & PROJECT_NUMBER != 26842 & PROJECT_NUMBER != 26883 & PROJECT_NUMBER != 27112 & PROJECT_NUMBER != 27130 & PROJECT_NUMBER != 27162)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 27252 & PROJECT_NUMBER != 35604 & PROJECT_NUMBER != 40314 & PROJECT_NUMBER != 55024 & PROJECT_NUMBER != 23995 & PROJECT_NUMBER != 24190)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 24240 & PROJECT_NUMBER != 26634 & PROJECT_NUMBER != 26672 & PROJECT_NUMBER != 26822 & PROJECT_NUMBER != 26837 & PROJECT_NUMBER != 26878) 

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 26957 & PROJECT_NUMBER != 27086 & PROJECT_NUMBER != 27140 & PROJECT_NUMBER != 30206 & PROJECT_NUMBER != 30500 & PROJECT_NUMBER != 49023) 

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 49030 & PROJECT_NUMBER != 52409 & PROJECT_NUMBER != 55005 & PROJECT_NUMBER != 26473 & PROJECT_NUMBER != 26774 & PROJECT_NUMBER != 26796)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 27014 & PROJECT_NUMBER != 27034 & PROJECT_NUMBER != 27039 & PROJECT_NUMBER != 27052 & PROJECT_NUMBER != 27088 & PROJECT_NUMBER != 27093)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 27239 & PROJECT_NUMBER != 30007 & PROJECT_NUMBER != 30303 & PROJECT_NUMBER != 30601 & PROJECT_NUMBER != 10001 & PROJECT_NUMBER != 19981)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 23051 & PROJECT_NUMBER != 23711 & PROJECT_NUMBER != 23979 & PROJECT_NUMBER != 24230 & PROJECT_NUMBER != 26706 & PROJECT_NUMBER != 27097)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 30001 & PROJECT_NUMBER != 49012 & PROJECT_NUMBER != 49020 & PROJECT_NUMBER != 51307 & PROJECT_NUMBER != 52410)

pals_sub <- pals_sub %>%
  filter(PROJECT_NUMBER != 26632 & PROJECT_NUMBER != 26712 & PROJECT_NUMBER != 23763 & PROJECT_NUMBER != 1999 & PROJECT_NUMBER != 26889 & PROJECT_NUMBER != 27108 & PROJECT_NUMBER != 24002 & PROJECT_NUMBER != 27038 & PROJECT_NUMBER != 50218)



#pals_sub <- pals_sub %>%
#  filter(str_detect(NEPA_NAME, "HAPPY CAMP TOWN FIRE PROTECTION PROJECT 03") != TRUE)


## Combine the USFS Timber Harvest data (th)

th <- rbind(th_r01,th_r02, th_r03, th_r04, th_r05, th_r06)

## Only for th, remove the NEW_CODE column
#th <- th %>%
#  select(-NEW_CODE)

## Add 4 columns to each dataframe - th, hf, rf, tsi
## fill with a 0 or 1 each objective

th <- add_column(th, th = 1, hf = 0, rf = 0 , tsi = 0)
head(th)

## Rename NEPA_PROJE in th to PROJECT_NUMBER
th <- rename(th, PROJECT_NUMBER = NEPA_PROJE)
head(th)

## Repeat for the other datasets

hf <- rbind(hf_r01, hf_r02, hf_r03, hf_r04, hf_r05, hf_r06)
hf <- add_column(hf, th = 0, hf = 1, rf = 0 , tsi = 0)
head(hf)

rf <- rbind(rf_r01, rf_r02, rf_r03, rf_r04, rf_r05, rf_r06)
rf <- add_column(rf, th = 0, hf = 0, rf = 1 , tsi = 0)
head(rf)

tsi <- rbind(tsi_r01, tsi_r02, tsi_r03, tsi_r04, tsi_r05, tsi_r06)
tsi <- add_column(tsi, th = 0, hf = 0, rf = 0 , tsi = 1)
head(tsi)

## Rename REGION_COD to ADMIN_REGION for rf and tsi
rf <- rename(rf, ADMIN_REGI = REGION_COD)
head(rf)

tsi <- rename(tsi, ADMIN_REGI = REGION_COD)
head(tsi)

## Rename NEPA_PROJE in th to PROJECT_NUMBER
hf <- rename(hf, PROJECT_NUMBER = NEPA_PROJE)
head(hf)

rf <- rename(rf, PROJECT_NUMBER = NEPA_PROJE)
head(rf)

tsi <- rename(tsi, PROJECT_NUMBER = NEPA_PROJE)
head(tsi)

## For hf only, rename ACTIVITY to ACTIVITY_N and ACTIVITY_C to ACTIVITY_2
hf <- rename(hf, ACTIVITY_N = ACTIVITY)
hf <- rename(hf, ACTIVITY_2 = ACTIVITY_C)
head(hf)

## For rf and tsi rename ACTIVITY_C to ACTIVITY_2
rf <- rename(rf, ACTIVITY_2 = ACTIVITY_C)
head(rf)
tsi <- rename(tsi, ACTIVITY_2 = ACTIVITY_C)
head(tsi)



## Join the activities datasets into one dataframe

fs_act <- rbind(th, hf, rf, tsi)

## Filter the FS_ACT data to include only projects planned after 2005. 
## This will match the data range for the USFS Pals data.

fs_act_2005 <- fs_act %>% 
  filter(SerDatesPlan >= "2005-01-01")

fs_act_2005_n <- fs_act_2005

## Remove specific projects by name
# Creating this vector doesn't actually do anything. It was helpful as a reference when running the susequent code to remove projects by name.
bad.proj.names <- c("HAPPY CAMP TOWN FIRE PROTECTION PROJECT 03", "ERICKSON THIN CHIP",
                    "DAWSON 1", "KELLY PASS YG THIN", "TAYLOR", "LITTLE GRIDER FUELBREAK",
                    "GIANT REOFFER", "LITTLE DEER/DAVIS CABIN YG TIMBER SALE", "STRAW")

fs_act_2005_n <- fs_act_2005_n %>%
  filter(str_detect(NEPA_DOC_N, "HAPPY CAMP TOWN FIRE PROTECTION PROJECT 03") != TRUE)

fs_act_2005_n <- fs_act_2005_n %>%
  filter(str_detect(NEPA_DOC_N, "ERICKSON THIN CHIP") != TRUE)

fs_act_2005_n <- fs_act_2005_n %>%
  filter(str_detect(NEPA_DOC_N, "DAWSON 1") != TRUE)

fs_act_2005_n <- fs_act_2005_n %>%
  filter(str_detect(NEPA_DOC_N, "KELLY PASS YG THIN") != TRUE)

fs_act_2005_n <- fs_act_2005_n %>%
  filter(str_detect(NEPA_DOC_N, "TAYLOR") != TRUE)

fs_act_2005_n <- fs_act_2005_n %>%
  filter(str_detect(NEPA_DOC_N, "LITTLE GRIDER FUELBREAK") != TRUE)

fs_act_2005_n <- fs_act_2005_n %>%
  filter(str_detect(NEPA_DOC_N, "GIANT REOFFER") != TRUE)

fs_act_2005_n <- fs_act_2005_n %>%
  filter(str_detect(NEPA_DOC_N, "LITTLE DEER/DAVIS CABIN YG TIMBER SALE") != TRUE)

fs_act_2005_n <- fs_act_2005_n %>%
  filter(str_detect(NEPA_DOC_N, "STRAW") != TRUE)

fs_act_2005_n <- fs_act_2005_n %>%
  filter(str_detect(NEPA_DOC_N, "MANN CREEK") != TRUE)

fs_act_2005_n <- fs_act_2005_n %>%
  filter(str_detect(NEPA_DOC_N, "JOHN DAY MISTLETOE") != TRUE)

# For pals_sub make the Project Number values characters.
pals_sub$PROJECT_NUMBER <- as.character(pals_sub$PROJECT_NUMBER)

# Create a dataframe with the subset USFS_Pals data joined to the FS_ACT_2005 dataframe by Project Number.

df_x <- pals_sub %>% 
  left_join(fs_act_2005_n, by="PROJECT_NUMBER")

# Drop NA

df_x <- df_x %>%
  drop_na(X1)
# Check the data
head(df_x)

# This is the code I used to create the dataframes I used to double check for "bad" or unmatched project numbers and names.  
proj_name_check <- df_x %>%
  select(NEPA_NAME, NEPA_DOC_N, PROJECT_NUMBER, REGION)

proj_name_check2 <- proj_name_check %>% 
  group_by(PROJECT_NUMBER) %>% 
  summarise(NEPA_NAME = paste(unique(NEPA_NAME), collapse = ', '), NEPA_DOC_N = paste(unique(NEPA_DOC_N), collapse = ','), 
            REGION = mean(REGION))


### PART 2: DATA TRANSFORMATION: ########

# Once it is organized, you may want to be able to transform your data.

# BASIC tranformation commands included in the "DPLYR" package:
# Pick observations by their values (filter()).
# Reorder the rows (arrange()).
# Pick variables by their names (select()).
# Create new variables with functions of existing variables (mutate()).
# Collapse many values down to a single summary (summarize()).

# Group the data by PROJECT_NUMBER with median time_lag

df_x <- mutate(df_x,  
             TIME_LAG.all = (SerDatesComp - SerDatesPlan))
df_x$TIME_LAG.all <- as.numeric(df_x$TIME_LAG.all)
head(df_x)

# Group again and add in data for the number of activities per project (act.count)
df_x <- df_x %>% group_by(PROJECT_NUMBER) %>% mutate(act.count=n())

# At this point df_x has the data sorted by project but still has all the data for each individual activity in the project.
# But need data summarized by project. Minimum init_date, minimum decision_date, minimum and maximum planned date and completed dates, 
# the time lag as the median time lag, the sum of the area (units) planned to be treated and actually treated, the average th, hf, rf, and tsi 
# (to show the objective(s) of the project), the average number of activities completed, and the average number of activities.

df_test02 <- summarize(group_by(df_x, PROJECT_NUMBER), init.date = min(INIT_DATE), decision.data = min(DECISION_DATE), 
                       plan.date.min = min(SerDatesPlan), plan.date.max = max(SerDatesPlan), comp.date.min = min(SerDatesComp, na.rm = TRUE), comp.date.max = max(SerDatesComp),
                       num.units.plan = as.numeric(sum(NBR_UNITS_, na.rm=TRUE)), num.units.comp = as.numeric(sum(NBR_UNITS1, na.rm=TRUE)),
                       med.time.lag = median(TIME_LAG, na.rm=TRUE), th = mean(th), hf = mean(hf), rf = mean(rf), tsi = mean(tsi),
                       completed = mean(Completed), act.count=mean(act.count))  
head(df_test02)  

# Calculate the planned project duration  
df_test02 <- mutate(df_test02, 
                    proj.dur.plan = as.numeric(plan.date.max - plan.date.min)+1)
# Calculate the project duration as completed
df_test02 <- mutate(df_test02, 
                    proj.dur.comp = as.numeric(comp.date.max - comp.date.min)+1)
# Calculate the project delay 
df_test02 <- mutate(df_test02, 
                    proj.delay = as.numeric(comp.date.min - plan.date.min))

# Check the data
head(df_test02)  

# No rejoin to the subset Pals data by project number.

df_fin <- pals_sub %>%
  left_join(df_test02, by = "PROJECT_NUMBER")

# Drop NAs in the initiation date and check the data.

df_fin <- df_fin %>%
  drop_na(init.date)
head(df_fin)

# Create a Completed column
# 0 for imcomplete projects (...) 1 for completed projects. 
df_fin$COMPLETED <- ifelse(df_fin$proj.dur.comp >= 0, 1, 0)
df_fin$COMPLETED[is.na(df_fin$COMPLETED)] <- 0

# Create an Initiated column
# 0 for uninitiated (no activity has been completed) or 1 for inititated (at least one activity in the project has been completed.)
df_fin$proj.delay[is.infinite(df_fin$proj.delay)] <- NA  
df_fin$INITIATED <- ifelse(df_fin$proj.delay >= 0, 1, 0)
df_fin$INITIATED[is.na(df_fin$INITIATED)] <- 0


## Create a new csv for all of the datasets created. 

#write.csv(df_fin, "~/Desktop/Survival_Analysis/Sept_2020/df_c20201020.csv")
#write.csv(fs_act, "~/Desktop/Survival_Analysis/Sept_2020/fs-act_c20201020.csv")
#write.csv(fs_act_2005, "~/Desktop/Survival_Analysis/Sept_2020/fs-act-2005_c20201020.csv")


### PART 3: Visualization of data #########

# 3.1) After your data is organized and transformed, the next step is to explore your data using visualization, which can help you:
#1. (initially) Assess your hypotheses and look for covariation.
#2. Notice mistakes in your data.
#3. Refine your questions and/or generate new questions.

ggplot(data = df_fin, aes(x = proj.dur.plan)) + ## specify the data we want to plot
  geom_histogram(binwidth = 50)  ## This line tells ggplot that we want to plot that data as a histogram, with bins that are 5 units wide.
## Zero Inflated or real?
ggplot(data = df_fin, aes(group = REGION, x = REGION, y=proj.dur.plan)) + 
  geom_boxplot()  +
  labs(y="Planned Project Duration (days)", x = "Admin Region")

ggplot(data = df_fin, aes(group = REGION, x = REGION, y=proj.dur.comp)) + 
  geom_boxplot()  +
  labs(y="Completed Project Duration (days)", x = "Admin Region")

ggplot(data = df_fin, aes(group = REGION, x = REGION, y=proj.delay)) + 
  geom_boxplot()  +
  labs(y="Project Delay (days)", x = "Admin Region")

ggplot(data = df_fin, aes(group = NEPA_TYPE, x = NEPA_TYPE, y=proj.dur.plan)) + 
  geom_boxplot()  +
  labs(y="Planned Project Duration (days)", x = "NEPA Analysis Type")

ggplot(data = df_fin, aes(group = NEPA_TYPE, x = NEPA_TYPE, y=proj.dur.comp)) + 
  geom_boxplot()  +
  labs(y="Completed Project Duration (days)", x = "NEPA Analysis Type")

ggplot(data = df_fin, aes(group = NEPA_TYPE, x = NEPA_TYPE, y=proj.delay)) + 
  geom_boxplot()  +
  labs(y="Project Delay (days)", x = "NEPA Analysis Type")

ggplot(data = df_fin, aes(group = NEPA_TYPE, x = ELAPSED_DAYS, y=proj.delay)) + 
  geom_point(aes(colour = factor(NEPA_TYPE)) )  +
  labs(x="Time to Complete NEPA Analysis (days)", y = "Timber Harvest Project Delay (days")

ggplot(data = df_fin, aes(group = NEPA_TYPE, x = ELAPSED_DAYS, y=proj.dur.plan)) + 
  geom_point(aes(colour = factor(NEPA_TYPE)) )  +
  labs(x="Time to Complete NEPA Analysis (days)", y = "Timber Harvest Project Planned Duration (days")

ggplot(data = df_fin, aes(group = NEPA_TYPE, x = proj.dur.plan, y=proj.dur.comp)) + 
  geom_point(aes(colour = factor(NEPA_TYPE)) )  +
  labs(y="Completed (Actual) Project Duration (days)", x = "Planned Project Duration (days")


### PART 4: Survival Analyses! See R_Survival_Analysis.Rmd ###
