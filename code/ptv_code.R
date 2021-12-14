
### load library
library(tidyverse)
library(magrittr)
library(haven) ## to read xpt file



### obtain the path for the datasets you want 
path_demo <- file.path(path_this, "P_DEMO.XPT") ## demographics data
path_mental <- file.path(path_survey, "P_DPQ.XPT") ## mental health
path_diabetes <- file.path(path_survey, "P_DIQ.XPT") ## diabetes
path_bp <- file.path(path_survey, "P_BPQ.XPT") ## blood pressure and cholesterol
path_cardio <- file.path(path_survey, "P_CDQ.XPT") ## cardiovascular health
path_body <- file.path(path_exam, "P_BMX.XPT") ## body measures
path_insurance <- file.path(path_survey, "P_HIQ.XPT") ## health insurance
path_smoking <- file.path(path_survey, "P_SMQ.XPT") ## smoking
path_sleep <- file.path(path_survey, "P_SLQ.XPT") ## sleep disorders
path_medical <- file.path(path_survey, "P_MCQ.XPT") ## medical condition

### start reading (raw) datasets in

demo_raw <- read_xpt(file = "data/raw/P_DEMO.XPT"); names(demo_raw) %<>% tolower ## this is to make all names lower-case, easier to type later

mental_raw <- read_xpt(file = "data/raw/P_DPQ.XPT"); names(demo_raw) %<>% tolower #mental health

diabetes_raw <- read_xpt(file = "data/raw/P_DIQ.XPT"); names(demo_raw) #diabetes

bp_raw <- read_xpt(file = "data/raw/P_BPQ.XPT"); names(demo_raw) #blood pressure and cholesterol

cardio_raw <- read_xpt(file = "data/raw/P_CDQ.XPT"); names(demo_raw) #cardiovascular health

body_raw <- read_xpt(path_body); names(body_raw) %<>% tolower
insurance_raw <- read_xpt(path_insurance); names(insurance_raw) %<>% tolower
physical_raw <- read_xpt(path_physical); names(physical_raw) %<>% tolower
smoking_raw <- read_xpt(path_smoking); names(smoking_raw) %<>% tolower
sleep_raw <- read_xpt(path_sleep); names(sleep_raw) %<>% tolower
medical_raw <- read_xpt(path_medical); names(medical_raw) %<>% tolower

### handle demo dataset --- the one in the quotations is the original variables 
demo <- demo_raw %>% 
  rename(subject="seqn",
         age="ridageyr", ## take only age in year
         ratio_income="indfmpir") %>%
  ## to make it easy for your lasso and random forest later, I'm going to create some new variables
  mutate(female=ifelse(is.na(riagendr), NA, riagendr-1), ## female=1, male=0
         nonhisp_white=ifelse(is.na(ridreth3), NA, as.numeric(ridreth3==3)), ## non-hispanic white=1, all other=0
         never_married=ifelse(is.na(dmdmartz), NA, as.numeric(dmdmartz==3)), ## never married = 1, all others = 0
         born_us=ifelse(is.na(dmdborn4), NA, as.numeric(dmdborn4==1)), ## born in the US = 1, others = 0
         edu_college=ifelse(is.na(dmdeduc2), NA, as.numeric(dmdeduc2 %in% c(4, 5)))) %>% ## education some college or above = 1, others = 0
  filter(age>=20) %>% ## take only 20+ year-old adults because only they have education recorded 
  select(subject, female, age, nonhisp_white, never_married, ratio_income, born_us, edu_college) 
  
### mental health/depression --- calculate a composite score that is the total of all scores
### for simplicity we add all score together, ignore question with NA value
### mental_score is going to be your outcome variable
mental <- mental_raw %>% 
  rename(subject="seqn") %>%
  mutate(mental_score=rowSums(cbind(dpq010, dpq020, dpq030, dpq040, dpq050, 
                              dpq060, dpq070, dpq080, dpq090, dpq100))) %>% 
  select(subject, mental_score) 

### diabetes
diabetes <- diabetes_raw %>%
  rename(subject="seqn") %>%
  mutate(dr_diabetes=ifelse(is.na(diq010), NA, as.numeric(diq010==1))) %>% ## doctor told participant - have diabetes
  select(subject, dr_diabetes)

### blood pressure and cholesterol
bp <- bp_raw %>%
  rename(subject="seqn") %>%
  mutate(dr_highbp=ifelse(is.na(bpq020), NA, as.numeric(bpq020==1)), ## dr told having high blood pressure
         dr_highchol=ifelse(is.na(bpq080), NA, as.numeric(bpq080==1))) %>% ## dr told having high cholesterol
  select(subject, dr_highbp, dr_highchol) 

### cardiovascular health
cardio <- cardio_raw %>%
  rename(subject="seqn") %>% 
  mutate(pain_chest=ifelse(is.na(cdq001), NA, as.numeric(cdq001==1)), ## ever have chest pain
         short_breath=ifelse(is.na(cdq010), NA, as.numeric(cdq010==1))) %>% ## shortness of breath
  select(subject, pain_chest, short_breath)

### body measures
### This file you did not download for me but I found it here
### https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&Cycle=2017-2020
### You need to download it and put it in the Examination folder for the code related to it work
body <- body_raw %>%
  rename(subject="seqn", 
         weight="bmxwt", ## this is in kg
         height="bmxht", ## this is in cm
         bmi="bmxbmi", 
         waist="bmxwaist") %>% ## waist circumference in cm 
  select(subject, weight, height, bmi, waist)

### insurance
insurance <- insurance_raw %>%
  rename(subject="seqn") %>%
  mutate(insurance=ifelse(is.na(hiq011), NA, as.numeric(hiq011==1))) %>%
  select(subject, insurance)

### smoking
smoking <- smoking_raw %>%
  rename(subject="seqn") %>%
  mutate(smoke100=ifelse(is.na(smq020), NA, as.numeric(smq020==1))) %>% ## have smoked at least 100 cigarette in life (this variable is the most promising in there)
  select(subject, smoke100)

### did not use physical activity because too many variables to work with right now

### did not use reproductive health because it is very specific to women only

### sleep disorder
sleep <- sleep_raw %>%
  rename(subject="seqn", 
         sleep_weekday="sld012",
         sleep_weekend="sld013") %>%
  mutate(dr_sleep=ifelse(is.na(slq050), NA, as.numeric(slq050==1))) %>% ## ever told doctor had trouble sleeping
  select(subject, sleep_weekday, sleep_weekend, dr_sleep)

### medical condition
medical <- medical_raw %>%
  rename(subject="seqn") %>%
  mutate(told_asthma=ifelse(is.na(mcq010), NA, as.numeric(mcq010==1)), ## ever told you had asthma
         told_arthritis=ifelse(is.na(mcq160a), NA, as.numeric(mcq160a==1)), ## ever told to have arthritis
         told_coronary=ifelse(is.na(mcq160c), NA, as.numeric(mcq160c==1)), ## ever told to have coronary heart disease
         told_attack=ifelse(is.na(mcq160e), NA, as.numeric(mcq160e==1)), ## ever told to have heart attack
         told_stroke=ifelse(is.na(mcq160f), NA, as.numeric(mcq160f==1)), ## ever told to have a stroke
         told_thyroid=ifelse(is.na(mcq160m), NA, as.numeric(mcq160m==1)), ## ever told to have thyroid problem
         told_cancer=ifelse(is.na(mcq220), NA, as.numeric(mcq220==1)), ## ever told to have cancer or magnilancy 
         told_weight=ifelse(is.na(mcq366a), NA, as.numeric(mcq366a==1)), ## ever told by health professional to lose weight
         told_exe=ifelse(is.na(mcq366b), NA, as.numeric(mcq366b==1)), ## ever told by health professional to exercise
         told_salt=ifelse(is.na(mcq366c), NA, as.numeric(mcq366c==1)), ## ever told by health professional to reduce salt in diet
         told_fat=ifelse(is.na(mcq366d), NA, as.numeric(mcq366d==1))) %>% ## ever told by health professional to lose fat/calories
  select(subject, told_asthma, told_arthritis, told_coronary,
         told_attack, told_stroke, told_thyroid, 
         told_cancer, told_weight, told_exe, told_salt, told_fat) 

### merge everything
db <- Reduce(function(x,y) left_join(x, y, by = "subject"), 
         list(mental, demo, diabetes, bp, cardio, body,
              insurance, smoking, sleep, medical)) %>%
  drop_na() ## remove all survey participants with missing data. 

write_csv(db, file="db.csv", na="")


