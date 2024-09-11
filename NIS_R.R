# Clean work space
rm(list=ls(all=T))
options(stringAsFactors = F)
options("scipen" = 100, "digits" = 4)

library(tidyverse)
library(readr)
library(stringr)
library(descr)
library(stringi)
library(survey)
library(srvyr)

setwd("/Users/M276066/Documents/R projects/NIS/")

#####
# 115 kb. metadata about cost to charge
ctoc <- read.csv("NIS_2021 (4)/ASCII files/cc2021NIS.csv") 
str(ctoc)
View(ctoc)

#####
# 290 kb - can read it in

# 64 characters long per row
# 1 1-11 = DISCWT - NIS discharge weight - 
# 2 12-13 = HOSP_BEDSIZE - relative bedsize category of hospital (1 = small, 2 = medium, 3 = large)
# 3 14-15 = HOSP_DIVISION - census division of hospital (1 = New england, 2 = mid atlantic, etc)
# 4 16-17 = HOSP_LOCTEACH - teaching status / location of hospital (1 = rural, 2 = urban nonteaching, 3 = urban teaching)
# 5 18-22 = HOSP_NIS - NIS hospital  (starts with 5.n)
# 6 23-24 = HOSP_REGION - region of hospital (1 = northeast, 2 = midwest, 3 = south, 4 = west)
# 7 25-26 = H-CONTRL - control/ownership of hospital (1 = gov, nonfederal, 2 = private nonprofit, 3 = private, invest-own)
# 8 27-30 = NIS_STRATUM - NIS hospital stratum (region 1-4, division 1-9)
# 9 31-38 = N_DISC_U - number of universe discharges in the stratum 
# 10 39-42 = N_HOSP_U - number of universe hospitals in the stratum
# 11 43-50 = S_DISC_U - Number of discharges in the sample for the stratum
# 12 51-54 = S_HOSP_U - number of hospitals in the sample for the stratum
# 13 55-60 = TOTAL_DISC - number of hospital discharges from this hospital in the NIS
# 14 61-64 = YEAR - calendar year


hospital <- read_fwf(file = "NIS_2021 (4)/ASCII files/NIS_2021_Hospital.ASC",
         col_positions = fwf_cols(11, 2, 2, 2, 5, 2, 2, 4, 8, 4, 8, 4, 6, 4))
colnames(hospital) <- c("DISCWT", "HOSP_BEDSIZE", "HOSP_DIVISION", "HOSP_LOCTEACH", "HOSP_NIS",
                        "HOSP_REGION", "H-Control", "NIS_STRATUM", "N_DISC_U", "N-HOSP_U",
                        "S_DISC_U", "S_HOSP_U", "TOTAL_DISC", "Year")


#####
# Severity - 1.5gb - Can read it in... takes awhile
severity <- read.csv("NIS_2021 (4)/ASCII files/NIS_2021_Severity.ASC")
str(severity)
View(severity[1:2,])
severity[1,]
severity[1:20,]
severity[1:5,]

# Severity has 1001, 10000099 and different numbers, 346 a different number for each, and 1-3, 1-3
# No idea what the final two numbers look like but maybe some sort of number for each person into a big category

# Want to look at severity trends. Filter through it a little bit and see if you can select subsets
# Need to read the files about what the data actually is


#####
# Core file is 4gb. Can read it in and takes awhile
core <- read.csv("NIS_2021 (4)/ASCII files/NIS_2021_Core.ASC")

# Fast read to test
# column num / columns / variable / descr
missing_values <- as.character(
  quote(c(-9,-8,-6,-5,-99,-88,-66,-999,-888,-666,-9.9,-8.8,-6.6, 
          -9999,-8888,-6666,-9.99,-8.88,-6.66,-99999,-88888,-66666,
          -99.99,-88.88,-66.66,-999.99,-888.88,-666.66,-999999,
          -888888,-666666,-9999.99,-8888.88,-6666.66,-99.9999,
          -88.8888,-66.6666,-999999999,-888888888,-666666666,-9999.9999,
          -8888.8888,-6666.6666,-999.99999,-888.88888,-666.66666,-99999999,
          -88888888,-66666666,-99.9999999,-88.8888888,-66.6666666,
          -99999999.99,-88888888.88,-66666666.66,-99999.99999,
          -88888.88888,-66666.66666,-99999999999.99,-88888888888.88,
          -66666666666.66))
          )
missing_values

keyfile <- read.table(file = "NIS_2021 (4)/ASCII files/key.txt")
colnames(keyfile) <- c("Number", "Variabl_Name", "startpos", "endpos")
keyfile

icdcodes <- read.fwf(file = "NIS_2021 (4)/ASCII files/icd10cm-codes-Jan-2021.txt",widths = c(8,50))
colnames(icdcodes) <- c("code", "diagnosis")
dim(icdcodes)
icdcodes

## Still need to get the rownames right to make this function work well
pcscodes <- read.fwf(file = "NIS_2021 (4)/ASCII files/icd10pcs_order_2021.txt", widths = c(5, 8, 61, 20))
colnames(pcscodes) <- c("#", "Code", "0/1", "Shortname", "Longname")
dim(pcscodes)
pcscodes

# read in file - takes awhile
core <- read_fwf(
  file = "NIS_2021 (4)/ASCII files/NIS_2021_Core.ASC",
  col_positions = fwf_positions(start = keyfile[,3], end = keyfile[,4]),
  progress = TRUE,
  na = missing_values)
colnames(core) <- keyfile[,2]
spec(core)
core
View(core[1:50,])

# This function takes 5-8 secs
readinICD <- function(code){ 
  subset(x = core, subset = core$I10_DX1 == code | 
    core$I10_DX2 == code  | core$I10_DX3 == code |
    core$I10_DX4 ==  code | core$I10_DX5 == code |
    core$I10_DX6 == code | core$I10_DX7 == code |
    core$I10_DX8 == code | core$I10_DX9 == code |
    core$I10_DX10 == code | core$I10_DX11 == code |
    core$I10_DX12 == code | core$I10_DX13 == code |
    core$I10_DX14 == code | core$I10_DX15 == code |
    core$I10_DX16 == code | core$I10_DX17 == code |
    core$I10_DX18 == code | core$I10_DX19 == code |
    core$I10_DX20 == code | core$I10_DX21 == code |
    core$I10_DX22 == code | core$I10_DX23 == code |
    core$I10_DX24 == code | core$I10_DX25 == code )
}
readinICD("I421")

# Sub in a second ICD code
readinICDadditional <- function(icdcode, dataset){ 
  subset(x = dataset, subset = dataset$I10_DX1 == icdcode | 
                            dataset$I10_DX2 == icdcode | dataset$I10_DX3 == icdcode |
                            dataset$I10_DX4 == icdcode | dataset$I10_DX5 == icdcode  |
                            dataset$I10_DX6 == icdcode | dataset$I10_DX7 == icdcode |
                            dataset$I10_DX8 == icdcode | dataset$I10_DX9 == icdcode |
                            dataset$I10_DX10 == icdcode | dataset$I10_DX11 == icdcode |
                            dataset$I10_DX12 == icdcode | dataset$I10_DX13 == icdcode |
                            dataset$I10_DX14 == icdcode | dataset$I10_DX15 == icdcode |
                            dataset$I10_DX16 == icdcode | dataset$I10_DX17 == icdcode |
                            dataset$I10_DX18 == icdcode | dataset$I10_DX19 == icdcode |
                            dataset$I10_DX20 == icdcode | dataset$I10_DX21 == icdcode |
                            dataset$I10_DX22 == icdcode | dataset$I10_DX23 == icdcode |
                            dataset$I10_DX24 == icdcode | dataset$I10_DX25 == icdcode )
}

# Find a procedure code
PCScode <- function(pcs, dataset){ 
  subset(x = dataset, subset = dataset$I10_PR1 == pcs | 
           dataset$I10_PR2 == pcs | dataset$I10_PR3 == pcs |
           dataset$I10_PR4 == pcs | dataset$I10_PR5 == pcs  |
           dataset$I10_PR6 == pcs | dataset$I10_PR7 == pcs |
           dataset$I10_PR8 == pcs | dataset$I10_PR9 == pcs |
           dataset$I10_PR10 == pcs | dataset$I10_PR11 == pcs |
           dataset$I10_PR12 == pcs | dataset$I10_PR13 == pcs |
           dataset$I10_PR14 == pcs | dataset$I10_PR15 == pcs |
           dataset$I10_PR16 == pcs | dataset$I10_PR17 == pcs |
           dataset$I10_PR18 == pcs | dataset$I10_PR19 == pcs |
           dataset$I10_PR20 == pcs | dataset$I10_PR21 == pcs |
           dataset$I10_PR22 == pcs | dataset$I10_PR23 == pcs |
           dataset$I10_PR24 == pcs | dataset$I10_PR25 == pcs )
}
PCScode("025L3ZZ", core)

# This function returns the diagnosis name if you input an icd code
  # Note the input has to be 8 characters
dxname <- function(codenumber) {
  for (i in 1:72621) {
    if (icdcodes[i,1] == codenumber) {
      savename <- icdcodes[i,2]
      } 
  }
  savename
}
dxname("I422    ")

# This function returns parses through procedure codes 
# Grabs data with pcs codes including any of the numbers you put in
parcepcspattern <- function(data, pattern) {
  subset_data <- data[, 63:87]
  key <- apply(subset_data, MARGIN = 1, function(x) any(grepl(pattern, x)))
  final <- data[key, ]
  return(final)
}

parcepcspattern(hcmvt,"0258")
parcepcspattern(hcm,"0258")
core_ablation <- parcepcspattern(core,"0258")

# Function - adds a dummy variable for an ICD code to core
add_dummy_variable <- function(dataset, code, new_variable_name) {
  columns <- c("I10_DX1", "I10_DX2", "I10_DX3", "I10_DX4", "I10_DX5", "I10_DX6",
               "I10_DX7", "I10_DX8", "I10_DX9","I10_DX10", "I10_DX11", "I10_DX12",
               "I10_DX13", "I10_DX14", "I10_DX15","I10_DX16", "I10_DX17", "I10_DX18",
               "I10_DX19", "I10_DX20", "I10_DX21","I10_DX22", "I10_DX23", "I10_DX24",
               "I10_DX25")
  any_column_contains_code <- apply(dataset[, columns], 1, function(x) any(x == code))
  dataset[[new_variable_name]] <- ifelse(any_column_contains_code, 1, 0)
  return(dataset)
}

add_dummy_dx1 <- function(dataset, code, new_variable_name) {
  columns <- c("I10_DX1")
  any_column_contains_code <- apply(dataset[, columns], 1, function(x) any(x == code))
  dataset[[new_variable_name]] <- ifelse(any_column_contains_code, 1, 0)
  return(dataset)
}

# Example usage:
core_with_hcm <- add_dummy_variable(core, "I421", "hcm")
core_hcm_sarcoid <- add_dummy_variable(core_with_hcm, "D8685", "sarcoid")

corehcmcovid <- add_dummy_variable(core_with_hcm, "U071", "covid")
freq(corehcmcovid$covid)

View(core_with_hcm[1:20,])
freq(core_with_hcm$hcm)

## Survey ##
## Not automized ##
# Working survey element - to get national estimates
coresvy <- svydesign(ids = ~as.factor(core$HOSP_NIS), 
                     strata = as.factor(core$NIS_STRATUM), 
                     weight = ~core$DISCWT, 
                     data = core)

coresvy


corehcmsvy <- svydesign(ids = ~as.factor(core_with_hcm$HOSP_NIS), 
                     strata = as.factor(core_with_hcm$NIS_STRATUM), 
                     weight = ~core_with_hcm$DISCWT, 
                     data = core_with_hcm)

corehcmsvy


a <- svytotal(x = ~hcm, design = corehcmsvy,na.rm = FALSE)
b <- svyvar(x = ~hcm, design = corehcmsvy,na.rm = TRUE)
c <- svymean(x = ~hcm, design = corehcmsvy,na.rm = TRUE)


svychisq()

?svychisq


svyby(~hcm, design = corehcmsvy,na.rm = FALSE)
?svyb


covid <- subset(x = core, subset = core$I10_DX1 == "U071")
dim(covid)
covidany <- readinICD("U071")
dim(covidany)
hcmonly <- subset(x = core, subset = core$I10_DX1 == "I421")
dim(hcmonly)
coredx1 <- add_dummy_dx1(core_with_hcm, "I421", "hcmdx1")
coredx1 <- add_dummy_dx1(coredx1, "U071", "coviddx1")
# test survey
coretestsvy <- svydesign(ids = ~as.factor(coredx1$HOSP_NIS), 
                         strata = as.factor(coredx1$NIS_STRATUM), 
                         weight = ~coredx1$DISCWT, 
                         data = coredx1)
d <- svytotal(x = ~hcmdx1, design = coretestsvy,na.rm = FALSE)
e <- svytotal(x = ~coviddx1, design = coretestsvy,na.rm = FALSE)


coretestsvy <- svydesign(ids = ~as.factor(corehcmcovid$HOSP_NIS), 
                         strata = as.factor(corehcmcovid$NIS_STRATUM), 
                         weight = ~corehcmcovid$DISCWT, 
                         data = corehcmcovid)

a <- svytotal(x = ~hcm, design = coretestsvy,na.rm = FALSE)
b <- svytotal(x = ~covid, design = coretestsvy,na.rm = FALSE)











# This works. need to now infer things about subgroups in here
core %>%
  count(DIED)
str(core)
View(core[1:50,])


tab_race <- core %>% group_by(RACE) %>%
  summarize(Freq = n()) %>% mutate(Prop = Freq / sum(Freq)) %>%
  arrange(desc(Prop))
tab_race

# unweighted distribution of race
ggplot(data = tab_race, aes(x = RACE, y = Prop)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = tab_race$RACE)

tab_w_race <- svytable(~RACE,design = coresvy) %>%
  as.data.frame() %>%
  mutate(Prop = Freq / sum(Freq)) %>%
  arrange(desc(Prop))
tab_w_race

# Survey weighted distribution of race
ggplot(data = tab_w_race, mapping = aes(x = RACE, y = Prop)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = tab_w_race$RACE)





core %>%
  summarize(n_hat = sum())




glimpse(coresvy)
summary(coresvy)

ggplot(data = core, aes(DISCWT)) +
  geom_histogram()
ggplot(data = core, aes(HOSP_NIS)) +
  geom_histogram()





# Can make comparisons of t tests for sex, age, race, weekend, elective, 
# time to procedures, LOS, total charge, died, 
colnames(core)


## Don't know how to look at estimates for a specific population...
## Like HCM with VT, those with ablation, etc ##






# Filter and summarize by groups
anes_des %>%
  filter(!is.na(VotedPres2016), !is.na(VotedPres2020)) %>%
  group_by(VotedPres2016, VotedPres2020) %>%
  summarize(
    p=survey_mean(),
    N=survey_total(),
    n=unweighted(n()), 
    .groups="drop"
  )

# Proportions with CI
anes_des %>%
  group_by(interact(Income7, VotedPres2016, VotedPres2020)) %>% 
  summarize(
    pd=survey_prop(vartype="ci") %>% round(4),
    pl=survey_prop(proportion = TRUE, prop_method="logit", vartype="ci") %>% round(4),
    px=survey_prop(proportion = TRUE, prop_method="likelihood", vartype="ci") %>% round(4)
  ) %>% select(Income7, VotedPres2016, VotedPres2020, contains("_")) %>%
  DT::datatable(fillContainer = FALSE, options = list(pageLength = 4))














# Hypertrophic cardiomyopathy - I421
hcm <- readinICD("I421")
str(hcm)
dim(hcm)
freq(hcm$DIED)

# Read in HCM and VT
hcmvt <- readinICDadditional(icdcode = "I472", dataset = hcm)
hcmvt
dim(hcmvt)
freq(hcmvt$DIED)
freq(hcmvt$RACE)






# Hypertrophic nonobstructive cardiomyopathy
hnocm <- readinICD("I422")
dim(hnocm)
freq(hnocm$DIED)

# sarcoidosis
sarcoid <- readinICD("D8685")
dim(sarcoid)
freq(sarcoid$DIED)

# Sysemic Amyloidosis
amyloid <- readinICD("E854")
dim(amyloid)
freq(amyloid$DIED)

# Aortic stenosis
AS <- readinICD("I350")
dim(AS)
freq(AS$DIED)

# Renal artery stenosis
RS <- readinICD("I701")
dim(RS)
freq(RS$DIED)

# subsets for practice
coretest <- read_fwf(
  file = "NIS_2021 (4)/ASCII files/NIS_2021_Core.ASC",
  col_positions = fwf_positions(start = keyfile[,3], end = keyfile[,4]),
  progress = TRUE,
  na = missing_values,
  n_max = 100)
colnames(coretest) <- keyfile[,2]
View(coretest)


# CCSR function
ccsr <- read.csv(file = "NIS_2021 (4)/CCSR.csv")
ccsr <- ccsr[-c(1:5),1:4]
colnames(ccsr) <- c("ICD10 code", "ICD Description", "CCSR code", "CCSR Description")
View(ccsr)

icdtoccsr <- function(ICD10code) {
  match_idx <- which(ccsr[, 1] == ICD10code)
  if (length(match_idx) > 0) {
    return(ccsr[match_idx, 3])
  } else {
    return(NA)
  }
}

icdtoccsr("A009")


#####
# Dx file is 6gb. Can read it in - takes even longer
dx <- read.csv("NIS_2021 (4)/ASCII files/NIS_2021_DX_PR_GRPS.ASC")
dx[1,]
# Dx file is 2 numbers, then like 18 zeros, then a long long number











# storing variable labels here ------------------------------------------------------

# core file
# label var AGE                      "Age in years at admission"
# label var AGE_NEONATE              "Neonatal age (first 28 days after birth) indicator"
# label var AMONTH                   "Admission month"
# label var AWEEKEND                 "Admission day is a weekend"
# label var DIED                     "Died during hospitalization"
# label var DISCWT                   "NIS discharge weight"
# label var DISPUNIFORM              "Disposition of patient (uniform)"
# label var DQTR                     "Discharge quarter"
# label var DRG                      "DRG in effect on discharge date"
# label var DRGVER                   "DRG grouper version used on discharge date"
# label var DRG_NoPOA                "DRG in use on discharge date, calculated without POA"
# label var DXVER                    "Diagnosis Version"
# label var ELECTIVE                 "Elective versus non-elective admission"
# label var FEMALE                   "Indicator of sex"
# label var HCUP_ED                  "HCUP Emergency Department service indicator"
# label var HOSP_DIVISION            "Census Division of hospital"
# label var HOSP_NIS                 "NIS hospital number"
# label var I10_DX1                  "ICD-10-CM Diagnosis 1"
# label var I10_DX2                  "ICD-10-CM Diagnosis 2"
# label var I10_DX3                  "ICD-10-CM Diagnosis 3"
# label var I10_DX4                  "ICD-10-CM Diagnosis 4"
# label var I10_DX5                  "ICD-10-CM Diagnosis 5"
# label var I10_DX6                  "ICD-10-CM Diagnosis 6"
# label var I10_DX7                  "ICD-10-CM Diagnosis 7"
# label var I10_DX8                  "ICD-10-CM Diagnosis 8"
# label var I10_DX9                  "ICD-10-CM Diagnosis 9"
# label var I10_DX10                 "ICD-10-CM Diagnosis 10"
# label var I10_DX11                 "ICD-10-CM Diagnosis 11"
# label var I10_DX12                 "ICD-10-CM Diagnosis 12"
# label var I10_DX13                 "ICD-10-CM Diagnosis 13"
# label var I10_DX14                 "ICD-10-CM Diagnosis 14"
# label var I10_DX15                 "ICD-10-CM Diagnosis 15"
# label var I10_DX16                 "ICD-10-CM Diagnosis 16"
# label var I10_DX17                 "ICD-10-CM Diagnosis 17"
# label var I10_DX18                 "ICD-10-CM Diagnosis 18"
# label var I10_DX19                 "ICD-10-CM Diagnosis 19"
# label var I10_DX20                 "ICD-10-CM Diagnosis 20"
# label var I10_DX21                 "ICD-10-CM Diagnosis 21"
# label var I10_DX22                 "ICD-10-CM Diagnosis 22"
# label var I10_DX23                 "ICD-10-CM Diagnosis 23"
# label var I10_DX24                 "ICD-10-CM Diagnosis 24"
# label var I10_DX25                 "ICD-10-CM Diagnosis 25"
# label var I10_DX26                 "ICD-10-CM Diagnosis 26"
# label var I10_DX27                 "ICD-10-CM Diagnosis 27"
# label var I10_DX28                 "ICD-10-CM Diagnosis 28"
# label var I10_DX29                 "ICD-10-CM Diagnosis 29"
# label var I10_DX30                 "ICD-10-CM Diagnosis 30"
# label var I10_DX31                 "ICD-10-CM Diagnosis 31"
# label var I10_DX32                 "ICD-10-CM Diagnosis 32"
# label var I10_DX33                 "ICD-10-CM Diagnosis 33"
# label var I10_DX34                 "ICD-10-CM Diagnosis 34"
# label var I10_DX35                 "ICD-10-CM Diagnosis 35"
# label var I10_DX36                 "ICD-10-CM Diagnosis 36"
# label var I10_DX37                 "ICD-10-CM Diagnosis 37"
# label var I10_DX38                 "ICD-10-CM Diagnosis 38"
# label var I10_DX39                 "ICD-10-CM Diagnosis 39"
# label var I10_DX40                 "ICD-10-CM Diagnosis 40"
# label var I10_NDX                  "ICD-10-CM Number of diagnoses on this record"
# label var I10_NPR                  "ICD-10-PCS Number of procedures on this record"
# label var I10_PR1                  "ICD-10-PCS Procedure 1"
# label var I10_PR2                  "ICD-10-PCS Procedure 2"
# label var I10_PR3                  "ICD-10-PCS Procedure 3"
# label var I10_PR4                  "ICD-10-PCS Procedure 4"
# label var I10_PR5                  "ICD-10-PCS Procedure 5"
# label var I10_PR6                  "ICD-10-PCS Procedure 6"
# label var I10_PR7                  "ICD-10-PCS Procedure 7"
# label var I10_PR8                  "ICD-10-PCS Procedure 8"
# label var I10_PR9                  "ICD-10-PCS Procedure 9"
# label var I10_PR10                 "ICD-10-PCS Procedure 10"
# label var I10_PR11                 "ICD-10-PCS Procedure 11"
# label var I10_PR12                 "ICD-10-PCS Procedure 12"
# label var I10_PR13                 "ICD-10-PCS Procedure 13"
# label var I10_PR14                 "ICD-10-PCS Procedure 14"
# label var I10_PR15                 "ICD-10-PCS Procedure 15"
# label var I10_PR16                 "ICD-10-PCS Procedure 16"
# label var I10_PR17                 "ICD-10-PCS Procedure 17"
# label var I10_PR18                 "ICD-10-PCS Procedure 18"
# label var I10_PR19                 "ICD-10-PCS Procedure 19"
# label var I10_PR20                 "ICD-10-PCS Procedure 20"
# label var I10_PR21                 "ICD-10-PCS Procedure 21"
# label var I10_PR22                 "ICD-10-PCS Procedure 22"
# label var I10_PR23                 "ICD-10-PCS Procedure 23"
# label var I10_PR24                 "ICD-10-PCS Procedure 24"
# label var I10_PR25                 "ICD-10-PCS Procedure 25"
# label var KEY_NIS                  "NIS record number"
# label var LOS                      "Length of stay (cleaned)"
# label var MDC                      "MDC in effect on discharge date"
# label var MDC_NoPOA                "MDC in use on discharge date, calculated without POA"
# label var NIS_STRATUM              "NIS hospital stratum"
# label var PAY1                     "Primary expected payer (uniform)"
# label var PL_NCHS                  "Patient Location: NCHS Urban-Rural Code"
# label var PRDAY1                   "Number of days from admission to I10_PR1"
# label var PRDAY2                   "Number of days from admission to I10_PR2"
# label var PRDAY3                   "Number of days from admission to I10_PR3"
# label var PRDAY4                   "Number of days from admission to I10_PR4"
# label var PRDAY5                   "Number of days from admission to I10_PR5"
# label var PRDAY6                   "Number of days from admission to I10_PR6"
# label var PRDAY7                   "Number of days from admission to I10_PR7"
# label var PRDAY8                   "Number of days from admission to I10_PR8"
# label var PRDAY9                   "Number of days from admission to I10_PR9"
# label var PRDAY10                  "Number of days from admission to I10_PR10"
# label var PRDAY11                  "Number of days from admission to I10_PR11"
# label var PRDAY12                  "Number of days from admission to I10_PR12"
# label var PRDAY13                  "Number of days from admission to I10_PR13"
# label var PRDAY14                  "Number of days from admission to I10_PR14"
# label var PRDAY15                  "Number of days from admission to I10_PR15"
# label var PRDAY16                  "Number of days from admission to I10_PR16"
# label var PRDAY17                  "Number of days from admission to I10_PR17"
# label var PRDAY18                  "Number of days from admission to I10_PR18"
# label var PRDAY19                  "Number of days from admission to I10_PR19"
# label var PRDAY20                  "Number of days from admission to I10_PR20"
# label var PRDAY21                  "Number of days from admission to I10_PR21"
# label var PRDAY22                  "Number of days from admission to I10_PR22"
# label var PRDAY23                  "Number of days from admission to I10_PR23"
# label var PRDAY24                  "Number of days from admission to I10_PR24"
# label var PRDAY25                  "Number of days from admission to I10_PR25"
# label var PRVER                    "Procedure Version"
# label var RACE                     "Race (uniform)"
# label var TOTCHG                   "Total charges (cleaned)"
# label var TRAN_IN                  "Transfer in indicator"
# label var TRAN_OUT                 "Transfer out indicator"
# label var YEAR                     "Calendar year"
# label var ZIPINC_QRTL              "Median household income national quartile for patient ZIP Code"
# 
# *** Convert special values to missing values ***
# recode AGE                       (-99 -88 -66=.)
# recode AGE_NEONATE               (-9 -8 -6 -5=.)
# recode AMONTH                    (-9 -8 -6 -5=.)
# recode AWEEKEND                  (-9 -8 -6 -5=.)
# recode DIED                      (-9 -8 -6 -5=.)
# recode DISCWT                    (-99.9999999 -88.8888888 -66.6666666=.)
# recode DISPUNIFORM               (-9 -8 -6 -5=.)
# recode DQTR                      (-9 -8 -6 -5=.)
# recode DRG                       (-99 -88 -66=.)
# recode DRGVER                    (-9 -8 -6 -5=.)
# recode DRG_NoPOA                 (-99 -88 -66=.)
# recode DXVER                     (-9 -8 -6 -5=.)
# recode ELECTIVE                  (-9 -8 -6 -5=.)
# recode FEMALE                    (-9 -8 -6 -5=.)
# recode HCUP_ED                   (-99 -88 -66=.)
# recode HOSP_DIVISION             (-9 -8 -6 -5=.)
# recode HOSP_NIS                  (-9999 -8888 -6666=.)
# recode I10_NDX                   (-9 -8 -6 -5=.)
# recode I10_NPR                   (-9 -8 -6 -5=.)
# recode KEY_NIS                   (-999999999 -888888888 -666666666=.)
# recode LOS                       (-9999 -8888 -6666=.)
# recode MDC                       (-9 -8 -6 -5=.)
# recode MDC_NoPOA                 (-9 -8 -6 -5=.)
# recode NIS_STRATUM               (-999 -888 -666=.)
# recode PAY1                      (-9 -8 -6 -5=.)
# recode PL_NCHS                   (-99 -88 -66=.)
# recode PRDAY1                    (-99 -88 -66=.)
# recode PRDAY2                    (-99 -88 -66=.)
# recode PRDAY3                    (-99 -88 -66=.)
# recode PRDAY4                    (-99 -88 -66=.)
# recode PRDAY5                    (-99 -88 -66=.)
# recode PRDAY6                    (-99 -88 -66=.)
# recode PRDAY7                    (-99 -88 -66=.)
# recode PRDAY8                    (-99 -88 -66=.)
# recode PRDAY9                    (-99 -88 -66=.)
# recode PRDAY10                   (-99 -88 -66=.)
# recode PRDAY11                   (-99 -88 -66=.)
# recode PRDAY12                   (-99 -88 -66=.)
# recode PRDAY13                   (-99 -88 -66=.)
# recode PRDAY14                   (-99 -88 -66=.)
# recode PRDAY15                   (-99 -88 -66=.)
# recode PRDAY16                   (-99 -88 -66=.)
# recode PRDAY17                   (-99 -88 -66=.)
# recode PRDAY18                   (-99 -88 -66=.)
# recode PRDAY19                   (-99 -88 -66=.)
# recode PRDAY20                   (-99 -88 -66=.)
# recode PRDAY21                   (-99 -88 -66=.)
# recode PRDAY22                   (-99 -88 -66=.)
# recode PRDAY23                   (-99 -88 -66=.)
# recode PRDAY24                   (-99 -88 -66=.)
# recode PRDAY25                   (-99 -88 -66=.)
# recode PRVER                     (-9 -8 -6 -5=.)
# recode RACE                      (-9 -8 -6 -5=.)
# recode TOTCHG                    (-999999999 -888888888 -666666666=.)
# recode TRAN_IN                   (-9 -8 -6 -5=.)
# recode TRAN_OUT                  (-9 -8 -6 -5=.)
# recode YEAR                      (-999 -888 -666=.)
# recode ZIPINC_QRTL               (-9 -8 -6 -5=.)

# hospital file
# label var DISCWT                   "NIS discharge weight"
# label var HOSP_BEDSIZE             "Bed size of hospital (STRATA)"
# label var HOSP_DIVISION            "Census Division of hospital (STRATA)"
# label var HOSP_LOCTEACH            "Location/teaching status of hospital (STRATA)"
# label var HOSP_NIS                 "NIS hospital number"
# label var HOSP_REGION              "Region of hospital"
# label var H_CONTRL                 "Control/ownership of hospital (STRATA)"
# label var NIS_STRATUM              "NIS hospital stratum"
# label var N_DISC_U                 "Number of universe discharges in the stratum"
# label var N_HOSP_U                 "Number of universe hospitals in the stratum"
# label var S_DISC_U                 "Number of sample discharges in the stratum"
# label var S_HOSP_U                 "Number of sample hospitals in the stratum"
# label var TOTAL_DISC               "Total number of discharges from this hospital in the NIS"
# label var YEAR                     "Calendar year"



#####
## old thoughts





for (i in 1:nrow(test)) {
  if (sum(key[i,]) == 0) {
    test2[-(i),]
  }
}
apply(X = test[25,], MARGIN = 1:2, function(x){grepl("0258",x)})
apply(X = test,MARGIN = 2, function(x){grepl("0258",x)})
apply(X = test[,1], MARGIN = 1:2, function(x){grepl("0258",x)})
test[apply(X = test[,1], MARGIN = 1:2, function(x){grepl("0258",x)}),]
rowsum(apply(X = test, MARGIN = 1:2, function(x){grepl("0258",x)}))

grepl("0258",test)
grep("0258",test)

test[str_detect(test$I10_PR1, "0258"),]
test[str_detect(test$I10_PR2, "0258"),]





