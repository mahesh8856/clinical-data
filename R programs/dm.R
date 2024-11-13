library(random.cdisc.data)
library(haven)
library(tidyverse)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
library(sqldf)
library(labeling)
library(expss)
library(reshape)

#setwd("C:/sas/Great online training material/CDISC Materials")
#path <- "C:/sas/Great online training material/CDISC Materials/RAW DATASETS/"
#DM1 <- read_sas(paste0(path,"dm.sas7bdat"))

# Load data
adam <- "C:/sas/Great online training material/CDISC Materials/ADAM DATASETS"
sdtm <- "C:/sas/Great online training material/CDISC Materials/SDTM DATASETS"
raw <- "C:/sas/Great online training material/CDISC Materials/RAW DATASETS"

# Set options
options(error = NULL) # equivalent to nofmterr

# Load datasets
dm <- read_sas(paste0(raw, "/dm.sas7bdat"))
ex <- read_sas(paste0(raw, "/ex.sas7bdat"))
eot <- read_sas(paste0(raw, "/ds_eot.sas7bdat"))
dth <- read_sas(paste0(raw, "/dth.sas7bdat"))
ic <- read_sas(paste0(raw, "/ds_ic.sas7bdat"))
eos <- read_sas(paste0(raw, "/eos.sas7bdat"))
ie <- read_sas(paste0(raw, "/ie.sas7bdat"))

# Data Manipulation
dm1 <- dm %>%
  dplyr::rename(AGEX = AGE, SEXX = SEX, ETHNICX = ETHNIC) %>%
  mutate(STUDYID = "043-18101",
         DOMAIN = "DM",
         USUBJID = paste0(STUDYID, "-", SUBNUM),
         SUBJID = sub(".*-", "", SUBNUM),
         SITEID = sub("-.*", "", SUBNUM),
         AGE = AGEX,
         AGEU = ifelse(!is.na(AGE), "YEARS", NA),
         BRTHDTC = YOB,
         SEX = SEXX,
         ETHNIC = toupper(trimws(ETHNIC_DEC)))

dm1 <- dm1 %>%
  mutate(RACE = case_when(
    WHITE == "X" ~ "WHITE",
    BLACK == "X" ~ "BLACK",
    NATIVE == "X" ~ "NATIVE",
    ASIAN == "X" ~ "ASIAN",
    PACIFIC == "X" ~ "PACIFIC",
    OTHER == "X" ~ "OTHER",
    RACEUNK == "X" ~ "UNKNOWN",
    TRUE ~ NA_character_
  ))

# RFSTDTC and RFXSTDTC
ex <- ex %>%
  filter(VISNAME == "Cycle 1 - Day 1 (1st infusion)") %>%
  mutate(RFSTDTC = paste0(format(EXSTDAT, "%Y-%m-%d"), "T", format(EXSTTIM, "%H:%M:%S")),
         RFXSTDTC = paste0(format(EXSTDAT, "%Y-%m-%d"), "T", format(EXSTTIM, "%H:%M:%S")))%>%
  select(SUBNUM, RFSTDTC, RFXSTDTC, EXLEVEL)

eot <- eot %>%
  mutate(RFENDTCN = format(DSDAT, "%Y-%m-%d"),
         RFENDTCO = format(DSCDAT, "%Y-%m-%d"))%>%
  select(SUBNUM, RFENDTCN, RFENDTCO)

rfendtc2 <- eot %>%
  filter(!is.na(RFENDTCN) | !is.na(RFENDTCO)) %>%
  arrange(SUBNUM, ifelse(!is.na(RFENDTCN), RFENDTCN, RFENDTCO)) %>%
  group_by(SUBNUM) %>%
  summarise(RFXENDTC = first(ifelse(!is.na(RFENDTCN), RFENDTCN, RFENDTCO)),
            RFENDTC = first(ifelse(!is.na(RFENDTCN), RFENDTCN, RFENDTCO))) %>%
  ungroup()%>%
  select(SUBNUM, RFENDTC, RFXENDTC)

dth <- dth %>%
  mutate(DTHFL = "Y",
         DTHDTC = format(DTHDAT, "%Y-%m-%d"))%>%
  select(SUBNUM, DTHFL, DTHDTC)

ic <- ic %>%
  filter(PAGENAME == "Informed Consent") %>%
  mutate(RFICDTC = format(RFICDAT, "%Y-%m-%d")) %>%
  filter(RFICDTC != "")%>%
  select(SUBNUM, RFICDTC)

eos <- eos %>%
  mutate(RFPENDTC = format(DSDAT, "%Y-%m-%d"))%>%
  select(SUBNUM, RFPENDTC)

# Sort datasets
ex <- ex %>% arrange(SUBNUM)
dm1 <- dm1 %>% arrange(SUBNUM)
ic <- ic %>% arrange(SUBNUM)
ie <- ie %>% arrange(SUBNUM)

# Merge datasets
dm2 <- dm1 %>%
  left_join(ic, by = "SUBNUM") %>%
  left_join(ex, by = "SUBNUM") %>%
  left_join(rfendtc2, by = "SUBNUM") %>%
  left_join(dth, by = "SUBNUM") %>%
  left_join(eos, by = "SUBNUM") %>%
  left_join(ie, by = "SUBNUM")

dm3 <- dm2 %>%
  mutate(LENGTH_studyid = 20, # Assuming default length
         LENGTH_armcd = 20,
         LENGTH_actarmcd = 20,
         LENGTH_domain = 2,
         LENGTH_usubjid = 50,
         LENGTH_arm = 40,
         LENGTH_actarm = 40,
         LENGTH_subjid = 50,
         LENGTH_siteid = 20,
         LENGTH_ageu = 6,
         LENGTH_country = 50,
         ARMCD = ifelse(EXLEVEL != "", paste0("AIRIS", sub(" .*", "", EXLEVEL)), ifelse(ieyn_dec == "No", "SCRNFAIL", "NOTASSGN")),
         ARM = ifelse(EXLEVEL != "", paste0("AIRIS-101 ", sub(" .*", "", EXLEVEL)), ifelse(ieyn_dec == "No", "Screen Failure", "Not Assigned")),
         ACTARMCD = ifelse(RFXSTDTC != "" | ARMCD == "SCRNFAIL", ARMCD, "NOTTRT"),
         ACTARM = ifelse(RFXSTDTC != "" | ARMCD == "SCRNFAIL", ARM, "Not Treated"),
         COUNTRY = "USA") %>%
  select(STUDYID, DOMAIN, USUBJID, SUBJID, SITEID, RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC, RFICDTC, RFPENDTC, DTHDTC, DTHFL,
         AGE, AGEU, SEX, RACE, ETHNIC, ARMCD, ARM, ACTARMCD, ACTARM, COUNTRY)

# Create final dataset
final <- dm3

# Save final dataset
write_sas(final, paste0(sdtm, "/dm_r.sas7bdat"))





















