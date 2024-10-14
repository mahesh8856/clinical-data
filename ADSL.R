#install.packages("random.cdisc.data")
#install.packages("expss")
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

#data(package="random.cdisc.data")$results[, "Item"]
#data("cadsl", package = "random.cdisc.data")
#head(cadsl)
#install.packages(c("rtables", "tern", "gt", "remotes", "tidyverse", "bookdown",
#                   "tables", "formatters", "tidytlg", "flextable"))
#install.packages("remotes")
#remotes::install_github("pharmaverse/sdtm.oak")

setwd("C:\\sas\\Great online training material\\CDISC Materials\\SDTM DATASETS")

dmo <- read_sas("dm.sas7bdat")%>%
  arrange(USUBJID)

suppdm0 <- read_sas("suppdm.sas7bdat")%>%
  arrange(USUBJID)%>%
  select(USUBJID, QNAM, QVAL)

SUPPDM_T  <- pivot_wider(suppdm0, 
                         id_cols = USUBJID,
                         names_from = QNAM,
                         values_from = QVAL)

dm1 <- left_join(dmo, SUPPDM_T, by = "USUBJID")

#Child-Bearing Potential (if female)

RPO <- read_sas("rp.sas7bdat")%>%
  filter(RPTESTCD == "CHILDPOT")%>%
  mutate(CHILDPOT = str_trim(RPSTRESC))%>%
  select(USUBJID, CHILDPOT)

DM1 <- left_join(dm1,RPO, by="USUBJID")

ARIS <- DM1%>%
  filter(RFXSTDTC != "")%>%  # filter(!is.na(RFXSTDTC))%>%
  arrange(USUBJID)%>%
  group_by(USUBJID)%>%
  slice(1)%>%
  select(USUBJID, ACTARMCD)

#COPY ALL THE VARIABLES FROM SDTM EC
eco <- read_sas("ec.sas7bdat")%>%
  arrange(USUBJID, ECSEQ)

suppeco <- read_sas("suppec.sas7bdat")%>%
  mutate(ECSEQ = as.numeric(IDVARVAL))%>%
  arrange(USUBJID, ECSEQ)

speco_t <-  pivot_wider(suppeco,
                          id_cols = c(USUBJID,ECSEQ),
                          names_from = QNAM,
                          values_from = QVAL)

ec1 <- left_join(eco,speco_t, by= c("USUBJID","ECSEQ"))

nab <- ec1%>%
  arrange(USUBJID,DOSELEVL)%>%
  filter(!is.na(DOSELEVL))%>%
  select(USUBJID,DOSELEVL)

trt <- merge(ARIS,nab, by= "USUBJID")%>%
  mutate(ARISN = as.numeric(case_when(
    ACTARMCD == "AIRIS1" ~ 1,
    ACTARMCD == "AIRIS1A" ~ 2,
    ACTARMCD == "AIRIS2A" ~ 3,
    ACTARMCD == "AIRIS3A" ~ 4,
    ACTARMCD == "AIRIS3B" ~ 5,
    ACTARMCD == "AIRIS2B" ~ 6,
    ACTARMCD == "AIRIS1B" ~ 7)))%>%
  mutate(NABN = as.numeric(case_when(
    DOSELEVL == "100 mg/m^2" ~ 1,
    DOSELEVL == "75 mg/m^2" ~ 2)))%>%
  mutate(TRT01PN = as.numeric(case_when(
    ARISN == 1 & NABN == 1 ~ 1,
    ARISN == 2 & NABN == 2 ~ 2,
    ARISN == 3 & NABN == 2 ~ 3)))%>%
  mutate(TRT01P = as.character(case_when(
    TRT01PN == 1 ~ "ARIS-101 240 mg QD Intermittent + nab-paclitaxel 100 mg/m^2",
    TRT01PN == 2 ~ "ARIS-101 0 mg QD Intermittent + nab-paclitaxel 75 mg/m^2",
    TRT01PN == 3 ~ "ARIS-101 160 mg QD Intermittent + nab-paclitaxel 75 mg/m^2",
    TRT01PN == 4 ~ "ARIS-101 240 mg QD Intermittent + nab-paclitaxel 75 mg/m^2")))%>%  #USE CODE NO MISSING TRT01PN IF NEEDED
  mutate(TRT01A = TRT01P,
         TRT01AN = TRT01PN)

DM2 <- DM1%>%
  mutate(AGEGR1 = case_when(
    AGE < 65 ~ "< 65",
    AGE >= 65 ~ ">= 65",))%>%
  mutate(SAFFL = case_when(
    RFXSTDTC != "" ~ "Y",
    RFXSTDTC == "" ~ "N"))%>%
  mutate(ENRLFL = if_else
         (toupper(ARMCD) != "SCRNFAIL","Y","N"))   #ENRLFL = case_when(            
                                                    #toupper(ARMCD) != "SCRNFAIL" ~ "Y",
                                                    #TRUE ~ "N"))


DM3 <- DM2 %>%
  mutate(DT = as.POSIXct(RFXSTDTC, tz="UTC", format("%Y-%m-%dT%H:%M")))%>%
  mutate(TRTSDT = as.Date(DT) - as.Date("1960-01-01"))%>%
  mutate(TRTSTM = hour(DT)*3600 + minute(DT)*60)%>%
  mutate(TRTSDTM = TRTSDT/dseconds() + TRTSTM)%>%
  mutate(ET = as.POSIXct(RFXENDTC, tz="UTC", format("%Y-%m-%d")))%>%
  mutate(TRTEDT = as.Date(RFXENDTC) - as.Date("1960-01-01"))%>%
  mutate(TRTETM = hour(ET)*3600 + minute(ET)*60)%>%
  mutate(TRTEDTM = TRTEDT/dseconds() + TRTETM)%>%
  mutate(TRTEDTM = if_else(TRTETM == 0,TRTETM,TRTEDTM))%>%
  mutate(RFICDT = as.Date(RFICDTC) - as.Date("1960-01-01"))%>%
  mutate(DTHDT = as.Date(DTHDTC) - as.Date("1960-01-01"))

DS0 <- read_sas("ds.sas7bdat")%>%
  arrange(USUBJID, DSSEQ)

SUPPDS0 <- read_sas("suppds.sas7bdat")%>%
  mutate(DSSEQ = as.numeric(IDVARVAL))%>%
  arrange(USUBJID, DSSEQ)

spds0_t <-  pivot_wider(SUPPDS0,
                        id_cols = c(USUBJID,DSSEQ),
                        names_from = QNAM,
                        values_from = QVAL)

ds1 <- left_join(DS0,spds0_t, by= c("USUBJID","DSSEQ"))

EOS <- ds1 %>%
  filter(toupper(DSSCAT) == "STUDY DISCONTINUATION")

EOT_O <- ds1 %>%
  filter(toupper(DSSCAT) == "TREATMENT TERMINATION" & 
         DSGRPID == "AIRIS-101")

EOT_N <- ds1 %>%
  filter(toupper(DSSCAT) == "TREATMENT TERMINATION" & 
           DSGRPID == "NAB-PACLITAXEL")
eos1 <- EOS%>%
  mutate(EOSSTT = if_else(DSSTDTC != "","DISCONTINUED","ONGOING"))%>%
  mutate(EOSDT = as.Date(DSSTDTC) - as.Date("1960-01-01"))%>%
  mutate(DCSREAS = str_trim(DSDECOD))%>%
  mutate(DCSREASP = case_when(toupper(DSDECOD) == "OTHER" ~ str_trim(DSTERM)))%>%
  mutate(ICWTHDT = case_when(toupper(DSDECOD) == "WITHDRAWAL BY PATIENT FROM STUDY" ~ EOSDT))%>%
  select(USUBJID, EOSSTT, DCSREAS, DCSREASP, ICWTHDT, EOSDT)

EOT_O1 <- EOT_O%>%
  mutate(EOSSTT_O = if_else(DSSTDTC != "","DISCONTINUED","ONGOING"))%>%
  mutate(EOTDT = as.Date(DSSTDTC) - as.Date("1960-01-01"))%>%
  mutate(DCTREAS = str_trim(DSDECOD))%>%
  mutate(DCTREASP = case_when(toupper(DSDECOD) == "OTHER" ~ str_trim(DSTERM)))%>%
  select(USUBJID, EOSSTT_O, DCTREAS, DCTREASP, EOTDT)

EOT_N1 <- EOT_N%>%
  mutate(EOSSTT_N = if_else(DSSTDTC != "","DISCONTINUED","ONGOING"))%>%
  mutate(DCTRSNB = str_trim(DSDECOD))%>%
  mutate(DCTRNBSP = case_when(toupper(DSDECOD) == "OTHER" ~ str_trim(DSTERM)))%>%
  select( USUBJID, EOSSTT_N, DCTRSNB, DCTRNBSP)

EOT1 <- left_join(EOT_O1, EOT_N1, by = "USUBJID")
EOT2 <- left_join(EOT1,eos1, by = "USUBJID")%>%
  mutate(EOTSTT = case_when(DCTREAS != "" | DCTRSNB != "" ~ "DISCONTINUED"))

DM4 <- left_join(DM3, EOT2, by = "USUBJID")

#DLTEVLFL
ex <- read_sas("ex.sas7bdat")

ARISTRT <- ex%>%filter(EXSTDY < 2 & toupper(EXTRT) == "AIRIS-101")
NABTRT <- ex %>% filter(EXSTDY < 2 & toupper(EXTRT) == "NAB-PACLITAXEL")


ARISTRT1 <- ARISTRT %>%
  filter(EXDOSE > 0)%>%
  group_by(USUBJID)%>%
  count()%>%
  mutate(n1 = n)%>%
  select(-n)


NABTRT1 <- NABTRT %>%
  filter(EXDOSE > 0)%>%
  group_by(USUBJID)%>%
  count()

EX2 <- left_join(ARISTRT1, NABTRT1, by = "USUBJID")%>%
  mutate(DLTEVLFL = case_when(n1 > 11 & n >2 ~ "Y"))

trt1 <- trt %>%
  arrange_all()%>%
  group_by_all()%>%
  slice(1)

DM5 <- left_join(DM4,EX2, by= "USUBJID")
DM6 <- left_join(DM5, trt1, by = "USUBJID")%>%
  mutate(PKEVLFL=SAFFL)%>%
  select(STUDYID, USUBJID,AGEGR1, SEX,SUBJID, SITEID, AGE, AGEU, CHILDPOT,
         RACE, ETHNIC, SAFFL, DLTEVLFL,PKEVLFL,ENRLFL, ARM, ARMCD, ACTARMCD.x,
         ACTARM,TRT01P, TRT01PN,TRT01A,TRT01AN,TRTSDT,TRTSTM,TRTSDTM,TRTEDT,TRTETM,
         TRTEDTM, EOSSTT, EOSDT, DCSREAS, DCSREASP, EOTSTT, EOTDT, DCTREAS, DCTREASP, DCTRSNB, DCTRNBSP, RFICDT, ICWTHDT, DTHDT)

DM7 <-  apply_labels(DM6,
                     STUDYID = "Study Identifier"  ,
             USUBJID	= "Unique Subject Identifier"   	,
             SUBJID	= "Subject Identifier for the Study"  		,
             SITEID	= "Study Site Identifier"  	,
             AGE =	 "Age"  		,
             AGEU	= "Age Units"  		,
             AGEGR1	= "Pooled Age Group 1"  		,
             SEX	= "Sex"  		,
             CHILDPOT	= "Child-Bearing Potential (if female)"  		,
             RACE	= "Race"  		,
             ETHNIC	= "Ethnicity"  		,
             SAFFL	= "Safety Population Flag"  		,
             DLTEVLFL	= "DLT Evaluable Population Flag"  		,
             PKEVLFL	= "PK Evaluable Population Flag"  		,
             ENRLFL	= "Enrolled Population Flag"  		,
             ARM	= "Description of Planned Arm"  		,
             ARMCD	= "Planned Arm Code"  		,
             ACTARMCD.x	= "Actual Arm Code"  		,
             ACTARM	= "Description of Actual Arm"  		,
             TRT01P	= "Planned Treatment for Period 01"  		,
             TRT01PN	= "Planned Treatment for Period 01 (N)"  		,
             TRTSDT	= "Date of First Exposure to Treatment"  		,
             TRTSTM	= "Time of First Exposure to Treatment"  		,
             TRTSDTM	= "Datetime of First Exposure to Treatment"  		,
             TRTEDT	= "Date of Last Exposure to Treatment"  		,
             TRTETM	= "Time of Last Exposure to Treatment"  		,
             TRTEDTM	= "Datetime of Last Exposure to Treatment"  		,
             RFICDT	= "Date of Informed Consent" ,
             DTHDT	= "Date of Death"  ,
             EOSSTT	= "End of Study Status",
             EOSDT	= "End of Study Date"  ,
             DCSREAS	= "Reason for Discontinuation from Study"  		,
             DCSREASP	= "Reason Spec for Discont from Study"  		,
             ICWTHDT	= "Date of Consent Withdrawal"  		,
             EOTSTT	= "End of Treatment Status"  		,
             EOTDT	= "End of Treatment Date"  		,
             DCTREAS	= "Reason for Discontinuation of Treatment"  		,
             DCTREASP	= "Reason Specify for Discont of Treatment"  		,
             DCTRSNB	= "Reason for Discontinuation from Nab-Pac"	,
             DCTRNBSP	= "Reason Specify for Discont of Nab-Pac")

final <- DM7 %>%
  arrange_all()%>%
  group_by_all()%>%
  slice(1)






