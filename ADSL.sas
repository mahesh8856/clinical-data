/*******************************************************************
* Client: XXXX                                                           
* Product: XXXXX                                                  
* Project: Protocol: 043-1810                                                    
* Program: ADSL.sas 
*
* Program Type: ADaM
*
* Purpose: To produce the ADSL
* Usage Notes: 
*
* SAS� Version: 9.4 [TS2M0]
* Operating System: Windows 2003 R2 Standard Edition.                   
*
* Author: Mahesh 
* Date Created: 13 AUG 2024
*******************************************************************/
libname adam "C:\sas\Great online training material\CDISC Materials\ADAM DATASETS";
libname sdtm "C:\sas\Great online training material\CDISC Materials\SDTM DATASETS";	

******************COPY ALL THE VARIABLES FROM SDTM DM***********;

DATA DM0;
SET SDTM.DM;
RUN;

PROC SORT;BY USUBJID;RUN;

************HANDLING OF SUPP-- DATASET****************;

DATA SUPPDM0;
SET SDTM.SUPPDM;
RUN;
PROC SORT;BY USUBJID;RUN;

PROC TRANSPOSE DATA=SUPPDM0 OUT=T_SUPPDM0 (DROP=_NAME_ _LABEL_) ;
BY USUBJID;
ID QNAM;
IDLABEL  QLABEL;
VAR QVAL;
RUN;

DATA DM1;
MERGE DM0 T_SUPPDM0;
BY USUBJID;
RUN;


/*Child-Bearing Potential (if female)*/

DATA RP0;
SET SDTM.RP;
WHERE RPTESTCD = 'CHILDPOT';
CHILDPOT= STRIP (RPSTRESC);
KEEP USUBJID CHILDPOT;
RUN;


DATA DM1;
MERGE DM1 (IN=A) RP0;
BY USUBJID;
IF A;
RUN;


PROC SORT DATA=DM1 OUT=AIRIS (KEEP=USUBJID ACTARMCD) NODUPKEY;
WHERE NOT MISSING (RFXSTDTC);
BY USUBJID;
RUN;



******************COPY ALL THE VARIABLES FROM SDTM EC***********;

DATA EC0;
SET SDTM.EC;
RUN;

PROC SORT;BY USUBJID ECSEQ;RUN;



************HANDLING OF SUPP-- DATASET****************;

DATA SUPPEC0;
SET SDTM.SUPPEC;
ECSEQ= INPUT (IDVARVAL,BEST.);
RUN;
PROC SORT;BY USUBJID ECSEQ;RUN;

PROC TRANSPOSE DATA=SUPPEC0 OUT=T_SUPPEC0 (DROP=_NAME_ _LABEL_);
BY USUBJID ECSEQ;
ID QNAM;
IDLABEL  QLABEL;
VAR QVAL;
RUN;

DATA EC1;
MERGE EC0 T_SUPPEC0;
BY USUBJID ECSEQ;
RUN;

PROC SORT DATA=EC1 OUT=NAB (KEEP=USUBJID DOSELEVL);
WHERE NOT MISSING (DOSELEVL);
BY USUBJID DOSELEVL;
RUN;

PROC FORMAT ;
INVALUE ARMCD_1F
'AIRIS1'=1
'AIRIS1A'=2
'AIRIS2A'=3
'AIRIS3A'=4
'AIRIS3B'=5
'AIRIS2B'=6
'AIRIS1B'=7
;

INVALUE NAB_1F
'100 mg/m^2'=1
'75 mg/m^2'=2
;

VALUE TRT01P_F
1="ARIS-101 240 mg QD Intermittent + nab-paclitaxel 100 mg/m^2"
2="ARIS-101 80 mg QD Intermittent + nab-paclitaxel 75 mg/m^2"
3="ARIS-101 160 mg QD Intermittent + nab-paclitaxel 75 mg/m^2"
4="ARIS-101 240 mg QD Intermittent + nab-paclitaxel 75 mg/m^2"
;

RUN;


DATA TRT;
MERGE AIRIS NAB;
BY USUBJID;

AIRIS= STRIP (ACTARMCD);
AIRISN = INPUT (AIRIS,ARMCD_1F.);

NABN= INPUT (DOSELEVL,NAB_1F.);

IF AIRISN=1 AND NABN=1 THEN TRT01PN=1;
ELSE IF AIRISN=2 AND NABN=2 THEN TRT01PN=2;
ELSE IF AIRISN=3 AND NABN=2 THEN TRT01PN=3;

IF NOT MISSING (TRT01PN) THEN TRT01P= STRIP ( PUT (TRT01PN,TRT01P_F.));
IF NOT MISSING (TRT01PN);
TRT01A=TRT01P;
TRT01AN=TRT01PN;

RUN;


data dm2;
set DM1;

IF . <AGE < 65 THEN AGEGR1="< 65";
ELSE IF AGE >= 65 THEN AGEGR1=">= 65";

IF NOT MISSING (RFXSTDTC) THEN SAFFL="Y";
ELSE SAFFL="N";

IF UPCASE (ARMCD) ^="SCRNFAIL" THEN ENRLFL="Y";
ELSE ENRLFL="N";
RUN;


DATA DM2;
SET DM2;
/*2019-05-08T09:23*/
IF LENGTH (RFXSTDTC) =16 THEN DO;
TRTSDTM= INPUT (RFXSTDTC,E8601DT.);
TRTSDT= DATEPART (TRTSDTM);
TRTSTM= TIMEPART (TRTSDTM);
END;
/*2019-05-08*/
IF LENGTH (RFXSTDTC) =10 THEN DO;
TRTSDTM= .;
TRTSDT= INPUT (RFXSTDTC,E8601DA.);;
TRTSTM= .;
END;


IF LENGTH (RFXENDTC) =16 THEN DO;
TRTEDTM= INPUT (RFXENDTC,E8601DT.);
TRTEDT= DATEPART (TRTEDTM);
TRTETM= TIMEPART (TRTEDTM);
END;

IF LENGTH (RFXENDTC) =10 THEN DO;
TRTEDTM= .;
TRTEDT= INPUT (RFXENDTC,E8601DA.);;
TRTETM= .;
END;

IF LENGTH (RFICDTC)=10 THEN RFICDT=INPUT (RFICDTC,E8601DA.);

IF LENGTH (DTHDTC)=10 THEN DTHDT=INPUT (DTHDTC,E8601DA.);
RUN;
/*YYYY-MM-DDTHH:MM:SS*/



******************COPY ALL THE VARIABLES FROM SDTM DS***********;

DATA DS0;
SET SDTM.DS;
RUN;

PROC SORT;BY USUBJID DSSEQ;RUN;



************HANDLING OF SUPP-- DATASET****************;

DATA SUPPDS0;
SET SDTM.SUPPDS;
DSSEQ= INPUT (IDVARVAL,BEST.);
RUN;
PROC SORT;BY USUBJID DSSEQ;RUN;

PROC TRANSPOSE DATA=SUPPDS0 OUT=T_SUPPDS0 (DROP=_NAME_ _LABEL_);
BY USUBJID DSSEQ;
ID QNAM;
IDLABEL  QLABEL;
VAR QVAL;
RUN;

DATA DS1;
MERGE DS0 T_SUPPDS0;
BY USUBJID DSSEQ;
RUN;

DATA EOS EOT_O EOT_N;
SET DS1;

IF UPCASE (DSSCAT)="STUDY DISCONTINUATION" THEN OUTPUT EOS;

IF UPCASE (DSSCAT)="TREATMENT TERMINATION" AND 
DSGRPID="AIRIS-101" THEN OUTPUT EOT_O;

IF UPCASE (DSSCAT)="TREATMENT TERMINATION" AND 
DSGRPID="NAB-PACLITAXEL" THEN OUTPUT EOT_N;
RUN;

DATA EOS1;
SET EOS;

IF NOT MISSING (DSSTDTC) THEN EOSSTT="DISCONTINUED";
ELSE EOSSTT="ONGOING";
EOSDT=INPUT (DSSTDTC,E8601DA.);
DCSREAS= STRIP (DSDECOD);

IF UPCASE (DSDECOD)="OTHER" THEN DCSREASP= STRIP (DSTERM);

IF UPCASE (DSDECOD)="WITHDRAWAL BY PATIENT FROM STUDY" THEN
ICWTHDT=EOSDT;

KEEP USUBJID EOSSTT DCSREAS DCSREASP ICWTHDT EOSDT;

RUN;

DATA EOT_O1;
SET EOT_O;

IF NOT MISSING (DSSTDTC) THEN EOSSTT_O="DISCONTINUED";
DCTREAS = STRIP (DSDECOD);
IF UPCASE (DSDECOD)="OTHER" THEN DCTREASP= STRIP (DSTERM);



IF NOT MISSING (DSSTDTC) AND LENGTH (DSSTDTC)=10 THEN
EOTDT= INPUT (DSSTDTC,E8601DA.);

KEEP USUBJID EOSSTT_O DCTREAS DCTREASP EOTDT;
RUN;

DATA EOT_N1;
SET EOT_N;
IF NOT MISSING (DSSTDTC) THEN EOSSTT_N="DISCONTINUED";
DCTRSNB=STRIP (DSDECOD);;
IF UPCASE (DSDECOD)="OTHER" THEN DCTRNBSP= STRIP (DSTERM);
KEEP USUBJID EOSSTT_N DCTRSNB DCTRNBSP;
RUN;

DATA EOT1;
MERGE EOT_O1 EOT_N1 EOS1;
BY USUBJID;

IF DCTREAS NE '' OR DCTRSNB NE '' THEN EOTSTT="DISCONTINUED";

RUN;

DATA DM3;
MERGE DM2 (IN=A) EOT1;
BY USUBJID;
IF A;
RUN;

/*DLTEVLFL*/

DATA ARISTRT NABTRT;
SET SDTM.EX;
WHERE EXSTDY <28;
IF UPCASE (EXTRT)="AIRIS-101" THEN OUTPUT ARISTRT;
IF UPCASE (EXTRT)="NAB-PACLITAXEL" THEN OUTPUT NABTRT;
RUN;

PROC SQL;
CREATE TABLE ARISTRT1 AS
SELECT USUBJID,COUNT (EXDOSE) AS CNT_O
FROM ARISTRT
WHERE EXDOSE >0
GROUP BY USUBJID;


QUIT;


PROC SQL;
CREATE TABLE NABTRT1 AS
SELECT USUBJID,COUNT (EXDOSE) AS CNT_O
FROM NABTRT
WHERE EXDOSE >0
GROUP BY USUBJID;


QUIT;

DATA EX2;
MERGE ARISTRT1 NABTRT1;
BY USUBJID;
IF CNT_O > 11 AND CNT_O >2 THEN DLTEVLFL="Y";
RUN;

PROC SORT DATA=TRT OUT=TRT_ NODUPKEY;BY _ALL_;
RUN;
DATA DM4;
MERGE DM3 (IN=A) EX2 TRT_;
BY USUBJID;
IF A;
PKEVLFL=SAFFL;
KEEP
STUDYID
USUBJID
SUBJID
SITEID
AGE
AGEU
AGEGR1
SEX
CHILDPOT
RACE
ETHNIC
SAFFL
DLTEVLFL
PKEVLFL
ENRLFL
ARM
ARMCD
ACTARMCD
ACTARM
TRT01P
TRT01PN
TRT01A
TRT01AN
TRTSDT
TRTSTM
TRTSDTM
TRTEDT
TRTETM
TRTEDTM
EOSSTT
EOSDT
DCSREAS
DCSREASP
EOTSTT
EOTDT
DCTREAS
DCTREASP
DCTRSNB
DCTRNBSP
RFICDT
ICWTHDT
/*FRBSWDT
ETTBWDT
*/
DTHDT
;
RUN;

proc sql noprint;
create table final as
select

STUDYID "Study Identifier" length=200,
USUBJID	 "Unique Subject Identifier"  length=	200	,
SUBJID	 "Subject Identifier for the Study"  length=	200	,
SITEID	 "Study Site Identifier"  length=	200	,
AGE	 "Age"  length=	8	,
AGEU	 "Age Units"  length=	200	,
AGEGR1	 "Pooled Age Group 1"  length=	200	,
SEX	 "Sex"  length=	200	,
CHILDPOT	 "Child-Bearing Potential (if female)"  length=	200	,
RACE	 "Race"  length=	200	,
ETHNIC	 "Ethnicity"  length=	200	,
SAFFL	 "Safety Population Flag"  length=	200	,
DLTEVLFL	 "DLT Evaluable Population Flag"  length=	200	,
PKEVLFL	 "PK Evaluable Population Flag"  length=	200	,
ENRLFL	 "Enrolled Population Flag"  length=	200	,
ARM	 "Description of Planned Arm"  length=	200	,
ARMCD	 "Planned Arm Code"  length=	200	,
ACTARMCD	 "Actual Arm Code"  length=	200	,
ACTARM	 "Description of Actual Arm"  length=	200	,
TRT01P	 "Planned Treatment for Period 01"  length=	200	,
TRT01PN	 "Planned Treatment for Period 01 (N)"  length=	8	,
TRTSDT	 "Date of First Exposure to Treatment"  length=	8	,
TRTSTM	 "Time of First Exposure to Treatment"  length=	8	,
TRTSDTM	 "Datetime of First Exposure to Treatment"  length=	8	,
TRTEDT	 "Date of Last Exposure to Treatment"  length=	8	,
TRTETM	 "Time of Last Exposure to Treatment"  length=	8	,
TRTEDTM	 "Datetime of Last Exposure to Treatment"  length=	8	,
RFICDT	 "Date of Informed Consent"  length=	8	FORMAT=DATE9.,
DTHDT	 "Date of Death"  length=	8	FORMAT=DATE9.,
EOSSTT	 "End of Study Status"  length=	200	,
EOSDT	 "End of Study Date"  length=	8	,
DCSREAS	 "Reason for Discontinuation from Study"  length=	200	,
DCSREASP	 "Reason Spec for Discont from Study"  length=	200	,
ICWTHDT	 "Date of Consent Withdrawal"  length=	8	,
EOTSTT	 "End of Treatment Status"  length=	200	,
EOTDT	 "End of Treatment Date"  length=	8	,
DCTREAS	 "Reason for Discontinuation of Treatment"  length=	200	,
DCTREASP	 "Reason Specify for Discont of Treatment"  length=	200	,
DCTRSNB	 "Reason for Discontinuation from Nab-Pac"  length=	200	,
DCTRNBSP	 "Reason Specify for Discont of Nab-Pac"  length=	200	
from dm4;
quit;
PROC SORT NODUPKEY;BY _ALL_;RUN;
data ADAM.ADSL_ (LABEL="Subject-Level Analysis Dataset") ;
SET FINAL;
RUN;
