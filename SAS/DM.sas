/*******************************************************************
* Client: XXXX                                                           
* Product: XXXXX                                                  
* Project: Protocol: 043-1810                                                    
* Program: DM.sas 
*
* Program Type: SDTM
*
* Purpose: To produce the SDTM DM
* Usage Notes: 
*
* SAS� Version: 9.4 [TS2M0]
* Operating System: Windows 2003 R2 Standard Edition.                   
*
* Author: Mahesh
* Date Created: 09 apr 2024
*******************************************************************/

libname adam "C:\sas\Great online training material\CDISC Materials\ADAM DATASETS";	
libname SDTM "C:\sas\Great online training material\CDISC Materials\SDTM DATASETS";	
options nofmterr;
libname RAW  "C:\sas\Great online training material\CDISC Materials\RAW DATASETS";

DATA DM1;
SET RAW.DM  (RENAME=(AGE=AGEX SEX=SEXX ETHNIC=ETHNICX));
STUDYID= "043-18101";
DOMAIN="DM";
USUBJID=STRIP (STUDYID)||"-"||STRIP (SUBNUM);
SUBJID=SCAN (SUBNUM,2,'-');
SITEID= SCAN (SUBNUM,1,'-');
AGE=AGEX;
IF AGE NE . THEN
AGEU="YEARS";

BRTHDTC= STRIP (YOB);

****************10nov2020*************;

SEX=SEXX;

ETHNIC= UPCASE (STRIP (ETHNIC_DEC));

RUN;

DATA DM1;
SET DM1;
LENGTH RACE $100.;
IF WHITE ="X" THEN RACE="WHITE";
IF BLACK ="X" THEN RACE="BLACK";
IF NATIVE ="X" THEN RACE="NATIVE";
IF ASIAN ="X" THEN RACE="ASIAN";
IF PACIFIC ="X" THEN RACE="PACIFIC";
IF OTHER ="X" THEN RACE="OTHER";
IF RACEUNK ="X" THEN RACE="UNKNOWN";

RUN;

/*ISO8601 YYYY-MM-DDTHH:MM:SS*/

**************RFSTDTC and RFXSTDTC*******************;

DATA EX;
SET RAW.EX;
IF VISNAME="Cycle 1 - Day 1 (1st infusion)";
RFSTDTC  = PUT (EXSTDAT,YYMMDD10.)||"T"||PUT (EXSTTIM,TOD8.);
RFXSTDTC =  PUT (EXSTDAT,YYMMDD10.)||"T"||PUT (EXSTTIM,TOD8.);
KEEP SUBNUM RFSTDTC RFXSTDTC EXLEVEL;
RUN;

DATA EOT;
SET RAW.DS_EOT;
RFENDTCN= PUT (DSDAT,YYMMDD10.);
RFENDTCO= PUT (DSCDAT,YYMMDD10.);
KEEP SUBNUM RFENDTCN RFENDTCO;
RUN;

DATA RFENDTC2;
SET EOT ( WHERE =(DATE^='         .') RENAME = (RFENDTCN=DATE))
    EOT  (WHERE =(DATE^='         .') RENAME = (RFENDTCO=DATE))  ;
BY SUBNUM DATE;
IF LAST.SUBNUM;
RFXENDTC=DATE;
RFENDTC=DATE;
KEEP SUBNUM RFENDTC RFXENDTC;
RUN;



DATA DTH;
SET RAW.DTH;
DTHFL="Y";
DTHDTC=PUT (DTHDAT,YYMMDD10.);
KEEP SUBNUM DTHFL DTHDTC;
RUN;


DATA IC;
SET RAW.DS_IC;
IF PAGENAME = "Informed Consent" THEN 
RFICDTC=PUT (RFICDAT,YYMMDD10.);
IF RFICDTC NE '';
KEEP SUBNUM RFICDTC;
RUN;

DATA EOS;
SET RAW.EOS;
RFPENDTC= PUT (DSDAT,YYMMDD10.);
KEEP SUBNUM RFPENDTC;
RUN;

PROC SORT DATA=EX;BY SUBNUM;RUN;
PROC SORT DATA=DM1;BY SUBNUM;RUN;
PROC SORT DATA=IC;BY SUBNUM;RUN;
PROC SORT DATA=raw.ie out=ie;BY SUBNUM;RUN;
DATA DM2;
MERGE DM1 (IN=A)
IC
EX
RFENDTC2
DTH
EOS ie;

BY SUBNUM;
IF A;
RUN;




DATA DM3;
SET DM2;
	LENGTH studyid armcd actarmcd $20 domain $2 usubjid arm actarm $40 subjid siteid ageu country $200;

IF exlevel^='' THEN ARMCD='AIRIS'||SCAN(exlevel,1,'');
		ELSE IF ieyn_dec='No' THEN ARMCD='SCRNFAIL';
		ELSE ARMCD='NOTASSGN';

	IF exlevel^='' THEN ARM='AIRIS-101 '||SCAN(exlevel,1,'');
		ELSE IF ieyn_dec='No' THEN ARM='Screen Failure';
		ELSE ARM='Not Assigned';


	IF rfxstdtc^='' or armcd='SCRNFAIL' THEN ACTARMCD=armcd;
		ELSE ACTARMCD='NOTTRT';
	IF rfxstdtc^='' or armcd='SCRNFAIL' THEN ACTARM=arm;
		ELSE ACTARM='Not Treated';


	COUNTRY='USA';
	keep
	STUDYID
DOMAIN
USUBJID
SUBJID
SITEID
RFSTDTC
RFENDTC
RFXSTDTC
RFXENDTC
RFICDTC
RFPENDTC
DTHDTC
DTHFL
/*INVNAM*/
/*INVID*/
BRTHDTC
AGE
AGEU
SEX
RACE
ETHNIC
ARMCD
ARM
ACTARMCD
ACTARM
COUNTRY
/*DMDTC
DMDY*/
;
RUN;



PROC SQL;
CREATE TABLE FINAL AS 
SELECT
STUDYID "Study Identifier" LENGTH=	8	,
DOMAIN  "Domain Abbreviation" LENGTH=	2	,
USUBJID "Unique Subject Identifier" LENGTH=	50	,
SUBJID "Subject Identifier for the Study" LENGTH=	50	,
SITEID "Study Site Identifier" LENGTH=	20	,
RFSTDTC" Subject Reference Start Date/Time" LENGTH=	25	,
RFENDTC "Subject Reference End Date/Time" LENGTH=	25	,
RFXSTDTC "Date/Time of First Study Treatment" LENGTH=	25	,
RFXENDTC "Date/Time of Last Study Treatment" LENGTH=	25	,
RFICDTC "Date/Time of Informed Consent" LENGTH=	25	,
RFPENDTC "Date/Time of End of Participation" LENGTH=	25	,
DTHDTC "Date/Time of Death" LENGTH=	25	,
DTHFL "Subject Death Flag" LENGTH=	2	,
AGE "Age" LENGTH=	8	,
AGEU "Age Units" LENGTH=	6	,
SEX "Sex" LENGTH=	2	,
RACE "Race" LENGTH=	100	,
ARMCD "Planned Arm Code" LENGTH=	10,
ARM "Description of Planned Arm" LENGTH=	50	,
ACTARMCD "Actual Arm Code" LENGTH=	10	,
ACTARM "Description of Actual Arm" LENGTH=	50	,
COUNTRY "Country" LENGTH=	50	
FROM DM3;
QUIT;
RUN;

data sdtm.dm_ (label="Demographics");
set final;
run;











