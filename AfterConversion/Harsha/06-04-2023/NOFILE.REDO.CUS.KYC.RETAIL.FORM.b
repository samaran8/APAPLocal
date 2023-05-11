* @ValidationCode : MjotMzY3NzkyODUxOkNwMTI1MjoxNjgwNzcyNzU1NjQyOklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 06 Apr 2023 14:49:15
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE NOFILE.REDO.CUS.KYC.RETAIL.FORM(CUS.DET.ARR)
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This is Nofile routine to fetch the values for KYC form from CUSTOMER file
* This development is for ODR Reference ODR-2010-04-0425
* Input/Output:
*--------------
* IN  : CUST.ID
* OUT : CUST.ID
* Dependencies:
*---------------
* CALLS     : -NA-
* CALLED BY : -NA-
* Revision History:
*------------------------------------------------------------------------------------------
* Date              who              Reference            Description
* 13-MAY-2010       B Renugadevi     ODR-2010-04-0425     Initial Creation
* 13-MAY-2011       SUDHARSANAN S    PACS00023993         Modify as per issue
* 16 JUN 2011       MANJU.G          PACS00072193        Select stmt modified
* 18 JUL 2011       SUDHARSANAN S    PACS00084100        Mapping for most of the fields has been changed as per discussion with pamela
* 15 AUG 2011       SUDHARSANAN S    PACS00102852        Check the multi value for segment field
*  DATE             WHO                   REFERENCE 
* 06-APRIL-2023      Conversion Tool       R22 Auto Conversion  - VM to @VM , FM to @FM ,SM to @SM and ++ to +=1 , F.READ to CACHE.READ and -- to -=1
* 06-APRIL-2023      Harsha                R22 Manual Conversion - No changes
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.CUSTOMER
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.AZ.PRODUCT.PARAMETER
    $INSERT I_F.CUSTOMER.ACCOUNT
    $INSERT I_F.COUNTRY
    $INSERT I_F.USER
    $INSERT I_F.SECTOR
*   $INCLUDE GLOBUS.BP I_F.INDUSTRY
    $INSERT I_F.DEPT.ACCT.OFFICER
    $INSERT I_F.CATEGORY
    $INSERT I_F.REDO.RISK.GROUP
    $INSERT I_F.REDO.SECTOR.DOMI
    $INSERT I_F.REDO.URB.ENS.RES.DOMI
    $INSERT I_F.CR.PROFILE.TYPE
    $INSERT I_F.APAP.INDUSTRY

    GOSUB INIT
    GOSUB PROCESS
RETURN
******
INIT:
******
* To open initialise & open files.
    CUSTOMER.NO              = '' ;  VAR.RE.INV.CATEG = ''

    FN.CUSTOMER              = 'F.CUSTOMER'             ; F.CUSTOMER               = ''
    FN.ACCOUNT               = 'F.ACCOUNT'              ; F.ACCOUNT                = ''
    FN.CUSTOMER.ACCOUNT      = 'F.CUSTOMER.ACCOUNT'     ; F.CUSTOMER.ACCOUNT       = ''
    FN.SECTOR                = 'F.SECTOR'               ; F.SECTOR                 = ''
*   FN.INDUSTRY              = 'F.INDUSTRY'             ; F.INDUSTRY               = ''
    FN.APAP.INDUSTRY = 'F.APAP.INDUSTRY'
    F.APAP.INDUSTRY = ''
    CALL OPF(FN.APAP.INDUSTRY,F.APAP.INDUSTRY)
    FN.REDO.RISK.GROUP       = 'F.REDO.RISK.GROUP'      ; F.REDO.RISK.GROUP        = ''
    FN.REDO.SECTOR.DOMI      = 'F.REDO.SECTOR.DOMI'     ; F.REDO.SECTOR.DOMI       = ''
    FN.REDO.URB.ENS.RES.DOMI = 'F.REDO.URB.ENS.RES.DOMI'; F.REDO.URB.ENS.RES.DOMI  = ''
    FN.DEPT.ACCT.OFFICER     = 'F.DEPT.ACCT.OFFICER'    ; F.DEPT.ACCT.OFFICER      = ''
    FN.AZ.CUSTOMER           = 'F.AZ.CUSTOMER'          ; F.AZ.CUSTOMER            = ''
    FN.AZ.ACCOUNT            = 'F.AZ.ACCOUNT'           ; F.AZ.ACCOUNT             = ''
    FN.APP                   = 'F.AZ.PRODUCT.PARAMETER' ; F.APP                    = ''
    FN.CR.PROFILE.TYPE       = 'F.CR.PROFILE.TYPE'      ; F.CR.PROFILE.TYPE        = ''
    FN.COUNTRY               = 'F.COUNTRY'              ; F.COUNTRY                = ''
    FN.CATEGORY = 'F.CATEGORY' ; F.CATEGORY = ''
    FN.CUS.LEGAL.ID = 'F.REDO.CUSTOMER.LEGAL.ID' ;  F.CUS.LEGAL.ID = ''


    CALL OPF(FN.CUSTOMER, F.CUSTOMER)
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)
    CALL OPF(FN.COUNTRY,F.COUNTRY)
    CALL OPF(FN.SECTOR,F.SECTOR)
*   CALL OPF(FN.INDUSTRY,F.INDUSTRY)
    CALL OPF(FN.REDO.RISK.GROUP,F.REDO.RISK.GROUP)
    CALL OPF(FN.REDO.SECTOR.DOMI,F.REDO.SECTOR.DOMI)
    CALL OPF(FN.REDO.URB.ENS.RES.DOMI,F.REDO.URB.ENS.RES.DOMI)
    CALL OPF(FN.DEPT.ACCT.OFFICER,F.DEPT.ACCT.OFFICER)
    CALL OPF(FN.AZ.CUSTOMER,F.AZ.CUSTOMER)
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)
    CALL OPF(FN.APP,F.APP)
    CALL OPF(FN.CATEGORY,F.CATEGORY)
    CALL OPF(FN.CR.PROFILE.TYPE,F.CR.PROFILE.TYPE)
    CALL OPF(FN.CUS.LEGAL.ID,F.CUS.LEGAL.ID)

    GOSUB GET.LOCAL.FIELDS.POS

RETURN
*********************
GET.LOCAL.FIELDS.POS:
*********************
*To get local field position
    LREF.APPL                = 'CUSTOMER':@FM:'ACCOUNT':@FM:'AZ.ACCOUNT':@FM:'AZ.PRODUCT.PARAMETER'
    LREF.FIELDS              = 'L.CU.ALT.CU.COD':@VM:'L.CU.AGE':@VM:'L.CU.GRP.RIESGO':@VM:'L.CU.RES.SECTOR':@VM:'L.CU.URB.ENS.RE':@VM:'L.CU.TEL.AREA':@VM:'L.CU.TEL.NO':@VM:'L.CU.TEL.EXT':@VM:'L.CU.CIDENT':@VM:'L.APAP.INDUSTRY'
    LREF.FIELDS<-1>          = 'L.AC.SRCINITDEP':@VM:'L.AC.TOT.DEP':@VM:'L.AC.CASH.DEP':@VM:'L.AC.CHQ.DEP':@VM:'L.AC.TRANS.DEP':@VM:'L.AC.TOT.WITHDR':@VM:'L.AC.CASHWITHDR':@VM:'L.AC.CHQWITHDR':@VM:'L.AC.TRANS.WDR':@VM:'L.AC.QTY.DEPOS':@VM:'L.AC.QTY.WITHDR':@VM:'L.AC.PROP.USE':@VM:'L.AC.FUNDORIGIN'
    LREF.FIELDS<-1>          = 'L.AC.SRCINITDEP':@VM:'L.AC.PROP.USE':@VM:'L.AC.FUNDORIGIN'
    LREF.FIELDS<-1>          = 'L.AZ.RE.INV.CAT'
    LREF.POS                 = ''
    CALL MULTI.GET.LOC.REF(LREF.APPL, LREF.FIELDS, LREF.POS)
    L.CU.ALT.CU.COD.POS      = LREF.POS<1,1>  ; L.CU.AGE.POS        = LREF.POS<1,2> ;  L.CU.GROUP.RIESGO.POS = LREF.POS<1,3>
    L.CU.RES.SECTOR.POS      = LREF.POS<1,4>  ; L.CU.URB.ENS.RE.POS = LREF.POS<1,5> ;  L.CU.TEL.AREA.POS     = LREF.POS<1,6>
    L.CU.TEL.NO.POS          = LREF.POS<1,7>  ; L.CU.TEL.EXT.POS    = LREF.POS<1,8> ;  L.CU.CIDENT.POS       = LREF.POS<1,9>
    Y.POS.AC.SRCNI           = LREF.POS<2,1>  ; TOT.DEP.POS         = LREF.POS<2,2> ;  Y.POS.DEP.CASH        = LREF.POS<2,3>
    Y.POS.DEP.CHQ            = LREF.POS<2,4>  ; Y.POS.DEP.TR        = LREF.POS<2,5> ;  TOT.WITHD.POS         = LREF.POS<2,6>
    Y.POS.WITH.CASH          = LREF.POS<2,7>  ; Y.POS.WITH.CHQ      = LREF.POS<2,8> ;  Y.POS.WITH.TR         = LREF.POS<2,9>
    Y.POS.QTY.DEP            = LREF.POS<2,10> ; Y.POS.QTY.WITH      = LREF.POS<2,11>;  Y.POS.PROP.USE        = LREF.POS<2,12>
    Y.POS.FUND.ORIGIN        = LREF.POS<2,13> ; Y.POS.AZ.SRCINI     = LREF.POS<3,1> ;  Y.POS.AZ.PROP         = LREF.POS<3,2>
    Y.POS.AZ.FUND            = LREF.POS<3,3>  ; Y.POS.RE.INV.CAT    = LREF.POS<4,1>
    Y.L.APAP.INDUS.POS = LREF.POS<1,10>
    VAR.USER.LANG = R.USER<EB.USE.LANGUAGE>
    GOSUB GET.VALUES
    GOSUB GET.REINV.CATEG
RETURN
**********************
GET.REINV.CATEG:
**********************
*This para is used to get the reinvestment category value for all records in the Application
    SEL.APP = "SSELECT ":FN.APP:" WITH L.AZ.RE.INV.CAT NE '' "
    CALL EB.READLIST(SEL.APP,SEL.APP.LIST,'',NOR.APP,APP.ERR)
    APP.CNT = 1
    LOOP
    WHILE APP.CNT LE NOR.APP
        APP.ID = SEL.APP.LIST<APP.CNT>
        CALL F.READ(FN.APP,APP.ID,R.APP,F.APP,AP.ER)
        RE.INV.CATEG = R.APP<AZ.APP.LOCAL.REF,Y.POS.RE.INV.CAT>
        IF RE.INV.CATEG THEN
            VAR.RE.INV.CATEG<-1> = RE.INV.CATEG
        END
        APP.CNT += 1
    REPEAT
RETURN
**********
PROCESS:
**********
*PACS00072193 - S
*It checks the selection field
    VALUE.BK = D.RANGE.AND.VALUE ;  OPERAND.BK = D.LOGICAL.OPERANDS ;  FIELDS.BK = D.FIELDS
    D.RANGE.AND.VALUE='' ; D.LOGICAL.OPERANDS=''; D.FIELDS=''
    CUS.APP.FLDS = '@ID':@FM:'LEGAL.ID':@FM:'L.CU.CIDENT'
    LOOP
        REMOVE APP.FLD FROM CUS.APP.FLDS SETTING CUS.FLD.POS
    WHILE APP.FLD:CUS.FLD.POS
        GOSUB CHECK.SELECTION.FIELD
    REPEAT
    GOSUB FORM.SELECT.STATEMENT
*PACS00072193 - E
RETURN
***************************************************************************************
CHECK.SELECTION.FIELD:
************************
    LOCATE APP.FLD IN FIELDS.BK<1> SETTING POS1 THEN
        IF APP.FLD NE 'LEGAL.ID' THEN
            D.RANGE.AND.VALUE<-1>=VALUE.BK<POS1>
            D.LOGICAL.OPERANDS<-1>=OPERAND.BK<POS1>
            D.FIELDS<-1>=FIELDS.BK<POS1>

        END ELSE
            GOSUB CHECK.PASSPORT
        END
    END
RETURN
*--------------------------------------------------------------------------------------
CHECK.PASSPORT:
*--------------------------------------------------------------------------------------
* To check passport number validation for perfomance issue
    PASSPORT.NUMBER =  VALUE.BK<POS1>
    CALL F.READ(FN.CUS.LEGAL.ID,PASSPORT.NUMBER,R.CUS.LEGAL.ID,F.CUS.LEGAL.ID,CUS.LEGAL.ERR)
    IF R.CUS.LEGAL.ID THEN
        CUS.ID = FIELD(R.CUS.LEGAL.ID,"*",2)
    END ELSE
        CUS.ID = ''
    END
    D.RANGE.AND.VALUE<-1>= CUS.ID
    D.LOGICAL.OPERANDS<-1>= "1" ;*****IT IS NOTHING BUT EQ OPERAND
    D.FIELDS<-1>= "@ID"
RETURN
***********************
FORM.SELECT.STATEMENT:
***********************
* It forms the selects statement
    IF D.FIELDS NE '' THEN
        FILE.NAME = FN.CUSTOMER
        CALL APAP.REDOENQ.REDO.E.FORM.SEL.STMT(FILE.NAME, '', '', SEL.CUS.CMD)
        SEL.CMD1 = SEL.CUS.CMD:" AND (L.CU.TIPO.CL NE 'PERSONA JURIDICA')"
    END ELSE
        SEL.CMD1 = "SELECT ":FN.CUSTOMER:" WITH (L.CU.TIPO.CL NE 'PERSONA JURIDICA')"
    END
    CALL EB.READLIST(SEL.CMD1,SEL.LIST,'',NOR,SEL.ERR)
    Y.SEL.COUNT = DCOUNT(SEL.LIST,@FM)
    Y.SEL.CT.LOOP = 1
    LOOP
    WHILE Y.SEL.CT.LOOP LE Y.SEL.COUNT
        CUST.ID = SEL.LIST<Y.SEL.CT.LOOP>
        R.CUST                     = '' ; CUST.ERR      = ''
        CALL F.READ(FN.CUSTOMER,CUST.ID,R.CUST,F.CUSTOMER,CUST.ERR)
        Y.SEL.CT.LOOP + =1
        GOSUB PROCESS.EXTRACT
    REPEAT
RETURN
***************************************************************************************
PROCESS.EXTRACT:
*****************
* this para is used to fetch customer details
    GOSUB FIRST.SET
    GOSUB SECOND.SET
    GOSUB THIRD.SET
    GOSUB JAVA.VALIDATION1
    START.DATE.APAP          = R.CUST<EB.CUS.CUSTOMER.SINCE>
    VAR.EXEC.RESPONSIBLE     = R.CUST<EB.CUS.ACCOUNT.OFFICER,1>
    CALL CACHE.READ(FN.DEPT.ACCT.OFFICER, VAR.EXEC.RESPONSIBLE, R.DEPT.ACCT.OFFICER, DEPT.ERR)
    EXEC.RESPONSIBLE  = R.DEPT.ACCT.OFFICER<EB.DAO.NAME>
*  EXEC.SECONDARY           = ''       ;************This field is removed as discussion with client
    CALL REDO.RELCUS.EDU.LVL(CUST.ID,EDU.LVEL)
    EDU.LEVEL                = EDU.LVEL
    GOSUB GET.SEGMENTO
    GOSUB GET.CUS.AC.DET
    GOSUB FORM.CUST.DET.ARRAY

RETURN
*-------------
FIRST.SET:
*------------
* It is used get first set of values
    CALL REDO.EXEC.SEC.INFO(CUST.ID,OTH.OFFICER)
    BRANCH                   = OTH.OFFICER
    CUSTOMER.CODE            = CUST.ID
*    ALTERNATE.CUSTOMER.NO    = R.CUST<EB.CUS.LOCAL.REF,L.CU.ALT.CU.COD.POS>     ;*******This field is removed as discussion with client
    Y.DATE                   = TODAY
    CALL REDO.SHOW.CUST.DESC(CUST.ID,STATUS.DESC)
    Y.STATUS                 = STATUS.DESC
    SURNAMES                 = R.CUST<EB.CUS.FAMILY.NAME>
    NAMES                    = R.CUST<EB.CUS.GIVEN.NAMES>
    CALL REDO.DS.FMT.TYP.CUST(CUST.ID,CUS.TYPE)
    TYPE.OF.IDENTIFICATION   = CUS.TYPE
    CALL REDO.IDENTITY.NUMBER(CUST.ID,ID.NUM)
    ID.NR                    = ID.NUM
    EXPIRATION.DATE          = R.CUST<EB.CUS.LEGAL.EXP.DATE>
    VAR.GENDER               = R.CUST<EB.CUS.GENDER>
    LOCATE VAR.GENDER IN Y.LOOKUP.LIST7 SETTING POS7 THEN
        GENDER = Y.LOOKUP.DESC7<POS7,VAR.USER.LANG>
        IF NOT(GENDER) THEN
            GENDER = Y.LOOKUP.DESC7<POS7,1>
        END
    END
    DATE.OF.BIRTH            = R.CUST<EB.CUS.DATE.OF.BIRTH>
    AGE                      = R.CUST<EB.CUS.LOCAL.REF,L.CU.AGE.POS>
    VAR.SECTOR               = R.CUST<EB.CUS.SECTOR>
    CALL CACHE.READ(FN.SECTOR, VAR.SECTOR, R.SECTOR, SEC.ERR)     ;*R22 Auto Conversion  - F.READ to CACHE.READ
    SECTOR                   = R.SECTOR<EB.SEC.DESCRIPTION,VAR.USER.LANG>
    IF NOT(SECTOR) THEN
        SECTOR                = R.SECTOR<EB.SEC.DESCRIPTION,1>
    END
    NATIONAL.CODE            = R.CUST<EB.CUS.NATIONALITY>
    CALL CACHE.READ(FN.COUNTRY, NATIONAL.CODE, R.COUNTRY, COUNT.ERR)    ;*R22 Auto Conversion  - F.READ to CACHE.READ
    IF R.COUNTRY THEN
        NATIONALITY          = R.COUNTRY<EB.COU.COUNTRY.NAME>
    END
RETURN
*------------
SECOND.SET:
*-----------
* Second set of values
    VAR.MARITAL.STATUS       = R.CUST<EB.CUS.MARITAL.STATUS>
    LOCATE VAR.MARITAL.STATUS IN Y.LOOKUP.LIST8 SETTING POS8 THEN
        MARITAL.STATUS = Y.LOOKUP.DESC8<POS8,VAR.USER.LANG>
        IF NOT(MARITAL.STATUS) THEN
            MARITAL.STATUS = Y.LOOKUP.DESC8<POS8,1>
        END
    END
*   VAR.MAIN.ECONOMIC.ACT    = R.CUST<EB.CUS.INDUSTRY>
    VAR.MAIN.ECONOMIC.ACT = R.CUST<EB.CUS.LOCAL.REF,Y.L.APAP.INDUS.POS>
*   CALL F.READ(FN.INDUSTRY,VAR.MAIN.ECONOMIC.ACT,R.INDUSTRY,F.INDUSTRY,IND.ERR)
    CALL F.READ(FN.APAP.INDUSTRY,VAR.MAIN.ECONOMIC.ACT,R.INDUSTRY,F.APAP.INDUSTRY,IND.ERR)
*   MAIN.ECONOMIC.ACTIVITY   = R.INDUSTRY<EB.IND.DESCRIPTION,VAR.USER.LANG>
    MAIN.ECONOMIC.ACTIVITY = R.INDUSTRY<REDO.DESCRIPTION,VAR.USER.LANG>
    IF NOT(MAIN.ECONOMIC.ACTIVITY) THEN
*       MAIN.ECONOMIC.ACTIVITY   = R.INDUSTRY<EB.IND.DESCRIPTION,1>
        MAIN.ECONOMIC.ACTIVITY = R.INDUSTRY<REDO.DESCRIPTION,1>
    END
    VAR.PROFESSION           = R.CUST<EB.CUS.EMPLOYMENT.STATUS>
    LOCATE VAR.PROFESSION IN Y.LOOKUP.LIST9 SETTING POS9 THEN
        PROFESSION = Y.LOOKUP.DESC9<POS9,VAR.USER.LANG>
        IF NOT(PROFESSION) THEN
            PROFESSION = Y.LOOKUP.DESC9<POS9,1>
        END
    END
*   OCCUPATION               = ''       ;******R.CUST<EB.CUS.JOB.TITLE>***************This field is removed as discussion with client
    VAR.GROUP.RISK           = R.CUST<EB.CUS.LOCAL.REF,L.CU.GROUP.RIESGO.POS>
    VAR.GROUP.JAVA.RISK = ''
    CHANGE @SM TO @FM IN VAR.GROUP.RISK
    RISK.GROUP.CNT = DCOUNT(VAR.GROUP.RISK,@FM) ; CNT.RISK = 1
    LOOP
    WHILE CNT.RISK LE RISK.GROUP.CNT
        VAR.GROUP.RISK.ID = VAR.GROUP.RISK<CNT.RISK>
        CALL F.READ(FN.REDO.RISK.GROUP,VAR.GROUP.RISK.ID,R.REDO.RISK.GROUP,F.REDO.RISK.GROUP,RISK.ERR)
        GROUP.RISK<1,-1>     = R.REDO.RISK.GROUP<RG.RISK.GRP.DESC,1>
        IF VAR.GROUP.JAVA.RISK EQ '' THEN
            VAR.GROUP.JAVA.RISK = R.REDO.RISK.GROUP<RG.RISK.GRP.DESC,1>
        END ELSE
            VAR.GROUP.JAVA.RISK = VAR.GROUP.JAVA.RISK : '##' : R.REDO.RISK.GROUP<RG.RISK.GRP.DESC,1>
        END
        CNT.RISK += 1
    REPEAT
RETURN
*------------
THIRD.SET:
*-----------
* Third set of values
    DATE.OF.ENTRY            = R.CUST<EB.CUS.LEGAL.ISS.DATE,1>
    VAR.ADDRESS.TYPE         = R.CUST<EB.CUS.RESIDENCE.TYPE>
    LOCATE VAR.ADDRESS.TYPE IN Y.LOOKUP.LIST10 SETTING POS10 THEN
        ADDRESS.TYPE = Y.LOOKUP.DESC10<POS10,VAR.USER.LANG>
        IF NOT(ADDRESS.TYPE) THEN
            ADDRESS.TYPE = Y.LOOKUP.DESC10<POS10,1>
        END
    END
    VAR.COUNTRY              = R.CUST<EB.CUS.RESIDENCE>
    CALL CACHE.READ(FN.COUNTRY, VAR.COUNTRY, R.COUNTRY, COUNT.ERR)    ;*R22 Auto Conversion  - F.READ to CACHE.READ
    IF R.COUNTRY THEN
        COUNTRY              = R.COUNTRY<EB.COU.COUNTRY.NAME>
    END
    PROVINCE                 = R.CUST<EB.CUS.TOWN.COUNTRY>
    CITY                     = R.CUST<EB.CUS.COUNTRY>
    VAR.SECTOR.2             = R.CUST<EB.CUS.LOCAL.REF,L.CU.RES.SECTOR.POS>
    CALL F.READ(FN.REDO.SECTOR.DOMI,VAR.SECTOR.2,R.SECTOR.DOMI,F.REDO.SECTOR.DOMI,DOMI.ERR)
    SECTOR.2                 = R.SECTOR.DOMI<XX.YY.DESCRIPTION,VAR.USER.LANG>
    IF NOT(SECTOR.2) THEN
        SECTOR.2             = R.SECTOR.DOMI<XX.YY.DESCRIPTION,1>
    END
    VAR.URB.ENS.RE           = R.CUST<EB.CUS.LOCAL.REF,L.CU.URB.ENS.RE.POS>
    CALL F.READ(FN.REDO.URB.ENS.RES.DOMI,VAR.URB.ENS.RE,R.URB.ENS.RE,F.REDO.URB.ENS.RES.DOMI,URB.ERR)
    URB.ENS.RE               = R.URB.ENS.RE<XX.YY.DESCRIPTION,VAR.USER.LANG>
    IF NOT(URB.ENS.RE) THEN
        URB.ENS.RE           = R.URB.ENS.RE<XX.YY.DESCRIPTION,1>
    END
    STREET                   = R.CUST<EB.CUS.STREET>
    NUMBER.SUITABLE          = R.CUST<EB.CUS.ADDRESS>
    ZIP.CODE                 = R.CUST<EB.CUS.POST.CODE>
    PO.BOX                   = R.CUST<EB.CUS.OFF.PHONE>
    E.MAIL                   = R.CUST<EB.CUS.EMAIL.1>
    CALL REDO.RELCUS.TEL.TYPE(CUST.ID,VAR.TEL.TYPE,VAR.TEL.AREA,VAR.TEL.NUMBER,VAR.TEL.EXT,VAR.TEL.CONTACT)
    TIPO                     = VAR.TEL.TYPE
    AREA                     = VAR.TEL.AREA
    NUMBER                   = VAR.TEL.NUMBER
    EXTENSION                = VAR.TEL.EXT
RETURN
*----------------------------------------------------------------------------------------------------------
JAVA.VALIDATION1:
*----------------------------------------------------------------------------------------------------------
* Java validtion is handled in this part
    JAVA.TIPO=TIPO
    JAVA.AREA=AREA
    JAVA.NUMBER=NUMBER
    JAVA.EXTENSION=EXTENSION
    CHANGE @VM TO '##' IN JAVA.TIPO
    CHANGE @VM TO '##' IN JAVA.AREA
    CHANGE @VM TO '##' IN JAVA.NUMBER
    CHANGE @VM TO '##' IN JAVA.EXTENSION

    COMPANY.NAME             = R.CUST<EB.CUS.EMPLOYERS.NAME>
    SINCE.DATE               = R.CUST<EB.CUS.EMPLOYMENT.START>
    MONTHLY.INCOME           = R.CUST<EB.CUS.SALARY>
    CALL REDO.DS.SPOUSE.DETAILS(CUST.ID,SPOUSE.IDENT,SPOUSE.ID,FAM.NAME,GIV.NAME)
    SPOUSE.IDENTIFICATION    = SPOUSE.IDENT
    SPOUSE.ID.NR             = SPOUSE.ID
    SPOUSE.SURNAMES          = FAM.NAME
    SPOUSE.NAMES             = GIV.NAME

    JAVA.SP.IDENT=SPOUSE.IDENTIFICATION
    JAVA.SP.ID=SPOUSE.ID.NR
    JAVA.SP.SURNAMES=SPOUSE.SURNAMES
    JAVA.SP.NAMES=SPOUSE.NAMES

    CHANGE @VM TO '##' IN JAVA.SP.IDENT
    CHANGE @VM TO '##' IN JAVA.SP.ID
    CHANGE @VM TO '##' IN JAVA.SP.SURNAMES
    CHANGE @VM TO '##' IN JAVA.SP.NAMES

RETURN
*---------------------------------------------------------------------------------------------------------
GET.SEGMENTO:
*----------------------------------------------------------------------------------------------------------
*PACS00102852 - S
    VAR.SEGMENT              = R.CUST<EB.CUS.CR.PROFILE.TYPE>
    CHANGE @VM TO @FM IN VAR.SEGMENT
    SEG.CNT = DCOUNT(VAR.SEGMENT,@FM) ; CNT.SEGMENT = 1
    LOOP
    WHILE CNT.SEGMENT LE SEG.CNT
        VAR.SEGMENT.ID = VAR.SEGMENT<CNT.SEGMENT>
        CALL F.READ(FN.CR.PROFILE.TYPE,VAR.SEGMENT.ID,R.CR.PROFILE.TYPE,F.CR.PROFILE.TYPE,CR.ERR)
        Y.SEGMENT = ''
        Y.SEGMENT                = R.CR.PROFILE.TYPE<CR.PFL.TYP.DESC,VAR.USER.LANG>
        IF NOT(Y.SEGMENT) THEN
            SEGMENTO<1,-1>              = R.CR.PROFILE.TYPE<CR.PFL.TYP.DESC,1>
        END ELSE
            SEGMENTO<1,-1>              = Y.SEGMENT
        END
        CNT.SEGMENT += 1
    REPEAT
*PACS00102852 - E
RETURN
*---------------------------------------------------------------------------------------------------
GET.CUS.AC.DET:
*---------------------------------------------------------------------------------------------------
    CALL F.READ(FN.CUSTOMER.ACCOUNT,CUST.ID,R.CUS.ACC,F.CUSTOMER.ACCOUNT,CUS.AC.ER)
    CALL F.READ(FN.AZ.CUSTOMER,CUST.ID,R.AZ.CUS.ACC,F.AZ.CUSTOMER,AZ.CUS.ACC.ER)
    GOSUB GET.ACCOUNT.DETAILS
    GOSUB GET.AZACC.DETAILS
RETURN
*---------------------------------------------------------------------------------------------------------
FORM.CUST.DET.ARRAY:
*-----------------------------------------------------------------------------------------------------------
    GOSUB JAVA.VALIDATION2
***********             1              2                       3                 4                  5
    CUS.DET.ARR<-1>= BRANCH:"VAR.SEP":CUSTOMER.CODE:"VAR.SEP":Y.DATE:"VAR.SEP":Y.STATUS:"VAR.SEP":SURNAMES
***********                 6                        7                       8                   9                      10                  11
    CUS.DET.ARR:="VAR.SEP":NAMES:"VAR.SEP":TYPE.OF.IDENTIFICATION:"VAR.SEP":ID.NR:"VAR.SEP":EXPIRATION.DATE:"VAR.SEP":GENDER:"VAR.SEP":DATE.OF.BIRTH
***********                 12             13               14                    15                       16                              17
    CUS.DET.ARR:="VAR.SEP":AGE:"VAR.SEP":SECTOR:"VAR.SEP":NATIONALITY:"VAR.SEP":MARITAL.STATUS:"VAR.SEP":MAIN.ECONOMIC.ACTIVITY:"VAR.SEP":PROFESSION
***********                   18                     19                    20                    21                  22              23
    CUS.DET.ARR:="VAR.SEP":GROUP.RISK:"VAR.SEP":DATE.OF.ENTRY:"VAR.SEP":ADDRESS.TYPE:"VAR.SEP":COUNTRY:"VAR.SEP":PROVINCE:"VAR.SEP":CITY
**********                   24                  25                 26                    27                    28                29                30              31
    CUS.DET.ARR:="VAR.SEP":SECTOR.2:"VAR.SEP":URB.ENS.RE:"VAR.SEP":STREET:"VAR.SEP":NUMBER.SUITABLE:"VAR.SEP":ZIP.CODE:"VAR.SEP":PO.BOX:"VAR.SEP":E.MAIL:"VAR.SEP":TIPO
**********                  32              33               34                   35                    36                     37                          38
    CUS.DET.ARR:="VAR.SEP":AREA:"VAR.SEP":NUMBER:"VAR.SEP":EXTENSION:"VAR.SEP":COMPANY.NAME:"VAR.SEP":SINCE.DATE:"VAR.SEP":MONTHLY.INCOME:"VAR.SEP":SPOUSE.IDENTIFICATION
**********                     39                      40                      41                       42                        43                      44
    CUS.DET.ARR:="VAR.SEP":SPOUSE.ID.NR:"VAR.SEP":SPOUSE.SURNAMES:"VAR.SEP":SPOUSE.NAMES:"VAR.SEP":START.DATE.APAP:"VAR.SEP":EXEC.RESPONSIBLE:"VAR.SEP":EDU.LEVEL
*********                     45                  46                   47                     48                      49                50                 51                 52                  53
    CUS.DET.ARR:="VAR.SEP":SEGMENTO:"VAR.SEP":VAR.ACCOUNT:"VAR.SEP":PRODUCT.CODE:"VAR.SEP":DEPOSIT.SOURCE:"VAR.SEP":TOT.DEP:"VAR.SEP":DEP.CASH:"VAR.SEP":DEP.CHQ:"VAR.SEP":DEP.TRANS:"VAR.SEP":TOT.WITHD
********                     54                  55                  56                  57                58                 59                     60                         J61
    CUS.DET.ARR:="VAR.SEP":WITH.CASH:"VAR.SEP":WITH.CHQ:"VAR.SEP":WITH.TRANS:"VAR.SEP":DEP.QTY:"VAR.SEP":WITH.QTY:"VAR.SEP":Y.PROP.USE:"VAR.SEP":FUND.ORIGIN:"VAR.SEP":JAVA.BUNCH.SET
RETURN
*-----------------------------------------------------------------------------------------------------
JAVA.VALIDATION2:
*-----------------------------------------------------------------------------------------------------

    JAVA.VAR.ACCOUNT=VAR.ACCOUNT
    JAVA.PRODUCT.CODE=PRODUCT.CODE
    JAVA.DEPOSIT.SOURCE=DEPOSIT.SOURCE
    JAVA.TOT.DEP=TOT.DEP
    JAVA.DEP.CASH=DEP.CASH
    JAVA.DEP.CHQ=DEP.CHQ
    JAVA.DEP.TRANS=DEP.TRANS
    JAVA.TOT.WITHD=TOT.WITHD
    JAVA.WITH.CASH=WITH.CASH
    JAVA.WITH.CHQ=WITH.CHQ
    JAVA.WITH.TRANS=WITH.TRANS
    JAVA.DEP.QTY=DEP.QTY
    JAVA.WITH.QTY=WITH.QTY
    JAVA.Y.PROP.USE=Y.PROP.USE
    JAVA.FUND.ORIGIN=FUND.ORIGIN
    JAVA.SEGMENTO=SEGMENTO

    CHANGE @VM TO '##' IN JAVA.VAR.ACCOUNT
    CHANGE @VM TO '##' IN JAVA.PRODUCT.CODE
    CHANGE @VM TO '##' IN JAVA.DEPOSIT.SOURCE
    CHANGE @SM TO '#@#' IN JAVA.DEPOSIT.SOURCE
    CHANGE @VM TO '##' IN JAVA.TOT.DEP
    CHANGE @VM TO '##' IN JAVA.DEP.CASH
    CHANGE @VM TO '##' IN JAVA.DEP.CHQ
    CHANGE @VM TO '##' IN JAVA.DEP.TRANS
    CHANGE @VM TO '##' IN JAVA.TOT.WITHD
    CHANGE @VM TO '##' IN JAVA.WITH.CASH
    CHANGE @VM TO '##' IN JAVA.WITH.CHQ
    CHANGE @VM TO '##' IN JAVA.WITH.TRANS
    CHANGE @VM TO '##' IN JAVA.DEP.QTY
    CHANGE @VM TO '##' IN JAVA.WITH.QTY
    CHANGE @VM TO '##' IN JAVA.Y.PROP.USE
    CHANGE @VM TO '##' IN JAVA.FUND.ORIGIN
    CHANGE @SM TO '#@#' IN JAVA.FUND.ORIGIN
    CHANGE @VM TO '##' IN JAVA.SEGMENTO

    JAVA.BUNCH.SET="^^SFD1=":JAVA.VAR.ACCOUNT:"^^SFD2=":JAVA.PRODUCT.CODE:"^^SFD3=":JAVA.DEPOSIT.SOURCE:"^^SFD4=":JAVA.TOT.DEP:"^^SFD5=":JAVA.DEP.CASH:"^^SFD6=":JAVA.DEP.CHQ:"^^SFD7=":JAVA.DEP.TRANS:"^^SFD8=":JAVA.TOT.WITHD:"^^SFD9=":JAVA.WITH.CASH:"^^SFD10=":JAVA.WITH.CHQ:"^^SFD11=":JAVA.WITH.TRANS:"^^SFD12=":JAVA.DEP.QTY:"^^SFD13=":JAVA.WITH.QTY:"^^SFD14=":JAVA.Y.PROP.USE:"^^SFD15=":JAVA.FUND.ORIGIN
    JAVA.BUNCH.SET:="^^SFD16=":JAVA.TIPO:"^^SFD17=":JAVA.AREA:"^^SFD18=":JAVA.NUMBER:"^^SFD19=":JAVA.EXTENSION:"^^SFD20=":JAVA.SP.IDENT:"^^SFD21=":JAVA.SP.ID:"^^SFD22=":JAVA.SP.SURNAMES:"^^SFD23=":JAVA.SP.NAMES:"^^SFD24=":VAR.GROUP.JAVA.RISK:"^^SFD25=":JAVA.SEGMENTO

RETURN
*------------------------------------------------------------------------------------------------------
GET.ACCOUNT.DETAILS:
*------------------------------------------------------------------------------------------------------
*This para is used to get the account details with in range of  1000 and 1999 category
    LOOP
        REMOVE Y.AC.ID FROM R.CUS.ACC SETTING POS.AC
    WHILE Y.AC.ID:POS.AC
        LOCATE Y.AC.ID IN R.AZ.CUS.ACC SETTING POS.AZ ELSE
            CALL F.READ(FN.ACCOUNT,Y.AC.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
            PROD.CODE = R.ACCOUNT<AC.CATEGORY>
            GOSUB CHECK.PROD.CODE
        END
    REPEAT
RETURN
*-------------------------------------------------------------------------------------------------
CHECK.PROD.CODE:
*--------------------------------------------------------------------------------------------------
    LOCATE PROD.CODE IN VAR.RE.INV.CATEG SETTING POS.RE ELSE
        IF (PROD.CODE GE 1000 AND PROD.CODE LE 1999) OR (PROD.CODE GE 6000 AND PROD.CODE LE 6599) THEN
            GOSUB CATEG.DESC
            PRODUCT.CODE<1,-1> = PROD.CODE.DESC
            VAR.ACCOUNT<1,-1> = Y.AC.ID
            DEP.CODE           = R.ACCOUNT<AC.LOCAL.REF,Y.POS.AC.SRCNI>
            GOSUB CHK.DEP.CODE
            GOSUB CHK.DEP.AMT
            GOSUB CHK.WITHDR.AMT
            GOSUB CHK.WITH.DEP.QTY
            PROP.USE = R.ACCOUNT<AC.LOCAL.REF,Y.POS.PROP.USE>
            GOSUB CHK.PR.USE
            VAL.FUND.ORIGIN =  R.ACCOUNT<AC.LOCAL.REF,Y.POS.FUND.ORIGIN>
            GOSUB CHK.FUND.ORIGIN
        END
    END
RETURN
*--------------------------------------------------------------------------------------------------
GET.AZACC.DETAILS:
*---------------------------------------------------------------------------------------------------
    LOOP
        REMOVE Y.AZ.AC.ID FROM R.AZ.CUS.ACC SETTING POS.AZ.ACC
    WHILE Y.AZ.AC.ID:POS.AZ.ACC
        CALL F.READ(FN.AZ.ACCOUNT,Y.AZ.AC.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,AZ.ACC.ERR)
        PROD.CODE = R.AZ.ACCOUNT<AZ.CATEGORY>
        IF PROD.CODE GE 6600 AND PROD.CODE LE 6699 THEN
            GOSUB CATEG.DESC
            PRODUCT.CODE<1,-1> = PROD.CODE.DESC
            VAR.ACCOUNT<1,-1> = Y.AZ.AC.ID
            DEP.CODE           = R.AZ.ACCOUNT<AZ.LOCAL.REF,Y.POS.AZ.SRCINI>
            GOSUB CHK.DEP.CODE
            TOT.DEP<1,-1> = 'N/A'   ; DEP.CASH<1,-1> = 'N/A'  ; DEP.CHQ<1,-1> = 'N/A'  ; DEP.TRANS<1,-1> = 'N/A'
            TOT.WITHD<1,-1> = 'N/A' ; WITH.CASH<1,-1> = 'N/A' ; WITH.CHQ<1,-1> = 'N/A' ; WITH.TRANS<1,-1> =  'N/A'
            DEP.QTY<1,-1> = 'N/A'   ; WITH.QTY<1,-1> = 'N/A'
            PROP.USE = R.AZ.ACCOUNT<AZ.LOCAL.REF,Y.POS.AZ.PROP>
            GOSUB CHK.PR.USE
            VAL.FUND.ORIGIN =  R.AZ.ACCOUNT<AZ.LOCAL.REF,Y.POS.AZ.FUND>
            GOSUB CHK.FUND.ORIGIN
        END
    REPEAT
RETURN
*---------------------------------------------------------------------------
CHK.DEP.CODE:
*----------------------------------------------------------------------------
    IF DEP.CODE THEN
        GOSUB AC.SRCINI
        DEPOSIT.SOURCE<1,-1> = VAR.DEPOSIT.SOURCE
        JAVA.DEP.SOC=VAR.DEPOSIT.SOURCE
    END ELSE
        DEPOSIT.SOURCE<1,-1> = 'N/A'
    END
    CHANGE @SM TO '##' IN JAVA.DEP.SOC
RETURN
*----------------------------------------------------------------------------
AC.SRCINI:
*-----------------------------------------------------------------------------
    CNT.SRC = DCOUNT(DEP.CODE,@SM) ; COUNT.SRC = 1   ; VAR.DEPOSIT.SOURCE = ''
    LOOP
    WHILE COUNT.SRC LE CNT.SRC
        VAR.DEP.CODE = DEP.CODE<1,1,COUNT.SRC>
        LOCATE VAR.DEP.CODE IN Y.LOOKUP.LIST11 SETTING POS11 THEN
            VAL.DEP.CODE = ''
            VAL.DEP.CODE = Y.LOOKUP.DESC11<POS11,VAR.USER.LANG>
            IF NOT(VAL.DEP.CODE) THEN
                VAR.DEPOSIT.SOURCE<1,1,-1> = Y.LOOKUP.DESC11<POS11,1>
            END ELSE
                VAR.DEPOSIT.SOURCE<1,1,-1> = VAL.DEP.CODE
            END
        END
        COUNT.SRC += 1
    REPEAT
RETURN
*------------------------------------------------------------------------------------------------------
CHK.DEP.AMT:
*------------------------------------------------------------------------------------------------------
* This is to get Dep Amount.
    VAR.TOT.DEP     = R.ACCOUNT<AC.LOCAL.REF,TOT.DEP.POS>
    IF VAR.TOT.DEP THEN
        TOT.DEP<1,-1> = VAR.TOT.DEP
    END ELSE
        TOT.DEP<1,-1> = 'N/A'
    END

    VAR.DEP.CASH = R.ACCOUNT<AC.LOCAL.REF,Y.POS.DEP.CASH>
    LOCATE VAR.DEP.CASH IN Y.LOOKUP.LIST1 SETTING POS1 THEN
        DEP.CASH<1,-1> = Y.LOOKUP.DESC1<POS1>
    END ELSE
        DEP.CASH<1,-1> = 'N/A'
    END

    VAR.DEP.CHQ = R.ACCOUNT<AC.LOCAL.REF,Y.POS.DEP.CHQ>
    LOCATE VAR.DEP.CHQ IN Y.LOOKUP.LIST2 SETTING POS2 THEN
        DEP.CHQ<1,-1> = Y.LOOKUP.DESC2<POS2>
    END ELSE
        DEP.CHQ<1,-1> = 'N/A'
    END

    VAR.DEP.TRANS = R.ACCOUNT<AC.LOCAL.REF,Y.POS.DEP.TR>
    LOCATE VAR.DEP.TRANS IN Y.LOOKUP.LIST3 SETTING POS3 THEN
        DEP.TRANS<1,-1> = Y.LOOKUP.DESC3<POS3>
    END ELSE
        DEP.TRANS<1,-1> = 'N/A'
    END

RETURN
*------------------------------------------------------------------------------------------------------------
CHK.WITHDR.AMT:
*-----------------------------------------------------------------------------------------------------------
    VAR.TOT.WITHD  = R.ACCOUNT<AC.LOCAL.REF,TOT.WITHD.POS>
    IF VAR.TOT.WITHD THEN
        TOT.WITHD<1,-1> = VAR.TOT.WITHD
    END ELSE
        TOT.WITHD<1,-1> = 'N/A'
    END

    VAR.WITH.CASH = R.ACCOUNT<AC.LOCAL.REF,Y.POS.WITH.CASH>
    LOCATE VAR.WITH.CASH IN Y.LOOKUP.LIST4 SETTING POS4 THEN
        WITH.CASH<1,-1> =  Y.LOOKUP.DESC4<POS4>
    END ELSE
        WITH.CASH<1,-1> = 'N/A'
    END

    VAR.WITH.CHQ = R.ACCOUNT<AC.LOCAL.REF,Y.POS.WITH.CHQ>
    LOCATE VAR.WITH.CHQ IN Y.LOOKUP.LIST5 SETTING POS5 THEN
        WITH.CHQ<1,-1> = Y.LOOKUP.DESC5<POS5>
    END ELSE
        WITH.CHQ<1,-1> = 'N/A'
    END

    VAR.WITH.TRANS = R.ACCOUNT<AC.LOCAL.REF,Y.POS.WITH.TR>
    LOCATE VAR.WITH.CASH IN Y.LOOKUP.LIST6 SETTING POS6 THEN
        WITH.TRANS<1,-1> =  Y.LOOKUP.DESC6<POS6>
    END ELSE
        WITH.TRANS<1,-1> = 'N/A'
    END
RETURN
*------------------------------------------------------------------------------------------------------
CHK.WITH.DEP.QTY:
*------------------------------------------------------------------------------------------------------
**********IT IS USED TO CHECK DEP WITH QTY*****
    VAR.DEP.QTY =R.ACCOUNT<AC.LOCAL.REF,Y.POS.QTY.DEP>
    IF VAR.DEP.QTY THEN
        DEP.QTY<1,-1> = VAR.DEP.QTY
    END ELSE
        DEP.QTY<1,-1> = 'N/A'
    END

    VAR.WITH.QTY  = R.ACCOUNT<AC.LOCAL.REF,Y.POS.QTY.WITH>
    IF VAR.WITH.QTY THEN
        WITH.QTY<1,-1> = R.ACCOUNT<AC.LOCAL.REF,Y.POS.QTY.WITH>
    END ELSE
        WITH.QTY<1,-1> = 'N/A'
    END
**********IT IS USED TO CHECK DEP WITH QTY*****
RETURN
*----------------------------------------------------------------------------------------------------
CHK.PR.USE:
*----------------------------------------------------------------------------------------------------
    LOCATE PROP.USE IN Y.LOOKUP.LIST12 SETTING POS12 THEN
        VAL.PROP.USE = ''
        VAL.PROP.USE = Y.LOOKUP.DESC12<POS12,VAR.USER.LANG>
        IF NOT(VAL.PROP.USE) THEN
            Y.PROP.USE<1,-1> = Y.LOOKUP.DESC12<POS12,1>
        END ELSE
            Y.PROP.USE<1,-1> = VAL.PROP.USE
        END
    END ELSE
        Y.PROP.USE<1,-1> = 'N/A'
    END
RETURN
*--------------------------------------------------------------------------------------------------------
CHK.FUND.ORIGIN:
*--------------------------------------------------------------------------------------------------------
    IF VAL.FUND.ORIGIN THEN
        FUND.ORIGIN<1,-1> = VAL.FUND.ORIGIN
    END ELSE
        FUND.ORIGIN<1,-1> = 'N/A'
    END
RETURN
*----------------------------------------------------------------------------------------------------
CATEG.DESC:
*-----------------------------------------------------------------------------------------------------
    CALL CACHE.READ(FN.CATEGORY, PROD.CODE, R.CATEG, CATEG.ERR)  ;*R22 Auto Conversion  - F.READ to CACHE.READ
    PROD.CODE.DESC = R.CATEG<EB.CAT.DESCRIPTION,VAR.USER.LANG>
    IF NOT(PROD.CODE.DESC) THEN
        PROD.CODE.DESC = R.CATEG<EB.CAT.DESCRIPTION,1>
    END
RETURN
*---------------------------------------------------------------------------------------------------
GET.VALUES:
*----------------------------------------------------------------------------------------------------
*This para is used to get the values from EB.LOOKUP table
    VIRTUAL.TAB.ID1='L.AC.CASH.DEP'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID1)
    Y.LOOKUP.LIST1=VIRTUAL.TAB.ID1<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID1,@FM)
    Y.LOOKUP.DESC1=VIRTUAL.TAB.ID1<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST1
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC1

    VIRTUAL.TAB.ID2='L.AC.CHQ.DEP'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID2)
    Y.LOOKUP.LIST2=VIRTUAL.TAB.ID2<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID2,@FM)
    Y.LOOKUP.DESC2=VIRTUAL.TAB.ID2<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST2
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC2

    VIRTUAL.TAB.ID3='L.AC.TRANS.DEP'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID3)
    Y.LOOKUP.LIST3=VIRTUAL.TAB.ID3<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID3,@FM)
    Y.LOOKUP.DESC3=VIRTUAL.TAB.ID1<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST3
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC3

    VIRTUAL.TAB.ID4='L.AC.CASHWITHDR'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID4)
    Y.LOOKUP.LIST4=VIRTUAL.TAB.ID4<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID4,@FM)
    Y.LOOKUP.DESC4=VIRTUAL.TAB.ID4<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST4
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC4

    VIRTUAL.TAB.ID5='L.AC.CHQWITHDR'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID5)
    Y.LOOKUP.LIST5=VIRTUAL.TAB.ID5<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID5,@FM)
    Y.LOOKUP.DESC5=VIRTUAL.TAB.ID5<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST5
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC5

    VIRTUAL.TAB.ID6='L.AC.TRANS.WDR'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID6)
    Y.LOOKUP.LIST6=VIRTUAL.TAB.ID6<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID6,@FM)
    Y.LOOKUP.DESC6=VIRTUAL.TAB.ID6<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST6
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC6

    VIRTUAL.TAB.ID7='GENDER'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID7)
    Y.LOOKUP.LIST7=VIRTUAL.TAB.ID7<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID7,@FM)
    Y.LOOKUP.DESC7=VIRTUAL.TAB.ID7<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST7
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC7

    VIRTUAL.TAB.ID8='MARITAL.STATUS'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID8)
    Y.LOOKUP.LIST8=VIRTUAL.TAB.ID8<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID8,@FM)
    Y.LOOKUP.DESC8=VIRTUAL.TAB.ID8<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST8
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC8

    VIRTUAL.TAB.ID9='EMPLOYMENT.STATUS'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID9)
    Y.LOOKUP.LIST9=VIRTUAL.TAB.ID9<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID9,@FM)
    Y.LOOKUP.DESC9=VIRTUAL.TAB.ID9<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST9
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC9

    VIRTUAL.TAB.ID10='RESIDENCE.TYPE'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID10)
    Y.LOOKUP.LIST10=VIRTUAL.TAB.ID10<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID10,@FM)
    Y.LOOKUP.DESC10=VIRTUAL.TAB.ID10<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST10
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC10

    VIRTUAL.TAB.ID11='L.AC.SRCINITDEP'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID11)
    Y.LOOKUP.LIST11=VIRTUAL.TAB.ID11<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID11,@FM)
    Y.LOOKUP.DESC11=VIRTUAL.TAB.ID11<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST11
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC11

    VIRTUAL.TAB.ID12='L.AC.PROP.USE'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID12)
    Y.LOOKUP.LIST12=VIRTUAL.TAB.ID12<2>
    DESC.CNT =DCOUNT(VIRTUAL.TAB.ID12,@FM)
    Y.LOOKUP.DESC12=VIRTUAL.TAB.ID12<DESC.CNT>
    CHANGE '_' TO @FM IN Y.LOOKUP.LIST12
    CHANGE '_' TO @FM IN Y.LOOKUP.DESC12
RETURN
END
