* @ValidationCode : MjotNjU1NDk1Mzk5OkNwMTI1MjoxNjgxMTE5MzY2ODM3OmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 10 Apr 2023 15:06:06
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.DRREG
*
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE DR.REG.RIEN8.EXTRACT(REC.ID)
*----------------------------------------------------------------------------
* Company Name   : APAP
* Company Name   : APAP
* Developed By   : gangadhar@temenos.com
* Program Name   : DR.REG.RIEN8.EXTRACT
* Date           : 3-May-2013
*----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the MM and SEC.TRADE in DOP and non DOP.
*----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date       Author              Modification Description

*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*10-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   FM to @FM
*10-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------





*----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_TSA.COMMON
    $INSERT I_F.CUSTOMER
    $INSERT I_F.MM.MONEY.MARKET
    $INSERT I_F.SEC.TRADE
    $INSERT I_F.SECURITY.MASTER
*    $INSERT I_F.EB.CONTRACT.BALANCES
*    $INSERT I_F.RE.STAT.LINE.CONT

    $INSERT I_DR.REG.RIEN8.EXTRACT.COMMON
    $INSERT I_F.REDO.AZACC.DESC
    $INSERT I_F.DR.REG.RIEN8.PARAM
*
    GOSUB PROCESS
*
RETURN
*----------------------------------------------------------------------------
PROCESS:
*------*
*
    BEGIN CASE
        CASE CONTROL.LIST<1,1> EQ "SEC.TRADE.LCY"
            CALL F.READ(FN.SEC.TRADE,REC.ID,R.SEC.TRADE,F.SEC.TRADE,SEC.TRADE.ERR)
            GOSUB PROCESS.SC
            CALL F.WRITE(FN.DR.REG.RIEN8.WORKFILE, REC.ID, RETURN.MSG)
        CASE CONTROL.LIST<1,1> EQ "SEC.TRADE.FCY"
            CALL F.READ(FN.SEC.TRADE,REC.ID,R.SEC.TRADE,F.SEC.TRADE,SEC.TRADE.ERR)
            GOSUB PROCESS.SC
            CALL F.WRITE(FN.DR.REG.RIEN8.WORKFILE.FCY,REC.ID,RETURN.MSG)
        CASE CONTROL.LIST<1,1> EQ "MM.LCY"
            CALL F.READ(FN.MM.MONEY.MARKET,REC.ID,R.MM.MONEY.MARKET,F.MM.MONEY.MARKET,MM.MONEY.MARKET.ERR)
            GOSUB PROCESS.MM
            CALL F.WRITE(FN.DR.REG.RIEN8.WORKFILE, REC.ID, RETURN.MSG)
        CASE CONTROL.LIST<1,1> EQ "MM.LCY"
            CALL F.READ(FN.MM.MONEY.MARKET,REC.ID,R.MM.MONEY.MARKET,F.MM.MONEY.MARKET,MM.MONEY.MARKET.ERR)
            GOSUB PROCESS.MM
            CALL F.WRITE(FN.DR.REG.RIEN8.WORKFILE.FCY,REC.ID,RETURN.MSG)
    END CASE
*
RETURN
*----------------------------------------------------------------------------
GET.CLIENT.TYPE:
****************
*
    CUS.NATION = R.CUSTOMER<EB.CUS.NATIONALITY>
    CUS.GENDER = R.CUSTOMER<EB.CUS.GENDER>
    CU.TIPO.CL = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.TIPO.CL.POS>
*    CUS.INDUST = R.CUSTOMER<EB.CUS.INDUSTRY>
    CUS.INDUST = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.APAP.INDUS.POS>
    CUS.RESIDE = R.CUSTOMER<EB.CUS.RESIDENCE>
    BEGIN CASE
        CASE CUS.NATION EQ 'DO' AND CUS.GENDER EQ 'MALE' AND CU.TIPO.CL EQ 'PERSONA FISICA'
            CLIENT.TYPE = 'P3'
        CASE CUS.NATION NE 'DO' AND CUS.RESIDE EQ 'DO' AND CU.TIPO.CL EQ 'PERSONA FISICA'
            CLIENT.TYPE = 'P4'
        CASE CUS.NATION EQ 'DO' AND CUS.GENDER EQ 'FEMALE' AND CU.TIPO.CL EQ 'PERSONA FISICA'
            CLIENT.TYPE = 'P5'
        CASE CUS.NATION NE 'DO' AND CUS.RESIDE EQ 'DO' AND CUS.GENDER EQ 'FEMALE' AND CU.TIPO.CL EQ 'PERSONA FISICA'
            CLIENT.TYPE = 'P6'
        CASE CUS.NATION NE 'DO' AND CUS.RESIDE NE 'DO' AND CUS.GENDER EQ 'MALE' AND CU.TIPO.CL EQ 'PERSONA FISICA'
            CLIENT.TYPE = 'P7'
        CASE CUS.NATION NE 'DO' AND CUS.RESIDE NE 'DO' AND CUS.GENDER EQ 'FEMALE' AND CU.TIPO.CL EQ 'PERSONA FISICA'
            CLIENT.TYPE = 'P8'
        CASE CUS.NATION EQ 'DO' AND CU.TIPO.CL EQ 'PERSONA JURIDICA'
            CLIENT.TYPE = 'E1'
        CASE CUS.NATION NE 'DO' AND CU.TIPO.CL EQ 'PERSONA JURIDICA' AND (CUS.INDUST LT 1065 AND CUS.INDUST GT 1069)
            CLIENT.TYPE = 'E2'
        CASE CUS.NATION NE 'DO' AND CU.TIPO.CL EQ 'PERSONA JURIDICA' AND (CUS.INDUST GE 1065 AND CUS.INDUST LE 1069)
            CLIENT.TYPE = 'E3'
    END CASE
*
RETURN
*----------------------------------------------------------------------------
PROCESS.SC:
***********
*
    RETURN.MSG = ''
*    RETURN.MSG<-1> = Y.TODAY[7,2]:"/":Y.TODAY[5,2]:"/":Y.TODAY[1,4]
    RETURN.MSG<-1> = Y.TODAY[7,2]:Y.TODAY[5,2]:Y.TODAY[1,4]   ;*PACS00312711
    RETURN.MSG<-1> = REC.ID
    SC.MAS.ID = R.SEC.TRADE<SC.SBS.SECURITY.CODE>
    CALL F.READ(FN.SECURITY.MASTER,SC.MAS.ID,R.SECURITY.MASTER,F.SECURITY.MASTER,SECURITY.MASTER.ERR)
    CUS.ID = R.SECURITY.MASTER<SC.SCM.ISSUER>
    CALL F.READ(FN.CUSTOMER,CUS.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
    RETURN.MSG<-1> = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.RNC.POS>
    RETURN.MSG<-1> = R.SECURITY.MASTER<SC.SCM.RATING>
    RETURN.MSG<-1> = R.CUSTOMER<EB.CUS.LOCAL.REF,L.AA.CAL.ISSUER.POS>
*
    GOSUB SC.FLD6.9
*    RETURN.MSG<-1> = MAT.DATE
    RETURN.MSG<-1> = MAT.DATE[7,2]:"/":MAT.DATE[5,2]:"/":MAT.DATE[1,4]  ;*PACS00312711
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    RETURN.MSG<-1> = R.SEC.TRADE<SC.SBS.CUST.NO.NOM>
    RETURN.MSG<-1> = R.SEC.TRADE<SC.SBS.LOCAL.REF,L.DIRTY.PERCENTAGE.POS>
    RETURN.MSG<-1> = R.SEC.TRADE<SC.SBS.LOCAL.REF,L.SC.TRN.YIELD.POS>
    RETURN.MSG<-1> = R.SEC.TRADE<SC.SBS.INTEREST.RATE>
    SUB.POS = ''
    LOCATE SUB.AS.TYPE IN R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.SC.SUB.TYPE> SETTING SUB.POS THEN
        SUB.REG.VAL = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.SC.REG.VAL,SUB.POS>
    END
    RETURN.MSG<-1> = SUB.REG.VAL
    RETURN.MSG<-1> = R.SEC.TRADE<SC.SBS.LOCAL.REF,L.CLEAN.PERCENTAGE.POS>
    RETURN.MSG<-1> = R.SECURITY.MASTER<SC.SCM.NO.OF.PAYMENTS>
    RETURN.MSG<-1> = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.REBUY.RESELL>
    RETURN.MSG<-1> = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.REBUY.RESELL>
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    GOSUB GET.LINE.CONT
    RETURN.MSG<-1> = LINE.CONT
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    GOSUB GET.CLIENT.TYPE
    RETURN.MSG<-1> = CLIENT.TYPE
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    CCY.POS = ''
    LOCATE R.SEC.TRADE<SC.SBS.SECURITY.CURRENCY> IN R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.CCY.TYPE> SETTING CCY.POS THEN
        CCY.REG = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.CCY.REG.VAL,CCY.POS>
    END
    RETURN.MSG<-1> = CCY.REG
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    IF R.SEC.TRADE<SC.SBS.INTEREST.DAYS> EQ 'A' THEN
        INT.DAY.BAS = '0'
    END ELSE
        INT.DAY.BAS = '1'
    END
    RETURN.MSG<-1> = INT.DAY.BAS
*    RETURN.MSG<-1> = R.SEC.TRADE<SC.SBS.ISSUE.DATE>
    SC.ISSUE.DATE = R.SEC.TRADE<SC.SBS.ISSUE.DATE>
    RETURN.MSG<-1> = SC.ISSUE.DATE[7,2]:"/":SC.ISSUE.DATE[5,2]:"/":SC.ISSUE.DATE[1,4]       ;*PACS00312711
    CHANGE @FM TO ',' IN RETURN.MSG
*
RETURN
*----------------------------------------------------------------------------
SC.FLD6.9:
**********
*
    SUB.AS.TYPE = R.SECURITY.MASTER<SC.SCM.SUB.ASSET.TYPE>
    TAB9.POS = ''
    LOCATE SUB.AS.TYPE IN R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.TAB9.T24> SETTING TAB9.POS THEN
        TAB9.REG.VAL = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.TAB9.REG,TAB9.POS>
    END
    RETURN.MSG<-1> = TAB9.REG.VAL
    RETURN.MSG<-1> = R.SECURITY.MASTER<SC.SCM.LOCAL.REF,L.ISIN.CODE.POS>
    SC.VALUE.DATE = R.SEC.TRADE<SC.SBS.VALUE.DATE>
    RETURN.MSG<-1> = SC.VALUE.DATE[7,2]:"/":SC.VALUE.DATE[5,2]:"/":SC.VALUE.DATE[1,4]       ;*PACS00312711
*    RETURN.MSG<-1> = R.SEC.TRADE<SC.SBS.VALUE.DATE>      ;*PACS00312711
    VAL.DATE = R.SEC.TRADE<SC.SBS.VALUE.DATE>
    MAT.DATE = R.SEC.TRADE<SC.SBS.MATURITY.DATE>
    Y.REGION = ''
    IF LEN(VAL.DATE) EQ 8 AND LEN(MAT.DATE) EQ 8 THEN
        Y.DAYS = 'C'
        CALL CDD(Y.REGION,MAT.DATE,VAL.DATE,Y.DAYS)
    END
    RETURN.MSG<-1> = ABS(Y.DAYS)

RETURN
*----------------------------------------------------------------------------
GET.LINE.CONT:
*************
*
    SC.ID.1 = SC.MAS.ID:'.':REC.ID
    CALL F.READ(FN.REDO.AZACC.DESC,SC.ID.1,R.REDO.AZACC.DESC,F.REDO.AZACC.DESC,REDO.AZACC.DESC.ERR)
    CR.VAL = 'LIVEDB' ;* For Principal
    IF R.REDO.AZACC.DESC THEN
        LOCATE CR.VAL IN R.REDO.AZACC.DESC<AZACC.ASSET.TYPE> SETTING DESC.POS THEN
            LINE.CONT = R.REDO.AZACC.DESC<AZACC.DESC,DESC.POS>
        END
    END
*    FIND CR.VAL IN R.REDO.AZACC.DESC SETTING FM.POS,VM.POS,SM.POS THEN
*        LINE.CONT = R.REDO.AZACC.DESC<AZACC.DESC,VM.POS>
*    END
*
RETURN
*----------------------------------------------------------------------------
GET.RE.STAT.LINE:
*****************
*
*    CALL F.READ(FN.REDO.AZACC.DESC,REC.ID,R.REDO.AZACC.DESC,F.REDO.AZACC.DESC,REDO.AZACC.DESC.ERR)
*    CR.VAL = 'CREDIT'         ;* For Principal
*    FIND CR.VAL IN R.REDO.AZACC.DESC SETTING FM.POS,VM.POS,SM.POS THEN
*        LINE.CONT = R.REDO.AZACC.DESC<AZACC.DESC,VM.POS>
*    END
*
    CALL F.READ(FN.REDO.AZACC.DESC,REC.ID,R.REDO.AZACC.DESC,F.REDO.AZACC.DESC,REDO.AZACC.DESC.ERR)
    IF R.REDO.AZACC.DESC THEN
        LOCATE 'LIVEDB' IN R.REDO.AZACC.DESC<AZACC.ASSET.TYPE> SETTING DESC.POS THEN
            LINE.CONT = R.REDO.AZACC.DESC<AZACC.DESC,DESC.POS>
        END
    END
*
RETURN
*-----------------------------------------------------------------------------
PROCESS.MM:
**********
*
    RETURN.MSG = ''
*    RETURN.MSG<-1> = Y.TODAY[7,2]:"/":Y.TODAY[5,2]:"/":Y.TODAY[1,4]      ;*PACS00312711
    RETURN.MSG<-1> = Y.TODAY[7,2]:Y.TODAY[5,2]:Y.TODAY[1,4]
    RETURN.MSG<-1> = REC.ID
    CUS.ID = R.MM.MONEY.MARKET<MM.CUSTOMER.ID>
    CALL F.READ(FN.CUSTOMER,CUS.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
    RETURN.MSG<-1> = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.RNC.POS>
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    RETURN.MSG<-1> = R.CUSTOMER<EB.CUS.LOCAL.REF,L.AA.CAL.ISSUER.POS>
    TAB9.POS = ''
    LOCATE R.MM.MONEY.MARKET<MM.CATEGORY> IN R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.TAB9.T24> SETTING TAB9.POS THEN
        TAB9.REG.VAL = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.TAB9.REG,TAB9.POS>
    END
    RETURN.MSG<-1> = TAB9.REG.VAL
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    VAL.DATE = R.MM.MONEY.MARKET<MM.VALUE.DATE>
*    RETURN.MSG<-1> = VAL.DATE
    RETURN.MSG<-1> = VAL.DATE[7,2]:"/":VAL.DATE[5,2]:"/":VAL.DATE[1,4]  ;*PACS00312711
    MAT.DATE = R.MM.MONEY.MARKET<MM.MATURITY.DATE>
    Y.REGION = ''
    IF LEN(VAL.DATE) EQ 8 AND LEN(MAT.DATE) EQ 8 THEN
        Y.DAYS = 'C'
        CALL CDD(Y.REGION,MAT.DATE,VAL.DATE,Y.DAYS)
    END
    RETURN.MSG<-1> = ABS(Y.DAYS)
*    RETURN.MSG<-1> = MAT.DATE
    RETURN.MSG<-1> = MAT.DATE[7,2]:"/":MAT.DATE[5,2]:"/":MAT.DATE[1,4]  ;*PACS00312711
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    RETURN.MSG<-1> = R.MM.MONEY.MARKET<MM.PRINCIPAL>
    RETURN.MSG<-1> = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.MM.BUY.PRICE>
    RETURN.MSG<-1> = R.MM.MONEY.MARKET<MM.INTEREST.RATE>
    RETURN.MSG<-1> = R.MM.MONEY.MARKET<MM.INTEREST.RATE>
    SUB.POS = ''
    LOCATE R.MM.MONEY.MARKET<MM.CATEGORY> IN R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.SC.SUB.TYPE> SETTING SUB.POS THEN
        SUB.REG.VAL = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.SC.REG.VAL,SUB.POS>
    END
    RETURN.MSG<-1> = SUB.REG.VAL
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    GOSUB GET.INT.PER
    RETURN.MSG<-1> = INT.PER
    RETURN.MSG<-1> = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.REBUY.RESELL>
    RETURN.MSG<-1> = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.REBUY.RESELL>
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    GOSUB GET.RE.STAT.LINE
    RETURN.MSG<-1> =  LINE.CONT
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    GOSUB GET.CLIENT.TYPE
    RETURN.MSG<-1> = CLIENT.TYPE
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    CCY.POS = ''
    LOCATE R.MM.MONEY.MARKET<MM.CURRENCY> IN R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.CCY.TYPE> SETTING CCY.POS THEN
        CCY.REG = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.CCY.REG.VAL,CCY.POS>
    END
    RETURN.MSG<-1> = CCY.REG
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    RETURN.MSG<-1> = ''         ;* Blank as per APAP
    IF R.MM.MONEY.MARKET<MM.INTEREST.BASIS> EQ 'A' THEN
        INT.DAY.BAS = '0'
    END ELSE
        INT.DAY.BAS = '1'
    END
    RETURN.MSG<-1> = INT.DAY.BAS
*    RETURN.MSG<-1> = VAL.DATE
    RETURN.MSG<-1> = VAL.DATE[7,2]:"/":VAL.DATE[5,2]:"/":VAL.DATE[1,4]  ;*PACS00312711
    CHANGE @FM TO ',' IN RETURN.MSG
*
RETURN
*----------------------------------------------------------------------------
GET.INT.PER:
***********
*
    INT.SCH = R.MM.MONEY.MARKET<MM.INT.SCHEDULE>
    IF INT.SCH THEN
        BEGIN CASE
            CASE INT.SCH[9,5] EQ 'WEEK2'
                INT.PER = 'Q'
            CASE INT.SCH[9,3] EQ 'M01'
                INT.PER = 'M'
            CASE INT.SCH[9,3] EQ 'M02'
                INT.PER = 'B'
            CASE INT.SCH[9,3] EQ 'M03'
                INT.PER = 'T'
            CASE INT.SCH[9,3] EQ 'M04'
                INT.PER = 'Q'
            CASE INT.SCH[9,3] EQ 'M06'
                INT.PER = 'S'
            CASE INT.SCH[9,3] EQ 'M12'
                INT.PER = 'M'
        END CASE

    END ELSE
        INT.PER = 'V'
    END
*
RETURN
*----------------------------------------------------------------------------

END
