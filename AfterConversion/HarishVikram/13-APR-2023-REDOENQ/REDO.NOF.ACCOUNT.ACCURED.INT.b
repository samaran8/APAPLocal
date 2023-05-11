* @ValidationCode : Mjo5MDUyNDg2NjM6Q3AxMjUyOjE2ODEzNzg5NDkzNzE6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 13 Apr 2023 15:12:29
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOF.ACCOUNT.ACCURED.INT(Y.FINAL.ARRAY)
*-------------------------------------------------------------------------
*Company Name      : APAP Bank
*Developed By      : Temenos Application Management
*Program Name      : REDO.NOF.ACCOUNT.ACCURED.INT
*Date              : 11.11.2010
*-------------------------------------------------------------------------
* Incoming/Outgoing Parameters
*-------------------------------
* In  : --N/A--
* Out : Y.FINAL.ARRAY
*-----------------------------------------------------------------------------
* Revision History:
* -----------------
* Date                   Name                   Reference               Version
* -------                ----                   ----------              --------
*11/11/2010      saktharrasool@temenos.com   ODR-2010-03-0180       Initial Version
*07/07/2014      Egambaram A                 PACS00313066           In line number 69 & 332 changes are done
*27/04/2015      V.P.Ashokkumar              PACS00313066           Fixed the region and column values.

* 13-APR-2023     Conversion tool   R22 Auto conversion   FM TO @FM, VM to @VM
* 13-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.COMPANY
    $INSERT I_F.CATEGORY
    $INSERT I_F.ACCR.ACCT.CR
    $INSERT I_F.ACCOUNT.CREDIT.INT
    $INSERT I_F.GROUP.CREDIT.INT
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.GROUP.DATE
    $INSERT I_F.ACCOUNT.CLASS
*-------------------------------------------------
*

    GOSUB INIT.VARIABLES
    GOSUB OPEN.FILES
    GOSUB ACC.PROCESS
    GOSUB AC.CR.PROCESS
    GOSUB AC.INACT.PROCESS
    GOSUB GP.CRD.INT.PROCESS
    GOSUB CUS.PROCESS
    GOSUB CTG.PROCESS
    GOSUB ACCR.ACCT.CR.PROCESS
    GOSUB FINAL.ARRAY
*
RETURN
*---------------
INIT.VARIABLES:
*---------------
* Initialising all the required variables

    Y.CLOSE.MONTH='' ; Y.AGENCY=''; Y.REGION='';Y.ACCOUNT.TYPE='';Y.ACCT.NO='';Y.CCY='';Y.INT.RATE=0;Y.BAL.BASIS=''
    Y.ACT.ACCT=0 ; Y.INACT.ACCT=0; Y.ACC.INT.PAY=0;Y.CAN.MONTH.INT='';Y.TOT.INT.PAY=0;Y.CLASSIFICATION=''

*LOC AL REFERENCE FIELD

    LREF.APP ='ACCOUNT'
    LREF.FIELDS = 'L.AC.STATUS1'
    LOCAL.REF.POS=''
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LOCAL.REF.POS)
    L.AC.STATUS1.POS = LOCAL.REF.POS<1,1>

*COMPANY NAME

*PACS00313066  - S
*Y.COM.NAME=R.COMPANY(EB.COM.COMPANY.NAME)
    Y.COM.NAME  = "Asociacisn Popular de Ahorros y Pristamos"
*PACS00313066 - E
RETURN

*---------------
OPEN.FILES:
*---------------
*Opening the required Application for Nofile enquiry

    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER='F.CUSTOMER'
    F.CUSTOMER=''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.CATEGORY='F.CATEGORY'
    F.CATEGORY=''
    CALL OPF(FN.CATEGORY,F.CATEGORY)

    FN.ACCR.ACCT.CR='F.ACCR.ACCT.CR'
    F.ACCR.ACCT.CR=''
    CALL OPF(FN.ACCR.ACCT.CR,F.ACCR.ACCT.CR)

    FN.ACCOUNT.CREDIT.INT='F.ACCOUNT.CREDIT.INT'
    F.ACCOUNT.CREDIT.INT=''
    CALL OPF(FN.ACCOUNT.CREDIT.INT,F.ACCOUNT.CREDIT.INT)

    FN.GROUP.CREDIT.INT='F.GROUP.CREDIT.INT'
    F.GROUP.CREDIT.INT=''
    CALL OPF(FN.GROUP.CREDIT.INT,F.GROUP.CREDIT.INT)

    FN.GROUP.DATE='F.GROUP.DATE'
    F.GROUP.DATE=''
    CALL OPF(FN.GROUP.DATE,F.GROUP.DATE)

    FN.ACCOUNT.CLASS = 'F.ACCOUNT.CLASS'
    F.ACCOUNT.CLASS = ''
    CALL OPF(FN.ACCOUNT.CLASS, F.ACCOUNT.CLASS)

    Y.AC.CLASS.ID = 'SAVINGS'
    R.AC.CLASS = ''
    CALL CACHE.READ(FN.ACCOUNT.CLASS, Y.AC.CLASS.ID, R.AC.CLASS, Y.READ.ERR)
    Y.SAV.CAT.LIST = R.AC.CLASS<AC.CLS.CATEGORY>
RETURN

*-------------
ACC.PROCESS:
*-------------
*Reading the Account Application
    R.ACC = '' ; ACC.ERR = '' ;

    TDATE=TODAY
    TTDATE=TDATE[5,2]

    IF ID.NEW EQ 'REDO.ENQ.ACCOUNT.ACCURED.INT' OR 'REDO.ACCT.ACCURED.INT' THEN

        SEL.CMD = "SELECT ":FN.ACCOUNT

        LOCATE "CURRENCY" IN D.FIELDS<1> SETTING Y.AC.CCY.POS THEN
            Y.CCY.TYPE.OPERAND       = D.LOGICAL.OPERANDS<Y.AC.CCY.POS>
            Y.CCY.TYPE               = D.RANGE.AND.VALUE<Y.AC.CCY.POS>
            SEL.CMD := " WITH CURRENCY EQ ":Y.CCY.TYPE
        END

        LOCATE "AGENCY" IN D.FIELDS<1> SETTING Y.AC.AG.POS THEN
            Y.AG.TYPE.OPERAND       = D.LOGICAL.OPERANDS<Y.AC.AG.POS>
            Y.AG.TYPE               = D.RANGE.AND.VALUE<Y.AC.AG.POS>
            IF Y.CCY.TYPE THEN
                SEL.CMD := " AND CO.CODE EQ ":Y.AG.TYPE
            END ELSE
                SEL.CMD := " WITH CO.CODE EQ ":Y.AG.TYPE
            END
        END


        LOCATE "ACCOUNT.TYPE" IN D.FIELDS<1> SETTING Y.AC.TY.POS THEN
            Y.AC.TYPE.OPERAND       = D.LOGICAL.OPERANDS<Y.AC.TY.POS>
            Y.AC.TYPE               = D.RANGE.AND.VALUE<Y.AC.TY.POS>
            IF NOT(Y.CCY.TYPE) AND NOT(Y.AG.TYPE) THEN
                SEL.CMD :=" WITH CATEGORY EQ ":Y.AC.TYPE
            END ELSE
                SEL.CMD :=" AND CATEGORY EQ ":Y.AC.TYPE
            END
        END ELSE
            Y.SAV.CAT.LIST = CHANGE(Y.SAV.CAT.LIST, @VM, ' ')
            SEL.CMD := ' AND WITH ( CATEGORY EQ ':Y.SAV.CAT.LIST:' )'
        END
    END ELSE
        SEL.CMD = "SELECT ":FN.ACCOUNT
        LOCATE 'CLOSING.MONTH' IN D.FIELDS<1> SETTING CLO.POS THEN
            Y.CLOSE.MONTH =  D.RANGE.AND.VALUE<CLO.POS>
            SEL.CMD = "SELECT ":FN.ACCOUNT
        END

        LOCATE "CURRENCY" IN D.FIELDS<1> SETTING Y.AC.CCY.POS THEN
            Y.CCY.TYPE.OPERAND       = D.LOGICAL.OPERANDS<Y.AC.CCY.POS>
            Y.CCY.TYPE               = D.RANGE.AND.VALUE<Y.AC.CCY.POS>
            SEL.CMD := " WITH CURRENCY EQ ":Y.CCY.TYPE
        END

        LOCATE "AGENCY" IN D.FIELDS<1> SETTING Y.AC.AG.POS THEN
            Y.AG.TYPE.OPERAND       = D.LOGICAL.OPERANDS<Y.AC.AG.POS>
            Y.AG.TYPE               = D.RANGE.AND.VALUE<Y.AC.AG.POS>
            IF Y.CCY.TYPE THEN
                SEL.CMD := " AND CO.CODE EQ ":Y.AG.TYPE
            END ELSE
                SEL.CMD := " WITH CO.CODE EQ ":Y.AG.TYPE
            END
        END

        LOCATE "ACCOUNT.TYPE" IN D.FIELDS<1> SETTING Y.AC.TY.POS THEN
            Y.AC.TYPE.OPERAND       = D.LOGICAL.OPERANDS<Y.AC.TY.POS>
            Y.AC.TYPE               = D.RANGE.AND.VALUE<Y.AC.TY.POS>

            IF NOT(Y.CCY.TYPE) AND NOT(Y.AG.TYPE) THEN
                SEL.CMD :=" WITH CATEGORY EQ ":Y.AC.TYPE
            END ELSE
                SEL.CMD :=" AND CATEGORY EQ ":Y.AC.TYPE
            END
        END ELSE
            Y.SAV.CAT.LIST = CHANGE(Y.SAV.CAT.LIST, @VM, ' ')
            IF NOT(Y.CCY.TYPE) AND NOT(Y.AG.TYPE) THEN
                SEL.CMD := ' AND WITH ( CATEGORY EQ Y.SAV.CAT.LIST )'
            END ELSE
                SEL.CMD := ' AND ( CATEGORY EQ Y.SAV.CAT.LIST )'
            END
        END
    END
    GOSUB ACC.PROCESS.SUB
RETURN

*----------------
ACC.PROCESS.SUB:
*----------------
*For Classification
    TEMP.RANGE.AND.VALUE = D.RANGE.AND.VALUE
    CHANGE @FM TO ',' IN TEMP.RANGE.AND.VALUE
    Y.CLASSIFICATION = TEMP.RANGE.AND.VALUE

    CALL EB.READLIST(SEL.CMD,SEL.LIST,'', NO.RECS,RET.CODE)

    LOOP
        REMOVE RMV.AC.ID FROM SEL.LIST SETTING RMV.ACPOS
    WHILE RMV.AC.ID:RMV.ACPOS
        CALL F.READ(FN.ACCOUNT,RMV.AC.ID,R.ACC,F.ACCOUNT,ACC.ERR)
        Y.ACCT.NO=RMV.AC.ID
        Y.AGENCY=R.ACC<AC.CO.CODE>
        CUS.ID=R.ACC<AC.CUSTOMER>
        Y.CATEG=R.ACC<AC.CATEGORY>
        Y.CCY=R.ACC<AC.CURRENCY>
        Y.CDN.GRP=R.ACC<AC.CONDITION.GROUP>
        Y.CR.AMT=R.ACC<AC.ACCR.CR.AMOUNT>
        Y.AC.INACT=R.ACC<AC.LOCAL.REF,L.AC.STATUS1.POS>
        Y.AC.CREDIT.INT = ""
        Y.AC.CREDIT.INT=R.ACC<AC.ACCT.CREDIT.INT>

        GOSUB CUS.PROCESS
        GOSUB CTG.PROCESS

        IF Y.AC.CREDIT.INT THEN
            GOSUB AC.CR.PROCESS
        END ELSE
            GOSUB GP.CRD.INT.PROCESS
        END

        IF Y.INT.RATE THEN
            GOSUB AC.INACT.PROCESS
            GOSUB ACCR.ACCT.CR.PROCESS
            GOSUB FINAL.ARRAY
        END

    REPEAT
RETURN

*--------------
CUS.PROCESS:
*--------------
*Reading the customer Application Using CUS.ID

    R.CUST = ''  ; CUS.ERR = '' ;
    CALL F.READ(FN.CUSTOMER,CUS.ID,R.CUST,F.CUSTOMER,CUS.ERR)
    IF R.CUST THEN
        Y.ACC.OFF=R.CUST<EB.CUS.ACCOUNT.OFFICER>
    END
    Y.REGI = Y.ACC.OFF
    IF Y.ACC.OFF THEN
        Y.CNT = LEN(Y.ACC.OFF)
        IF Y.CNT GT 8 THEN
            Y.CNT1 = Y.CNT-8
            Y.CNT2 = Y.CNT1+1
            Y.REGION = Y.ACC.OFF[Y.CNT2,2]
        END
        IF Y.CNT LE 8 THEN
            Y.REGION = Y.ACC.OFF[1,2]
        END
    END
RETURN

*-------------
CTG.PROCESS:
*-------------
*Reading Category Application Using Y.CATEG
    R.CTG = '' ; CTG.ERR = '' ;
    CALL CACHE.READ(FN.CATEGORY, Y.CATEG, R.CTG, CTG.ERR) ;*R22 Auto conversion
    Y.ACCOUNT.TYPE=R.CTG<EB.CAT.DESCRIPTION>
    Y.ACCOUNT.TYPE = CHANGE(Y.ACCOUNT.TYPE, '*','')
RETURN

*--------------
AC.CR.PROCESS:
*--------------
*Reading the Account Credit Int Application using Y.ACCT.NO:Y.AC.CREDIT.INT

    Y.INT.RATE = 0 ; Y.BAL.BASIS = '' ;R.ACCOUNT.CREDIT.INT='';AC.CR.ERR=''
    LOOP
        REMOVE AC.CR.ID FROM Y.AC.CREDIT.INT SETTING AC.CR.POS
    WHILE AC.CR.ID:AC.CR.POS
        Y.AC.CR.ID=Y.ACCT.NO:'-':AC.CR.ID
        CALL F.READ(FN.ACCOUNT.CREDIT.INT,Y.AC.CR.ID,R.ACCOUNT.CREDIT.INT,F.ACCOUNT.CREDIT.INT,AC.CR.ERR)
        IF R.ACCOUNT.CREDIT.INT THEN
            IF Y.INT.RATE  THEN
                Y.INT.RATE<1,-1>=R.ACCOUNT.CREDIT.INT<IC.GCI.CR.INT.RATE>
            END ELSE
                Y.INT.RATE=R.ACCOUNT.CREDIT.INT<IC.GCI.CR.INT.RATE>
            END
            IF Y.BAL.BASIS THEN
                Y.BAL.BASIS<1,-1>=R.ACCOUNT.CREDIT.INT<IC.GCI.CR.BALANCE.TYPE>
            END ELSE
                Y.BAL.BASIS=R.ACCOUNT.CREDIT.INT<IC.GCI.CR.BALANCE.TYPE>
            END
        END
    REPEAT
RETURN

*--------------------
GP.CRD.INT.PROCESS:
*--------------------
*Reading the Group Credit Int Application  Using GP.CR.ID

    R.GROUP.CREDIT.INT = '' ; GP.CR.ERR = '';R.GROUP.DATE='';Y.DATE.ERR=''
    Y.CDN.GROUP=Y.CDN.GRP:Y.CCY
    CALL CACHE.READ(FN.GROUP.DATE, Y.CDN.GROUP, R.GROUP.DATE, Y.DATE.ERR) ;*R22 Auto conversion
    Y.CRD.DATE=R.GROUP.DATE<AC.GRD.CREDIT.DATES>

    Y.INT.RATE = 0 ; Y.BAL.BASIS = '' ;
    LOOP
        REMOVE GP.CR.ID FROM Y.CRD.DATE SETTING GP.CR.POS
    WHILE GP.CR.ID:GP.CR.POS
        Y.GP.CR.ID=Y.CDN.GROUP:GP.CR.ID
        CALL CACHE.READ(FN.GROUP.CREDIT.INT, Y.GP.CR.ID, R.GROUP.CREDIT.INT, GP.CR.ERR) ;*R22 Auto conversion
        IF Y.INT.RATE  THEN
            Y.INT.RATE<1,-1>=R.GROUP.CREDIT.INT<IC.GCI.CR.INT.RATE>
        END ELSE
            Y.INT.RATE=R.GROUP.CREDIT.INT<IC.GCI.CR.INT.RATE>
        END
        IF Y.BAL.BASIS THEN
            Y.BAL.BASIS<1,-1>=R.GROUP.CREDIT.INT<IC.GCI.CR.BALANCE.TYPE>
        END ELSE
            Y.BAL.BASIS=R.GROUP.CREDIT.INT<IC.GCI.CR.BALANCE.TYPE>
        END
    REPEAT
RETURN

*-----------------
AC.INACT.PROCESS:
*-----------------
*Validating  Inactive Marker in Account Application

    IF Y.AC.INACT EQ 'ACTIVE' OR Y.AC.INACT EQ '' THEN
        Y.ACT.ACCT=Y.CR.AMT
        Y.INACT.ACCT=0
    END ELSE
        Y.INACT.ACCT=Y.CR.AMT
        Y.ACT.ACCT=0
    END

    IF Y.CR.AMT EQ '' THEN
        Y.INACT.ACCT=0
        Y.ACT.ACCT=0
    END
RETURN

*----------------------
ACCR.ACCT.CR.PROCESS:
*----------------------
*Reading the Accr Acct Cr Application Using Y.ACCT.NO
    R.ACCR.ACCT.CR = '' ; ACCT.CR.ERR = '' ; Y.CAN.MONTH.INT='' ;Y.ACC.INT.PAY=0 ;Y.TOT.INT.PAY=0
    CALL F.READ(FN.ACCR.ACCT.CR,Y.ACCT.NO,R.ACCR.ACCT.CR,F.ACCR.ACCT.CR,ACCT.CR.ERR)
    IF R.ACCR.ACCT.CR THEN
        Y.ACC.INT.PAY=R.ACCR.ACCT.CR<IC.ACRCR.CR.INT.AMT>
        Y.TOT.INT.PAY=R.ACCR.ACCT.CR<IC.ACRCR.TOTAL.INTEREST>
        Y.INT.RATE = R.ACCR.ACCT.CR<IC.ACRCR.CR.INT.RATE>
        IF Y.CAN.MONTH.INT THEN
            Y.CAN.MONTH.INT<1,-1>=R.ACCR.ACCT.CR<IC.ACRCR.CR.INT.DATE>
        END ELSE
            Y.CAN.MONTH.INT=R.ACCR.ACCT.CR<IC.ACRCR.CR.INT.DATE>
        END
    END
RETURN

*-------------
FINAL.ARRAY:
*-------------
*Final Output In For the Nofile Enquiry

    Y.BAL.BASIS = Y.BAL.BASIS<1,1>

    BEGIN CASE
        CASE Y.BAL.BASIS EQ "AVERAGE"
            Y.BAL.BASIS = "PROMEDIO"
        CASE Y.BAL.BASIS EQ "DAILY"
            Y.BAL.BASIS = "DIARIO"
        CASE Y.BAL.BASIS EQ "MINIMUM"
            Y.BAL.BASIS = "MINIMO"
    END CASE

    LOCATE Y.ACCT.NO IN Y.PROCESSED.AC SETTING Y.ACCT.NO.POS ELSE
        IF Y.INT.RATE AND (Y.ACT.ACCT OR Y.INACT.ACCT OR Y.ACC.INT.PAY OR Y.CAN.MONTH.INT OR Y.TOT.INT.PAY) THEN
            Y.FINAL.ARRAY<-1> =Y.CLOSE.MONTH:"*":Y.AGENCY:"*":Y.REGION:"*":Y.ACCOUNT.TYPE:"*":Y.ACCT.NO:"*":Y.CCY:"*":Y.INT.RATE:"*":Y.BAL.BASIS:"*":Y.ACT.ACCT:"*":Y.INACT.ACCT:"*":Y.ACC.INT.PAY:"*":Y.CAN.MONTH.INT:"*":Y.TOT.INT.PAY:"*":Y.CLASSIFICATION:"*":Y.COM.NAME
            Y.PROCESSED.AC<-1> = Y.ACCT.NO
        END
    END
*                                1                   2            3               4                 5          6          7               8                9               10             11                12                 13               14
RETURN

END
