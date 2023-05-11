* @ValidationCode : MjoxODAzMTk3Mjc1OkNwMTI1MjoxNjgxODA0MzIyNTMxOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 18 Apr 2023 13:22:02
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
SUBROUTINE REDO.E.NOF.ACCT.DEPOSIT.DETAILS(Y.RETURN.DATA)
*------------------------------------------------------------------------------------------------------------------------
* Description:
**************
* This is a nofile routine attached to the enquiry REDO.E.ACCT.DEPOSIT.DETAILS which is used to display the
* the transaction details in which the account deposit amount is greater than or equal to US$10,000.00
* or equivalent in local currency
*--------------------------------------------------------------------------------------------------------------------------
* Modification History:
***********************
* P.Saranya - New - 15-Nov-2010
*
* Shekar - Performance 24/Feb/2012
* - do not select internal account for batch process
* - do not rebuild/initialise Y.ACCOUNT.NAME for every stmt entry
* - change F.READ to READ for online performance
* - cache read RELATION
* PACS00345951 -Ega - changes done & READ logic has been changed
*
* 18-APR-2023     Conversion tool   R22 Auto conversion   FM TO @FM, VM to @VM, SM to @SM
* 18-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*--------------------------------------------------------------------------------------------------------------------------
* Common Variables Used:
*************************
* D.FIELDS - Contains the field names given in the selection fields of the enquiry
* D.RANGE.AND.VALUE - Contains the value of the Selection Fields
* RUNNING.UNDER.BATCH - Gives whether the enquiry is running in online or COB
*
*-----------------------------------------------------------------------------------------------------------------------------------
* Incoming/Outgoing Parameters
*------------------------------
* In : --N/A-
* Out : Y.RETURN.DATA
*------------------------------------------------------------------------------------------------------------------------------
* Insert Files
***************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.RELATION
    $INSERT I_F.CATEGORY
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.REDO.AML.PARAM
*

    GOSUB INIT
    IF RUNNING.UNDER.BATCH THEN
        GOSUB BATCH.PROCESS
        Y.RETURN.DATA = SORT(Y.RETURN)
    END ELSE
        GOSUB SELECTION.CRITERIA.PROCESS
        GOSUB SELECT.PROCESS
    END
*
RETURN
*----------------------------------------------------------------------------------------------------------------------------
INIT:
*****
* Variable Initialisation
*
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.AML.PARAM = 'F.REDO.AML.PARAM'
    F.REDO.AML.PARAM = ''
    R.REDO.AML.PARAM = ''
    CALL CACHE.READ(FN.REDO.AML.PARAM,'SYSTEM',R.REDO.AML.PARAM,ERR.RAP)

    FN.ACCT.ENT.LWORK.DAY = 'F.ACCT.ENT.LWORK.DAY'
    F.ACCT.ENT.LWORK.DAY = ''
    CALL OPF(FN.ACCT.ENT.LWORK.DAY,F.ACCT.ENT.LWORK.DAY)

*
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
*
    FN.STMT.ENTRY = 'F.STMT.ENTRY'
    F.STMT.ENTRY = ''
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)
*
    FN.RELATION = 'F.RELATION'
    F.RELATION = ''
    CALL OPF(FN.RELATION,F.RELATION)
*
    FN.CATEGORY = "F.CATEGORY"
    F.CATEGORY = ""
    CALL OPF(FN.CATEGORY,F.CATEGORY)
*
    Y.AGENCY = ''
    Y.DATE = ''
    Y.TRANS.REF = ''
    Y.INPUTTER = ''
    Y.AUTHORISER = ''
    Y.AC.CATEGORY = ''
    Y.AC.CURRENCY = ''
    Y.CUSTOMER = ''
    Y.ACCOUNT.OFFICER = ''
    Y.TRANS.AMOUNT = ''
    Y.ACCOUNT.NAME = ''
    Y.SELECTION.VALUE = ''
    Y.RETURN = ''
    Y.RETURN.DATA = ''
*
    Y.ACCOUNT.TYPE = ''
    Y.CURRENCY = ''
    Y.EX.AGENCY = ''
    Y.TRANSACTION.DATE = ''
*
* To Get the local currency value equivalent to US$10,000.00
*
* Y.AMT = '10000'
* IF LCCY NE 'USD' THEN
* Y.FCY = 'USD'
* CALL EB.CURR.CONV(Y.FCY,Y.AMT,LCCY,Y.CONV.AMT)
* Y.AMT = Y.CONV.AMT
* END
* We can refer to the field AMT.LIMIT.LCY in REDO.AML.PARAM
*
    Y.AMT = R.REDO.AML.PARAM<AML.PARAM.AMT.LIMIT.LCY>
*
* To get the Current time

    Y.TIME = TIME()
    Y.CONV.TIME = OCONV(Y.TIME,"MTS")
*
    LOC.L.CU.TIPO.CL.POS = ''
    CALL GET.LOC.REF('CUSTOMER','L.CU.TIPO.CL',LOC.L.CU.TIPO.CL.POS)
*
RETURN
*----------------------------------------------------------------------------------------------------------------------------
SELECTION.CRITERIA.PROCESS:
***************************
* To get the Values in the selection fields in te enquiry
*
    LOCATE "ACCOUNT.TYPE" IN D.FIELDS<1> SETTING POS1 THEN
        Y.ACCOUNT.TYPE = D.RANGE.AND.VALUE<POS1>
        Y.SELECTION.VALUE = Y.ACCOUNT.TYPE:','
    END
*
    LOCATE "CURRENCY" IN D.FIELDS<1> SETTING POS2 THEN
        Y.CURRENCY = D.RANGE.AND.VALUE<POS2>
        Y.SELECTION.VALUE := Y.CURRENCY:','
    END
*
    LOCATE "EXECUTING.AGENCY" IN D.FIELDS<1> SETTING POS3 THEN
        Y.EX.AGENCY = D.RANGE.AND.VALUE<POS3>
        Y.SELECTION.VALUE := Y.EX.AGENCY:','
    END
*
    LOCATE "TRANSACTION.DATE" IN D.FIELDS<1> SETTING POS4 THEN
        Y.TRANSACTION.DATE = D.RANGE.AND.VALUE<POS4>
*CONVERT SM TO ' ' IN Y.TRANSACTION.DATE
        CHANGE @SM TO ' ' IN Y.TRANSACTION.DATE
        Y.SELECTION.VALUE := Y.TRANSACTION.DATE
    END
*
RETURN
*----------------------------------------------------------------------------------------------------------------------------
SELECT.PROCESS:
***************
* Select ACCOUNT file for the given selection criteria
*
* PACS00345951 - EGA - S
*
*BEGIN CASE
*CASE Y.ACCOUNT.TYPE AND Y.CURRENCY
*SEL.CMD = "SELECT ":FN.ACCOUNT:" WITH @ID LIKE '0N'":" AND CATEGORY EQ ":Y.ACCOUNT.TYPE:" AND CURRENCY EQ ":Y.CURRENCY:" BY ACCOUNT.OFFICER": " BY CATEGORY":" BY CURRENCY"
*CASE Y.ACCOUNT.TYPE
*SEL.CMD = "SELECT ":FN.ACCOUNT:" WITH @ID LIKE '0N'":" AND CATEGORY EQ ":Y.ACCOUNT.TYPE:" BY ACCOUNT.OFFICER":" BY CATEGORY":" BY CURRENCY"
*CASE Y.CURRENCY
*SEL.CMD = "SELECT ":FN.ACCOUNT:" WITH @ID LIKE '0N'":" AND CURRENCY EQ ":Y.CURRENCY:" BY ACCOUNT.OFFICER":" BY CATEGORY":" BY CURRENCY"
*CASE 1
*SEL.CMD = "SELECT ":FN.ACCOUNT:" WITH @ID LIKE '0N'":" BY ACCOUNT.OFFICER":" BY CATEGORY":" BY CURRENCY"
*END CASE
*
    SEL.CMD = "SELECT ":FN.ACCOUNT:" WITH @ID LIKE '0N'":" BY ACCOUNT.OFFICER":" BY CATEGORY":" BY CURRENCY"
    SEL.LIST = ''
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.CODE)
    LOOP
        REMOVE Y.ACCOUNT.NUMBER FROM SEL.LIST SETTING AC.POS
    WHILE Y.ACCOUNT.NUMBER : AC.POS
        R.ACCOUNT = '' ; ERR.ACCOUNT = ''
*READ R.ACCOUNT FROM F.ACCOUNT,Y.ACCOUNT.NUMBER ELSE R.ACCOUNT = ''
*CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
*Read Converted by TUS-Convert
* READ R.ACCOUNT FROM F.ACCOUNT,Y.ACCOUNT.NUMBER THEN ;*Tus Start
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,R.ACCOUNT.ERR)
        IF R.ACCOUNT THEN ;* Tus End
        END ELSE
            R.ACCOUNT = ''
        END
        Y.ACCOUNT.NAME = ''
        GOSUB ONLINE.PROCESS
    REPEAT
*
    Y.RETURN.DATA = SORT(Y.RETURN)
    Y.RETURN = ""
*
RETURN
*----------------------------------------------------------------------------------------------------------------------------
BATCH.PROCESS:
**************
* To Generate the report during COB
*
    SEL.CMD = "SELECT ": FN.ACCT.ENT.LWORK.DAY :' WITH @ID LIKE "0N"'
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.CODE)
    LOOP
        REMOVE Y.ACCOUNT.NUMBER FROM SEL.LIST SETTING AC.POS
    WHILE Y.ACCOUNT.NUMBER : AC.POS
        R.ACCOUNT = '' ; ERR.ACCOUNT = ''
*READ R.ACCOUNT FROM F.ACCOUNT, Y.ACCOUNT.NUMBER ELSE R.ACCOUNT = ''
*CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
*Read Converted by TUS-Convert
* READ R.ACCOUNT FROM F.ACCOUNT,Y.ACCOUNT.NUMBER THEN ;*Tus Start
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,R.ACCOUNT.ERR)
        IF R.ACCOUNT THEN ;* Tus End
        END ELSE
            R.ACCOUNT = ''
        END
        Y.ACCOUNT.NAME = ''
*READ YID.LIST FROM F.ACCT.ENT.LWORK.DAY, Y.ACCOUNT.NUMBER ELSE YID.LIST = ''
*CALL F.READ(FN.ACCT.ENT.LWORK.DAY,Y.ACCOUNT.NUMBER,YID.LIST,F.ACCT.ENT.LWORK.DAY,ERR.ACCT.ENT.LWORK.DAY)
*Read Converted by TUS-Convert
* READ YID.LIST FROM F.ACCT.ENT.LWORK.DAY,Y.ACCOUNT.NUMBER THEN ;*Tus Start
        CALL F.READ(FN.ACCT.ENT.LWORK.DAY,Y.ACCOUNT.NUMBER,YID.LIST,F.ACCT.ENT.LWORK.DAY,YID.LIST.ERR)
        IF YID.LIST THEN ;* Tus End
        END ELSE
            YID.LIST = ''
        END
        LOOP
            REMOVE Y.STMT.ID FROM YID.LIST SETTING STMT.POS
        WHILE Y.STMT.ID : STMT.POS
            R.STMT.ENTRY = '' ; ERR.STMT.ENTRY = ''
*READ R.STMT.ENTRY FROM F.STMT.ENTRY, Y.STMT.ID ELSE R.STMT.ENTRY = ''
*CALL F.READ(FN.STMT.ENTRY,Y.STMT.ID,R.STMT.ENTRY,F.STMT.ENTRY,ERR.STMT.ENTRY)
*Read Converted by TUS-Convert
* READ R.STMT.ENTRY FROM F.STMT.ENTRY,Y.STMT.ID THEN ;*Tus Start
            CALL F.READ(FN.STMT.ENTRY,Y.STMT.ID,R.STMT.ENTRY,F.STMT.ENTRY,R.STMT.ENTRY.ERR)
            IF R.STMT.ENTRY THEN ;* Tus End
            END ELSE
                R.STMT.ENTRY = ''
            END
            IF R.STMT.ENTRY<AC.STE.AMOUNT.LCY> GE Y.AMT THEN
                GOSUB DATA.PROCESS
            END
        REPEAT
    REPEAT
***
RETURN
*----------------------------------------------------------------------------------------------------------------------------
ONLINE.PROCESS:
***************
* To Generate the report during online
*
*PACS00345469 -EGA
    Y.SEL.CATEG = R.ACCOUNT<AC.CATEGORY>
    IF Y.ACCOUNT.TYPE THEN
        IF Y.SEL.CATEG NE Y.ACCOUNT.TYPE THEN
            RETURN
        END
    END

    Y.SEL.CCY = R.ACCOUNT<AC.CURRENCY>
    IF Y.CURRENCY THEN
        IF Y.SEL.CCY NE Y.CURRENCY THEN
            RETURN
        END
    END

    Y.SEL.AGENCY = R.ACCOUNT<AC.CO.CODE>
    IF Y.EX.AGENCY THEN
        IF Y.SEL.AGENCY NE Y.EX.AGENCY THEN
            RETURN
        END
    END
*
    IF Y.TRANSACTION.DATE THEN
*YID.LIST = '' ; FROM.DATE = Y.TRANSACTION.DATE[1,8] ; END.DATE = Y.TRANSACTION.DATE[10,8]
        YID.LIST = '' ; FROM.DATE = Y.TRANSACTION.DATE[1,8] ; END.DATE = TODAY
    END ELSE
        YID.LIST = '' ; FROM.DATE = R.ACCOUNT<AC.OPENING.DATE> ; END.DATE = TODAY
    END
*PACS00345469 -EGA
*
    ER = ""
    CALL EB.ACCT.ENTRY.LIST(Y.ACCOUNT.NUMBER,FROM.DATE,END.DATE,YID.LIST,OPENING.BAL,ER)
    LOOP
        REMOVE Y.STMT.ID FROM YID.LIST SETTING STMT.POS
    WHILE Y.STMT.ID : STMT.POS
        R.STMT.ENTRY = '' ; ERR.STMT.ENTRY = ''
* PACS00345469 - EGA - S
*Read Converted by TUS-Convert
* READ R.STMT.ENTRY FROM F.STMT.ENTRY,Y.STMT.ID THEN ;*Tus Start
        CALL F.READ(FN.STMT.ENTRY,Y.STMT.ID,R.STMT.ENTRY,F.STMT.ENTRY,R.STMT.ENTRY.ERR)
        IF R.STMT.ENTRY THEN ;* Tus End
        END ELSE
            R.STMT.ENTRY = ''
        END
* PACS00345469 - EGA - E
        IF R.STMT.ENTRY<AC.STE.AMOUNT.LCY> GE Y.AMT THEN
            GOSUB CHK.COMPANY.CODE
        END
    REPEAT
*
RETURN
*-----------------------------------------------------------------------------------------------------------------------------
CHK.COMPANY.CODE:
*****************
    IF (Y.EX.AGENCY AND R.STMT.ENTRY<AC.STE.COMPANY.CODE> EQ Y.EX.AGENCY) THEN
        GOSUB DATA.PROCESS
    END ELSE
        GOSUB DATA.PROCESS
    END
*
RETURN
*----------------------------------------------------------------------------------------------------------------------------
DATA.PROCESS:
*************
    Y.AGENCY = R.STMT.ENTRY<AC.STE.COMPANY.CODE>
*Y.DATE = R.STMT.ENTRY<AC.STE.VALUE.DATE>
    Y.DATE = R.STMT.ENTRY<AC.STE.BOOKING.DATE>
    Y.TRANS.REF = R.STMT.ENTRY<AC.STE.TRANS.REFERENCE>
    Y.INPUTTER = FIELD(R.STMT.ENTRY<AC.STE.INPUTTER>,'_',2,1)
    Y.AUTHORISER = FIELD(R.STMT.ENTRY<AC.STE.AUTHORISER>,'_',2,1)
    Y.AC.CATEGORY = R.ACCOUNT<AC.CATEGORY>
    CALL CACHE.READ(FN.CATEGORY, Y.AC.CATEGORY, R.AC.CATEG, CAT.ERR) ;*R22 Auto conversion
    Y.AC.CATEGORY = R.AC.CATEG<EB.CAT.DESCRIPTION,1>
    Y.AC.CURRENCY = R.ACCOUNT<AC.CURRENCY>
    Y.CUSTOMER = R.ACCOUNT<AC.CUSTOMER>
    Y.ACCOUNT.OFFICER = R.ACCOUNT<AC.ACCOUNT.OFFICER>
    IF Y.AC.CURRENCY EQ LCCY THEN
        Y.TRANS.AMOUNT = R.STMT.ENTRY<AC.STE.AMOUNT.LCY>
    END ELSE
        Y.TRANS.AMOUNT = R.STMT.ENTRY<AC.STE.AMOUNT.FCY>
    END
*
    IF NOT(Y.ACCOUNT.NAME) THEN
        GOSUB GET.ACCOUNT.NAME
    END

    GOSUB FORM.ARRAY
*
RETURN
*----------------------------------------------------------------------------------------------------------------------------
FORM.ARRAY:
***********
    Y.CURRENCY.SORT = Y.AGENCY:':':Y.DATE:':':Y.AC.CATEGORY:':':Y.ACCOUNT.OFFICER:':':Y.AC.CURRENCY
    Y.ACCOUNT.OFF.SORT = Y.AGENCY:':':Y.DATE:':':Y.AC.CATEGORY:':':Y.ACCOUNT.OFFICER
    Y.CATEG.SORT = Y.AGENCY:':':Y.DATE:':':Y.AC.CATEGORY
    Y.DATE.SORT = Y.AGENCY:':':Y.DATE
    Y.AGENCY.SORT = Y.AGENCY
*
    Y.RETURN<-1> = Y.CURRENCY.SORT:"*":Y.AGENCY:"*":Y.DATE:"*":Y.ACCOUNT.OFFICER:"*":Y.AC.CATEGORY:"*":Y.AC.CURRENCY:"*":
    Y.RETURN:= Y.ACCOUNT.NUMBER:"*":Y.ACCOUNT.NAME:"*":Y.CUSTOMER:"*":Y.TRANS.AMOUNT:"*":Y.TRANS.REF:"*":Y.INPUTTER:"*":Y.AUTHORISER:"*":
    Y.RETURN:= Y.CATEG.SORT:"*":Y.ACCOUNT.OFF.SORT:"*":Y.DATE.SORT:"*":Y.AGENCY.SORT:"*":Y.SELECTION.VALUE:"*":Y.CONV.TIME
*
    Y.AGENCY = ''
    Y.DATE = ''
    Y.TRANS.REF = ''
    Y.INPUTTER = ''
    Y.AUTHORISER = ''
    Y.AC.CATEGORY = ''
    Y.AC.CURRENCY = ''
    Y.CUSTOMER = ''
    Y.ACCOUNT.OFFICER = ''
    Y.TRANS.AMOUNT = ''
*-Shek Y.ACCOUNT.NAME = ''
*
RETURN
*-----------------------------------------------------------------------------------------------------------------------------------------------------
GET.ACCOUNT.NAME:
*****************
    Y.ACC.NAMES = '' ; Y.CUS.NAME = '' ; Y.CUS.NAMES = ''
    R.CUSTOMER = '' ; ERR.CUSTOMER = ''
*READ R.CUSTOMER FROM F.CUSTOMER, Y.CUSTOMER ELSE R.CUSTOMER = ''
*CALL F.READ(FN.CUSTOMER,Y.CUSTOMER,R.CUSTOMER,F.CUSTOMER,ERR.CUSTOMER)
*Read Converted by TUS-Convert
* READ R.CUSTOMER FROM F.CUSTOMER,Y.CUSTOMER THEN ;*Tus Start
    CALL F.READ(FN.CUSTOMER,Y.CUSTOMER,R.CUSTOMER,F.CUSTOMER,R.CUSTOMER.ERR)
    IF R.CUSTOMER THEN ;* Tus End
    END ELSE
        R.CUSTOMER = ''
    END
*
    IF (R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ "PERSONA FISICA") OR (R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ "CLIENTE MENOR") THEN
        Y.CUS.NAME = R.CUSTOMER<EB.CUS.GIVEN.NAMES>:" ":R.CUSTOMER<EB.CUS.FAMILY.NAME>
    END
    IF (R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ "PERSONA JURIDICA") THEN
        Y.CUS.NAME = R.CUSTOMER<EB.CUS.NAME.1,1>:" ":R.CUSTOMER<EB.CUS.NAME.2,1>
    END
    IF NOT(R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS>) THEN
        Y.CUS.NAME = R.CUSTOMER<EB.CUS.SHORT.NAME,1>
    END
*
    GOSUB ANOTHER.PROCESS
**
RETURN
*-----------------------------------------------------------------------------------------------------------------------------------------------------
ANOTHER.PROCESS:
****************
    Y.RELATION.CODE = R.ACCOUNT<AC.RELATION.CODE>
    Y.REL.CNT = DCOUNT(Y.RELATION.CODE,@VM)
    Y.CNT = "1"
    LOOP
    WHILE Y.CNT LE Y.REL.CNT
        Y.REL.CODE = R.ACCOUNT<AC.RELATION.CODE,Y.CNT>
        Y.JOINT.HOLDER = R.ACCOUNT<AC.JOINT.HOLDER,Y.CNT>
        IF (Y.REL.CODE GE '500' AND Y.REL.CODE LE '529') THEN
            R.RELATION = '' ; ERR.RELATION = ''
            CALL F.READ(FN.RELATION,Y.REL.CODE,R.RELATION,F.RELATION,ERR.RELATION)
            Y.REL.DESC = R.RELATION<EB.REL.DESCRIPTION>

            R.CUSTOMER1 = '' ; ERR.CUSTOMER1 = ''
*READ R.CUSTOMER1 FROM F.CUSTOMER, Y.JOINT.HOLDER ELSE R.CUSTOMER1 = ''
*CALL F.READ(FN.CUSTOMER,Y.JOINT.HOLDER,R.CUSTOMER1,F.CUSTOMER,ERR.CUSTOMER)
*Read Converted by TUS-Convert
* READ R.CUSTOMER1 FROM F.CUSTOMER,Y.JOINT.HOLDER THEN ;*Tus Start
            CALL F.READ(FN.CUSTOMER,Y.JOINT.HOLDER,R.CUSTOMER1,F.CUSTOMER,R.CUSTOMER1.ERR)
            IF R.CUSTOMER1 THEN ;* Tus End
            END ELSE
                R.CUSTOMER1 = ''
            END
            Y.CUS.NAMES = ''
            IF (R.CUSTOMER1<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ "PERSONA FISICA") OR (R.CUSTOMER1<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ "CLIENTE MENOR") THEN
                Y.CUS.NAMES = R.CUSTOMER1<EB.CUS.GIVEN.NAMES>:" ":R.CUSTOMER1<EB.CUS.FAMILY.NAME>
            END
            IF R.CUSTOMER1<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ "PERSONA JURIDICA" THEN
                Y.CUS.NAMES = R.CUSTOMER1<EB.CUS.NAME.1,1>:" ":R.CUSTOMER1<EB.CUS.NAME.2,1>
            END
            IF R.CUSTOMER1<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ "" THEN
                Y.CUS.NAMES = R.CUSTOMER1<EB.CUS.SHORT.NAME,1>
            END
            Y.ACC.NAMES<-1> = Y.CUS.NAME:'-':Y.REL.DESC:'-':Y.CUS.NAMES
        END
        Y.CNT += 1
    REPEAT
*
    CHANGE @FM TO @VM IN Y.ACC.NAMES

    IF Y.ACC.NAMES THEN
        Y.ACCOUNT.NAME = Y.ACC.NAMES
    END
    IF Y.CUS.NAME THEN
        Y.ACCOUNT.NAME = Y.CUS.NAME
    END
*
RETURN
*-----------------------------------------------------------------------------------------------------------------------------
END
