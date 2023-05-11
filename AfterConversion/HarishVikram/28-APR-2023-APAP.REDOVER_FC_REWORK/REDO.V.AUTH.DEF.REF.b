* @ValidationCode : MjoxNDUzMzI5ODczOkNwMTI1MjoxNjgyNDEyMzM5MDI4OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:39
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.AUTH.DEF.REF
*-------------------------------------------------------------------------------------------
*DESCRIPTION:
*   This is auth rouitne for single method of payment version in B29.It will default value in
*  transaction ref field
* ------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who           Reference            Description
* 27-JAN-2010       Prabhu.N       ODR-2009-10-0315             Initial Creation
* 01-11-2011        SUDHARSANAN S   CR.18                 Update the local field based on condition
*--------------------------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*10-04-2023            Conversion Tool             R22 Auto Code conversion                FM, TO @FM,VM TO @VM,SM TO @SM, F.READ TO CACHE.READ, Y.COUNT+1 TO +=1
*10-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*---------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.DEAL.SLIP.FORMAT
    $INSERT I_GTS.COMMON
    $INSERT I_System
    $INSERT I_F.ACCOUNT
    $INSERT I_F.ACCT.CAPITALISATION
    $INSERT I_F.CATEGORY
    GOSUB INITIALISE
    GOSUB PROCESS
RETURN
***********
INITIALISE:
***********
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CATEGORY = 'F.CATEGORY'
    F.CATEGORY = ''
    CALL OPF(FN.CATEGORY,F.CATEGORY)

    FN.ACCAP = 'F.ACCT.CAPITALISATION'
    F.ACCAP = ''
    CALL OPF(FN.ACCAP,F.ACCAP)

    LOC.REF.APPLICATION="AZ.ACCOUNT"
    LOC.REF.FIELDS='L.AZ.REF.NO':@VM:'L.AZ.DEBIT.ACC':@VM:'L.AZ.METHOD.PAY':@VM:'L.AZ.DEP.NAME':@VM:'L.AZ.AMOUNT':@VM:'L.TYPE.INT.PAY'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.AZ.REF.NO=LOC.REF.POS<1,1>
    POS.L.AZ.DEBIT.ACC = LOC.REF.POS<1,2>
    POS.L.AZ.METHOD.PAY = LOC.REF.POS<1,3>
    POS.L.AZ.DEP.NAME = LOC.REF.POS<1,4>
    POS.L.AZ.AMOUNT = LOC.REF.POS<1,5>
    POS.L.TYPE.INT.PAY = LOC.REF.POS<1,6>
    OFS.ACCT.CAP = ''
    Y.OFS.POST.ARRAY = ''
    VAR.INT.RATE = ''
    OFS.SRC.ID = 'REINV.DEPOSIT'
    OFS$DEAL.SLIP.PRINTING = 1
    V$FUNCTION = "I"
    SAVE.APPLICATION = APPLICATION
RETURN
***********
PROCESS:
***********
    VAR.REPAY.ACC = R.NEW(AZ.REPAY.ACCOUNT)
    IF VAR.REPAY.ACC THEN
        R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.REF.NO>="AZ-":ID.NEW
    END
    Y.AZ.DEBIT.ACC = R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.DEBIT.ACC>
    Y.AZ.METHOD.PAY =  R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.METHOD.PAY>
    CHANGE @VM TO @FM IN Y.AZ.METHOD.PAY
    Y.CURR.NO = R.OLD(AZ.CURR.NO)
    IF R.NEW(AZ.LOCAL.REF)<1,POS.L.TYPE.INT.PAY> EQ 'Reinvested' AND Y.CURR.NO THEN
        GOSUB ACCT.CAPT.UPD
    END
    IF NOT(Y.CURR.NO) THEN
        GOSUB CASE.STMT
    END ELSE
        OFS$DEAL.SLIP.PRINTING = 0
        GOSUB PGM.END
    END
RETURN
***********
CASE.STMT:
***********
    BEGIN CASE
        CASE VAR.REPAY.ACC NE ''
            GOSUB REPAY.ACCOUNT
        CASE Y.AZ.DEBIT.ACC NE ''
            GOSUB AZ.DEPOSIT.ACCOUNT
            OFS$DEAL.SLIP.PRINTING = 1
        CASE 1
            OFS$DEAL.SLIP.PRINTING = 0
            GOSUB PGM.END
    END CASE
RETURN
*************
REPAY.ACCOUNT:
**************
    Y.ACCT.NO = VAR.REPAY.ACC
    Y.VALUE.DATE = R.NEW(AZ.VALUE.DATE)
    GOSUB CONV.VALUE.DATE
    CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    Y.TITLE1 = R.ACCOUNT<AC.ACCOUNT.TITLE.1>:R.ACCOUNT<AC.ACCOUNT.TITLE.2>
    Y.PRINCIPAL = R.NEW(AZ.PRINCIPAL)
    Y.PRINCIPAL = TRIMB(FMT(Y.PRINCIPAL,'L2,#19'))
    Y.TOTAL.AMOUNT  = R.NEW(AZ.CURRENCY):Y.PRINCIPAL
    Y.CATEGORY = R.NEW(AZ.CATEGORY)
    CALL CACHE.READ(FN.CATEGORY, Y.CATEGORY, R.CATEGORY, CATEG.ERR)  ;*R22 AUTO CODE CONVERSION
    Y.CATEG.DESC = R.CATEGORY<EB.CAT.DESCRIPTION>
    Y.NAME = R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.DEP.NAME>
    Y.NAME = Y.NAME:'-':ID.NEW
    CALL F.READ(FN.ACCOUNT,ID.NEW,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    Y.TITLE2 = R.ACCOUNT<AC.ACCOUNT.TITLE.1>:R.ACCOUNT<AC.ACCOUNT.TITLE.2>
    CALL System.setVariable("CURRENT.VALUE.DATE",SYS.DATE)
    CALL System.setVariable("CURRENT.ACCT.NO",Y.ACCT.NO)
    CALL System.setVariable("CURRENT.TITLE",Y.TITLE1)
    CALL System.setVariable("CURRENT.TOTAL.AMOUNT",Y.TOTAL.AMOUNT)
    CALL System.setVariable("CURRENT.CATEGORY.DESC",Y.CATEG.DESC)
    CALL System.setVariable("CURRENT.TITLE2",Y.TITLE2)
    Y.PRINCIPAL = TRIMB(FMT(Y.PRINCIPAL,'L2,#19'))
    Y.PRINCIPAL  = R.NEW(AZ.CURRENCY):Y.PRINCIPAL
    CALL System.setVariable("CURRENT.PRINCIPAL",Y.PRINCIPAL)
    CALL System.setVariable("CURRENT.NAME",Y.NAME)
    CALL PRODUCE.DEAL.SLIP('REDO.AZ.REPAY')
RETURN
******************
AZ.DEPOSIT.ACCOUNT:
*******************
    Y.COUNT = 1
    Y.TOTAL.METHOD = DCOUNT(Y.AZ.METHOD.PAY,@SM)
    LOOP
    WHILE Y.COUNT LE Y.TOTAL.METHOD
        CHANGE @SM TO @FM IN Y.AZ.METHOD.PAY
        LOCATE 'FROM.CUST.ACC' IN Y.AZ.METHOD.PAY SETTING Y.METHOD.PAY.POS THEN
            IF Y.AZ.METHOD.PAY<Y.COUNT> EQ 'FROM.CUST.ACC' THEN
                Y.ACCT.NO = R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.DEBIT.ACC,Y.COUNT>
                Y.VALUE.DATE = R.NEW(AZ.VALUE.DATE)
                GOSUB CONV.VALUE.DATE
                CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
                Y.PRINCIPAL = R.NEW(AZ.PRINCIPAL)
                Y.TITLE1 = R.ACCOUNT<AC.ACCOUNT.TITLE.1>:R.ACCOUNT<AC.ACCOUNT.TITLE.2>
                Y.POS.L.AZ.AMOUNT = R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.AMOUNT,Y.COUNT>
                Y.POS.L.AZ.AMOUNT = TRIMB(FMT(Y.POS.L.AZ.AMOUNT,'L2,#19'))
                Y.TOTAL.AMOUNT  = LCCY:Y.POS.L.AZ.AMOUNT
                Y.CATEGORY = R.NEW(AZ.CATEGORY)
                CALL CACHE.READ(FN.CATEGORY, Y.CATEGORY, R.CATEGORY, CATEG.ERR)    ;*R22 AUTO CODE CONVERSION
                Y.CATEG.DESC = R.CATEGORY<EB.CAT.DESCRIPTION>
                Y.NAME = R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.DEP.NAME>:'-':ID.NEW
                CALL F.READ(FN.ACCOUNT,ID.NEW,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
                Y.TITLE2 = R.ACCOUNT<AC.ACCOUNT.TITLE.1>:R.ACCOUNT<AC.ACCOUNT.TITLE.2>
                CALL System.setVariable("CURRENT.ACCT.NO",Y.ACCT.NO)
                CALL System.setVariable("CURRENT.VALUE.DATE",SYS.DATE)
                CALL System.setVariable("CURRENT.TITLE",Y.TITLE1)
                CALL System.setVariable("CURRENT.TOTAL.AMOUNT",Y.TOTAL.AMOUNT)
                CALL System.setVariable("CURRENT.CATEGORY.DESC",Y.CATEG.DESC)
                CALL System.setVariable("CURRENT.TITLE2",Y.TITLE2)
                Y.PRINCIPAL = TRIMB(FMT(Y.PRINCIPAL,'L2,#19'))
                Y.PRINCIPAL  = R.NEW(AZ.CURRENCY):Y.PRINCIPAL
                CALL System.setVariable("CURRENT.PRINCIPAL",Y.PRINCIPAL)
                CALL System.setVariable("CURRENT.NAME",Y.NAME)
                CALL PRODUCE.DEAL.SLIP('REDO.AZ.REPAY')
                OFS$DEAL.SLIP.PRINTING = 0
            END
        END ELSE
            GOSUB PGM.END
        END
        Y.COUNT += 1
    REPEAT
RETURN
***************
CONV.VALUE.DATE:
***************
    TEMP.COMI = COMI ; TEMP.N1=N1 ; TEMP.T1 = T1
    COMI= Y.VALUE.DATE ; N1=8 ; T1=".D"
    CALL IN2D(N1,T1)
    SYS.DATE = V$DISPLAY
    COMI = TEMP.COMI ; N1 = TEMP.N1 ; T1 = TEMP.T1
RETURN
****************
ACCT.CAPT.UPD:
****************
    Y.INT.LIQ.ACC = R.NEW(AZ.INTEREST.LIQU.ACCT)
    CALL F.READ(FN.ACCAP,Y.INT.LIQ.ACC,R.AZ.INT.LIQ.ACCT.CAP,F.ACCAP,ACC.CAP.ERR)
    IF R.AZ.INT.LIQ.ACCT.CAP EQ '' THEN
        Y.ACCT.ARRAY<IC.ACCAP.CR.CAP.FREQUENCY> = R.NEW(AZ.FREQUENCY)
        Y.ACCT.ARRAY<IC.ACCAP.DR.CAP.FREQUENCY> = R.NEW(AZ.FREQUENCY)
        GOSUB FORM.OFS.ACCT.CAP
    END ELSE
        IF R.NEW(AZ.FREQUENCY) NE R.OLD(AZ.FREQUENCY) THEN
            Y.ACCT.ARRAY<IC.ACCAP.CR.CAP.FREQUENCY> = R.NEW(AZ.FREQUENCY)
            Y.ACCT.ARRAY<IC.ACCAP.DR.CAP.FREQUENCY> = R.NEW(AZ.FREQUENCY)
            GOSUB FORM.OFS.ACCT.CAP
        END
    END

RETURN
*******************
FORM.OFS.ACCT.CAP:
*******************

    ACTUAL.APP.NAME = 'ACCT.CAPITALISATION'
    OFS.FUNCTION = 'I'
    PROCESS = 'PROCESS'
    OFS.VERSION = ''
    GTSMODE = ''
    NO.OF.AUTH = '0'
    TRANSACTION.ID = Y.INT.LIQ.ACC
    OFS.RECORD = ''
    VERSION.ACCT = 'ACCT.CAPITALISATION,RE'
    MSG.ID = ''
    ERR.OFS = ''
    OPTION = ''
    CALL OFS.BUILD.RECORD(ACTUAL.APP.NAME,OFS.FUNCTION,PROCESS,VERSION.ACCT,GTSMODE,NO.OF.AUTH,TRANSACTION.ID,Y.ACCT.ARRAY,OFS.ACCT.CAP)
    CALL OFS.POST.MESSAGE(OFS.ACCT.CAP,MSG.ID,OFS.SRC.ID,ERR.OFS)

RETURN
*******
PGM.END:
*******
END
