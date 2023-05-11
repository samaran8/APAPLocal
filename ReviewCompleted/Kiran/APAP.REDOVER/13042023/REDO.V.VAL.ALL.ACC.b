* @ValidationCode : Mjo3NTg0MzcyMjI6Q3AxMjUyOjE2ODEzNzI1MjE2NTQ6c2FtYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 13:25:21
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.VAL.ALL.ACC
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.V.VAL.CREDIT
*---------------------------------------------------------------------------------

*DESCRIPTION       :It is the input routine to validate the credit and debit accounts
*
*
*LINKED WITH       :

* ----------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date               who           Reference            Description
* 16-APR-2010        Prabhu.N       ODR-2010-08-0031   Initial Creation
* 07-APR-2011        Prabhu.N       PACS00036498       Routine modified
* 19-09-2011         PRABHUN        PACS00125978       MODIFICATION
* 30-09-2011         PRABHU N       PACS00036498        MODIFICATION
*-------------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*13-04-2023            Conversion Tool             R22 Auto Code conversion                      FM TO @FM VM TO @VM,++ TO +=1,IF CONDITION ADDED
*13-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*-----------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_F.ACCOUNT
    $INSERT I_EB.EXTERNAL.COMMON
    $INSERT I_F.STANDING.ORDER
*   $INSERT I_F.FUNDS.TRANSFER     ;*R22 AUTO CODE CONVERSION
    $INSERT I_F.BENEFICIARY
    $INSERT I_System
    $INSERT I_GTS.COMMON
*Tus Start
    $INSERT I_F.EB.CONTRACT.BALANCES
*Tus End
    GOSUB INIT
RETURN
*---
INIT:
*---


    FN.AC.LOCKED.EVENTS='F.AC.LOCKED.EVENTS'
    F.AC.LOCKED.EVENTS=''
    CALL OPF(FN.AC.LOCKED.EVENTS,F.AC.LOCKED.EVENTS)

    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER.ACCOUNT='F.CUSTOMER.ACCOUNT'
    F.CUSTOMER.ACCOUNT=''
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)

    LREF.APP = 'AC.LOCKED.EVENTS':@FM:'ACCOUNT'
    LREF.FIELDS = 'L.AC.GAR.AMT':@FM:'L.AC.STATUS1':@VM:'L.AC.NOTIFY.1':@VM:'L.AC.STATUS2':@VM:'L.AC.AV.BAL'
    LREF.POS=''
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    L.AC.LOCK.POS=LREF.POS<1,1>
    L.AC.STATUS.POS=LREF.POS<2,1>
    L.NOTIFY.POS = LREF.POS<2,2>
    L.STATS2.POS = LREF.POS<2,3>
    L.AVL.BAL.POS=LREF.POS<2,4>
    Y.NOTIFY=''
    Y.STATS2=''
    NO.STATS2.DB.CR='DECEASED'
    NO.STATS2.DB.BUT.CR = 'GARNISHMENT':@FM:'GUARANTEE.STATUS'
    NTFY.ACPT.DB.CR='DEBIT.PAYMENT':@FM:'RETURNED.CHEQUE':@FM:'SIGNATURE.MISSING':@FM:'TELLER'
    NTFY.NO.DB.CR='NOTIFY.MGMT.MONEY.LAUNDRY.PREV':@FM:'NOTIFY.OFFICER':@FM:'DECEASED'
    VAR.GAR.AMT = ''
    Y.END.DATE.VAL = ''
    Y.FROM.DATE.VAL1 = ''
    Y.FROM.DATE.INIT = ''
    DEB.FLG= ''
    IF APPLICATION EQ 'FUNDS.TRANSFER' THEN

        R.NEW(FT.CREDIT.THEIR.REF)=ID.NEW
        Y.VAR.ACC.NO = R.NEW(FT.DEBIT.ACCT.NO)
        Y.DEB.ACC.NO=R.NEW(FT.DEBIT.ACCT.NO)
*AF = FT.DEBIT.ACCT.NO
*GOSUB READ.ACCOUNT
*CALL F.READ(FN.ACCOUNT,Y.VAR.ACC.NO,R.ACCOUNT,F.ACCOUNT,ERR)
        IF R.NEW(FT.DEBIT.ACCT.NO) AND Y.VAR.ACC.NO[1,2] NE 'PL' THEN
            DEB.FLG=1
*AF=FT.DEBIT.ACCT.NO
            GOSUB READ.ACCOUNT
            GOSUB CHECK.STATUS
            GOSUB CHECK.BALANCE
        END
        Y.VAR.ACC.NO = ''
        R.ACCOUNT = ''
        ERR = ''
        DEB.FLG=''
        Y.VAR.ACC.NO = R.NEW(FT.CREDIT.ACCT.NO)
        Y.CR.ACC.NO = R.NEW(FT.CREDIT.ACCT.NO)
*AF = FT.CREDIT.ACCT.NO
        IF R.NEW(FT.CREDIT.ACCT.NO) AND Y.VAR.ACC.NO[1,2] NE 'PL' THEN
*AF=FT.CREDIT.ACCT.NO
            GOSUB READ.ACCOUNT
            GOSUB CHECK.STATUS
*            GOSUB CHECK.BALANCE

        END
*CALL F.READ(FN.ACCOUNT,Y.VAR.ACC.NO,R.ACCOUNT,F.ACCOUNT,ERR)
* GOSUB CHECK.STATUS
* GOSUB CHECK.BALANCE
*PACS00036498-S/E
        IF Y.VAR.ACC.NO[1,2] NE 'PL' THEN
            GOSUB CHECK.STATUS
        END
    END
    IF APPLICATION EQ 'STANDING.ORDER' THEN


        Y.VAR.ACC.NO = ID.NEW
        Y.VAR.ACC.NO=FIELD(Y.VAR.ACC.NO,'.',1)
        R.ACCOUNT=''
        CALL F.READ(FN.ACCOUNT,Y.VAR.ACC.NO,R.ACCOUNT,F.ACCOUNT,ERR)
        R.ECB= '' ; ECB.ERR= '' ;*Tus Start
        CALL EB.READ.HVT("EB.CONTRACT.BALANCES",Y.VAR.ACC.NO,R.ECB,ECB.ERR);*Tus End
        GOSUB CHECK.BALANCE
        Y.VAR.ACC.NO = ''
        R.ACCOUNT = ''
        ERR = ''
        Y.VAR.ACC.NO = R.NEW(STO.CPTY.ACCT.NO)
*AF = STO.CPTY.ACCT.NO
        CALL F.READ(FN.ACCOUNT,Y.VAR.ACC.NO,R.ACCOUNT,F.ACCOUNT,ERR)
        R.ECB= '' ; ECB.ERR= '' ;*Tus Start
        CALL EB.READ.HVT("EB.CONTRACT.BALANCES",Y.VAR.ACC.NO,R.ECB,ECB.ERR);*Tus End
        GOSUB CHECK.STATUS
        GOSUB CHECK.BALANCE
    END
RETURN
*-------------
CHECK.BALANCE:
*-------------

    Y.FROM.DATE.VAL = R.ACCOUNT<AC.FROM.DATE>
    IF Y.FROM.DATE.VAL THEN
        Y.FROM.DATE.CNT = DCOUNT(Y.END.DATE.VAL,@VM)
        Y.END.DATE.INIT = 1
        VAR.GAR.AMT = ''
        IF Y.FROM.DATE.CNT GT 1 THEN
            LOOP
                REMOVE Y.END.DATE.VAL1 FROM Y.END.DATE.VAL SETTING Y.END.DATE.POS
            WHILE Y.FROM.DATE.INIT LE Y.FROM.DATE.CNT
                IF Y.FROM.DATE.VAL1 GE TODAY OR Y.FROM.DATE.VAL EQ '' THEN
                    IF NOT(VAR.GAR.AMT) THEN
                        VAR.GAR.AMT = R.ACCOUNT<AC.LOCKED.AMOUNT,Y.FROM.DATE.INIT>
                    END ELSE
                        VAR.GAR.AMT+=R.ACCOUNT<AC.LOCKED.AMOUNT,Y.FROM.DATE.INIT>
                    END
                END
                Y.END.DATE.INIT += 1
            REPEAT
        END ELSE
            IF Y.FROM.DATE.VAL GE TODAY OR Y.FROM.DATE.VAL EQ '' THEN
                VAR.GAR.AMT = R.ACCOUNT<AC.LOCKED.AMOUNT>
            END
        END
    END ELSE
        VAR.GAR.AMT = ''
    END
* VAR.ONLINE.BAL=R.ACCOUNT<AC.ONLINE.ACTUAL.BAL>;*Tus Start
    VAR.ONLINE.BAL=R.ECB<ECB.ONLINE.ACTUAL.BAL>;*Tus End
    Y.CREDIT.AMOUNT= R.NEW(FT.CREDIT.AMOUNT)
    Y.REQ.AMOUNT=VAR.GAR.AMT+Y.CREDIT.AMOUNT
    IF VAR.GAR.AMT THEN
        IF VAR.ONLINE.BAL LE Y.REQ.AMOUNT THEN
            ETEXT = 'EB-AMT.NOT.AVAIL'
            AF=''
            CALL STORE.END.ERROR
        END
    END


* GOSUB CHECK.STATUS
    GOSUB CHECK.USER
RETURN
*--------------
CHECK.STATUS:
*--------------
*
    IF R.ACCOUNT<AC.LOCAL.REF><1,L.AC.STATUS.POS> NE 'ACTIVE' THEN
        ETEXT = 'EB-INACT.ACCOUNT'
        AF=''
        CALL STORE.END.ERROR
    END

    IF EB.EXTERNAL$CHANNEL EQ 'INTERNET' THEN

        Y.NOTIFY = R.ACCOUNT<AC.LOCAL.REF><1,L.NOTIFY.POS>
        Y.STATS2 = R.ACCOUNT<AC.LOCAL.REF><1,L.STATS2.POS>
*VIRTUAL.TAB.ID='L.AC.NOTIFY.1'
*CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID)
*Y.LOOKUP.LIST=VIRTUAL.TAB.ID<2>
*Y.LOOKUP.DESC=VIRTUAL.TAB.ID<11>
*CHANGE '_' TO FM IN Y.LOOKUP.LIST
*CHANGE '_' TO FM IN Y.LOOKUP.DESC
*LOCATE Y.NOTIFY IN Y.LOOKUP.LIST SETTING POS1 THEN

        LOCATE Y.NOTIFY IN NTFY.NO.DB.CR SETTING POS1 THEN

            ETEXT = 'AI-ACCT.BAL.LT.LOCKED'
*ERR.MESS1= 'AI-ACCT.BAL.LT.LOCKED'
*CALL EB.GET.ERROR.MESSAGE(ERR.MESS1)
*EXACT.ERR.MSG1 = FIELD(ERR.MESS1<1,1>,' ',3)

*FIND EXACT.ERR.MSG1 IN OFS$ETEXT SETTING ETEX.POS ELSE
            AF=''
            CALL STORE.END.ERROR
*END
        END

        GOSUB CHECK.NOTIFY.STATUS
    END


RETURN
*-----------
CHECK.USER:
*-----------
*PACS00125978-S/E
    IF APPLICATION NE 'STANDING.ORDER' THEN
        Y.CUSTOMER =System.getVariable('EXT.SMS.CUSTOMERS')
        IF E EQ "EB-UNKNOWN.VARIABLE" THEN   ;*R22 AUTO CODE CONVERSION.START

            Y.CUSTOMER = ""    ;*R22 AUTO CODE CONVERSION
        END    ;*R22 AUTO CODE CONVERSION.END

        CALL F.READ(FN.CUSTOMER.ACCOUNT,Y.CUSTOMER,R.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT,ERR)
        LOCATE Y.VAR.ACC.NO IN R.CUSTOMER.ACCOUNT SETTING POS ELSE
            ETEXT ='EB-NOT.USER.ACCT'
            AF=''
            CALL STORE.END.ERROR
        END
    END

RETURN
*------------------*
CHECK.NOTIFY.STATUS:
*-----------------*
    STATS2.VAL = R.ACCOUNT<AC.LOCAL.REF><1,L.STATS2.POS>
    TOT.AVL.BAL=R.ACCOUNT<AC.LOCAL.REF><1,L.AVL.BAL.POS>
    IF DEB.FLG THEN

        LOCATE STATS2.VAL IN NO.STATS2.DB.BUT.CR SETTING POS1 THEN
*AF=FT.DEBIT.ACCT.NO
*            IF R.NEW(FT.DEBIT.AMOUNT) GT TOT.AVL.BAL THEN
            IF TOT.AVL.BAL LT 0 THEN
                ETEXT = 'AI-ACCT.BAL.LT.LOCKED'
                AF=''
                CALL STORE.END.ERROR
            END
        END

    END
RETURN

************
READ.ACCOUNT:
***********
    CALL F.READ(FN.ACCOUNT,Y.VAR.ACC.NO,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    R.ECB= '' ; ECB.ERR= '' ;*Tus Start
    CALL EB.READ.HVT("EB.CONTRACT.BALANCES",Y.VAR.ACC.NO,R.ECB,ECB.ERR);*Tus End

RETURN
END
*---------------------------------------------*END OF SUBROUTINE*-------------------------------------------
