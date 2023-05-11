* @ValidationCode : MjotMTgzMTI5ODY0ODpDcDEyNTI6MTY4MjQxMjM2MDY5NTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MTpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:16:00
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
SUBROUTINE REDO.V.VAL.DUE.AMT

*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.V.VAL.DUE.AMT
*---------------------------------------------------------------------------------

*DESCRIPTION       :It is a validation routine attached to install payment version of FT and TELLER
*LINKED WITH       :

* ----------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date               who           Reference            Description
* 3-JUN-2010        Prabhu.N       ODR-2010-01-0081    Initial Creation
* 6-JUN-2010        Marimuthu S    PACS00080543
*-------------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*17-04-2023            Conversion Tool             R22 Auto Code conversion                FM TO @FM,VM TO @VM,SM TO @SM,++ to +=1
*17-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*---------------------------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.PAYMENT.SCHEDULE
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AA.BILL.DETAILS
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_F.ALTERNATE.ACCOUNT
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.ACCOUNT

    IF VAL.TEXT EQ '' THEN
        GOSUB INIT
    END
RETURN
INIT:

    FN.AA.ACCOUNT.DETAILS='F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS=''
    CALL OPF(FN.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS)

    FN.AA.BILL.DETAILS='F.AA.BILL.DETAILS'
    F.AA.BILL.DETAILS=''
    CALL OPF(FN.AA.BILL.DETAILS,F.AA.BILL.DETAILS)

    FN.ALTERNATE.ACCOUNT='F.ALTERNATE.ACCOUNT'
    F.ALTERNATE.ACCOUNT=''
    CALL OPF(FN.ALTERNATE.ACCOUNT,F.ALTERNATE.ACCOUNT)

    FN.EB.CONTRACT.BALANCES='F.EB.CONTRACT.BALANCES'
    F.EB.CONTRACT.BALANCES=''
    CALL OPF(FN.EB.CONTRACT.BALANCES,F.EB.CONTRACT.BALANCES)

    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    VAR.NO.OF.INSTALLMENT=COMI
* PACS00080543 -s
    Y.DUP.INS.CNT = VAR.NO.OF.INSTALLMENT
* PACS00080543 -e
    IF APPLICATION EQ 'FUNDS.TRANSFER' THEN
        VAR.AC.ID=R.NEW(FT.CREDIT.ACCT.NO)
        LREF.APP='FUNDS.TRANSFER'
        LREF.FLD = 'L.FT.INSTAL.AMT'
        GOSUB PROCESS
* PACS00080543 -s
        Y.POS.INS.AMT = LREF.POS<1,3>
        Y.VAL.CK = R.NEW(FT.LOCAL.REF)<1,Y.POS.INS.AMT>
        R.NEW(FT.CREDIT.AMOUNT)= R.NEW(FT.LOCAL.REF)<1,Y.POS.INS.AMT> * Y.DUP.INS.CNT
* PACS00080543 -e
        R.NEW(FT.LOCAL.REF)<1,LREF.POS<1,1>>=VAR.PRINCIPAL.DUE
        R.NEW(FT.LOCAL.REF)<1,LREF.POS<1,2>>=VAR.INTEREST.DUE
    END
    IF APPLICATION EQ 'TELLER' THEN
        VAR.AC.ID=R.NEW(TT.TE.ACCOUNT.2)
        LREF.APP='TELLER'
        LREF.FLD = 'L.TT.INSTAL.AMT'
        GOSUB PROCESS
* PACS00080543 -s
        Y.POS.INS.AMT = LREF.POS<1,3>
        Y.VAL.CK = R.NEW(FT.LOCAL.REF)<1,Y.POS.INS.AMT>
        R.NEW(TT.TE.AMOUNT.LOCAL.1)= R.NEW(TT.TE.LOCAL.REF)<1,Y.POS.INS.AMT> * Y.DUP.INS.CNT
* PACS00080543 -e
        R.NEW(TT.TE.LOCAL.REF)<1,LREF.POS<1,1>>=VAR.PRINCIPAL.DUE
        R.NEW(TT.TE.LOCAL.REF)<1,LREF.POS<1,2>>=VAR.INTEREST.DUE
    END
RETURN

PROCESS:

* PACS00080543 -s
    LREF.FIELDS='L.PRINC.AMT.DUE':@VM:'L.INT.AMT.DUE':@VM:LREF.FLD
* PACS00080543 -E
    LREF.POS=''
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)

    CALL F.READ(FN.ACCOUNT,VAR.AC.ID,R.ACCOUNT,F.ACCOUNT,ERR)
    VAR.AA.ID=R.ACCOUNT<AC.ARRANGEMENT.ID>
    CALL F.READ(FN.AA.ACCOUNT.DETAILS,VAR.AA.ID,R.AA.ACCOUNT.DETAILS ,F.AA.ACCOUNT.DETAILS ,ERR)
    VAR.BILL.LIST=R.AA.ACCOUNT.DETAILS<AA.AD.BILL.ID>
    VAR.SET.STATUS.LIST=R.AA.ACCOUNT.DETAILS<AA.AD.SET.STATUS>
    CHANGE @SM TO @FM IN VAR.BILL.LIST
    CHANGE @VM TO @FM IN VAR.BILL.LIST
    CHANGE @VM TO @FM IN VAR.SET.STATUS.LIST
    CHANGE @SM TO @FM IN VAR.SET.STATUS.LIST
    VAR.BILL.LIST.SIZE=DCOUNT(VAR.BILL.LIST,@FM)
    BILL.CNT=1
    LOOP
        VAR.SET.STATUS=VAR.SET.STATUS.LIST<BILL.CNT>
        IF VAR.SET.STATUS EQ 'UNPAID' THEN

            CALL F.READ(FN.AA.BILL.DETAILS,VAR.BILL.LIST<BILL.CNT>,R.AA.BILL.DETAILS,F.AA.BILL.DETAILS,ERR)
            VAR.NO.OF.INSTALLMENT-=1
            VAR.OS.AMOUNT=VAR.OS.AMOUNT+SUM(R.AA.BILL.DETAILS<AA.BD.OS.PROP.AMOUNT>)
            VAR.PROPERTY=R.AA.BILL.DETAILS<AA.BD.PROPERTY>
            CHANGE @VM TO @FM IN VAR.PROPERTY
            VAR.OS.PROP.AMOUNT=R.AA.BILL.DETAILS<AA.BD.OS.PROP.AMOUNT>
            CHANGE @VM TO @FM IN VAR.OS.PROP.AMOUNT

            LOCATE 'ACCOUNT' IN VAR.PROPERTY SETTING ACC.POS THEN
                VAR.PRINCIPAL.DUE= VAR.PRINCIPAL.DUE+VAR.OS.PROP.AMOUNT<ACC.POS>
            END
            LOCATE 'PRINCIPALINT' IN VAR.PROPERTY SETTING INT.POS THEN
                VAR.INTEREST.DUE=VAR.INTEREST.DUE+VAR.OS.PROP.AMOUNT<INT.POS>
            END
            IF VAR.NO.OF.INSTALLMENT LE 0 THEN
                Y.EXIT=1
            END
        END
    WHILE BILL.CNT LE VAR.BILL.LIST.SIZE AND NOT(Y.EXIT)
        BILL.CNT += 1
    REPEAT
RETURN
END
