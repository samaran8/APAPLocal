* @ValidationCode : MjotOTQxNTE4ODY0OkNwMTI1MjoxNjgyNDEyMzY0NzMyOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:16:04
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
SUBROUTINE REDO.V.VAL.SRC.ACC
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.V.VAL.SRC.ACC
*---------------------------------------------------------------------------------

*DESCRIPTION       :It is the input routine to validate the credit accounts

* ----------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date        who                 Reference           Description
* 03-12-2010    Prabhu.N            ODR-2010-11-0211    Initial Creation
* 04-04-2011    Prabhu.N            PACS00036498        Added to validate all account
* 21-07-2015    Vignesh Kumaar R    PACS00466352        0006793: PAGO DE TARJETA DE CREDITO
*-------------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*17-04-2023            Conversion Tool             R22 Auto Code conversion                      FM TO @FM VM TO @VM,++ TO +=1
*17-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*---------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_F.ACCOUNT
    $INSERT I_EB.EXTERNAL.COMMON
    $INSERT I_F.STANDING.ORDER
    $INSERT I_System
    $INSERT I_F.REDO.APAP.STO.DUPLICATE
    $INSERT I_F.FT.BULK.SUP.PAY
    $INSERT I_F.AI.REDO.ARCIB.PARAMETER
    $INSERT I_F.EB.CONTRACT.BALANCES ;*Tus S/E

    GOSUB INIT
RETURN
*---
INIT:
*---
    R.ECB = '' ;*Tus S/E
    Y.WORK.BAL = '' ;*Tus S/E

    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER.ACCOUNT='F.CUSTOMER.ACCOUNT'
    F.CUSTOMER.ACCOUNT=''
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)


    FN.FT.BULK.SUP.PAY = 'F.FT.BULK.SUP.PAY'
    F.FT.BULK.SUP.PAY = ''
    CALL OPF(FN.FT.BULK.SUP.PAY,F.FT.BULK.SUP.PAY)

    FN.AI.REDO.ARCIB.PARAMETER = 'F.AI.REDO.ARCIB.PARAMETER'
    F.AI.REDO.ARCIB.PARAMETER  = ''
*CALL OPF(FN.AI.REDO.ARCIB.PARAMETER,F.AI.REDO.ARCIB.PARAMETER) ;*Tus S/E

    LREF.APP = 'ACCOUNT':@FM:'FUNDS.TRANSFER'
    LREF.FIELDS = 'L.AC.AV.BAL':@FM:'L.TT.TAX.AMT'
    LREF.POS=''
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    POS.L.AC.AV.BAL = LREF.POS<1,1>
    POS.L.TT.TAX.AMT = LREF.POS<2,1>
*  CALL F.READ(FN.AI.REDO.ARCIB.PARAMETER,'SYSTEM',R.AI.REDO.ARCIB.PARAMETER,F.AI.REDO.ARCIB.PARAMETER,PARAM.ERR) ;*Tus Start
    CALL CACHE.READ(FN.AI.REDO.ARCIB.PARAMETER,'SYSTEM',R.AI.REDO.ARCIB.PARAMETER,PARAM.ERR) ; * Tus End
    Y.TXN.TYPE = R.AI.REDO.ARCIB.PARAMETER<AI.PARAM.TRANSACTION.TYPE>

    IF APPLICATION EQ 'FUNDS.TRANSFER' THEN
        Y.VAR.ACC.NO = R.NEW(FT.DEBIT.ACCT.NO)
        Y.DEBIT.AMOUNT = R.NEW(FT.DEBIT.AMOUNT)
        AF = FT.DEBIT.AMOUNT
        IF NOT(Y.DEBIT.AMOUNT) THEN
            Y.DEBIT.AMOUNT = R.NEW(FT.CREDIT.AMOUNT)
            AF = FT.CREDIT.AMOUNT
        END
        GOSUB CHECK.BALANCE.FT
    END
    IF APPLICATION EQ 'STANDING.ORDER' THEN
        Y.VAR.ACC.NO = ID.NEW
        Y.VAR.ACC.NO=FIELD(Y.VAR.ACC.NO,'.',1)
        AF = STO.CURRENT.AMOUNT.BAL
        Y.DEBIT.AMOUNT = R.NEW(STO.CURRENT.AMOUNT.BAL)
*        GOSUB CHECK.BALANCE.STO
    END

    IF APPLICATION EQ 'REDO.APAP.STO.DUPLICATE' THEN
        Y.VAR.ACC.NO= R.NEW(REDO.SO.ORIGIN.ACCT.NO)
        AF = REDO.SO.CURRENT.AMOUNT.BAL
        Y.DEBIT.AMOUNT = R.NEW(REDO.SO.CURRENT.AMOUNT.BAL)
*        GOSUB CHECK.BALANCE.STO.DUP
    END

    IF APPLICATION EQ 'FT.BULK.SUP.PAY' THEN
        Y.VAR.ACC.NO = R.NEW(FT.BUL76.DR.ACCOUNT)
        GOSUB CHECK.BALANCE.BULK
    END
RETURN
*----------------
CHECK.BALANCE.FT:
*----------------
    CALL F.READ(FN.ACCOUNT,Y.VAR.ACC.NO,R.ACCOUNT,F.ACCOUNT,ERR)
    Y.ACCT.BAL = R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.AV.BAL>
*Y.WORK.BAL = R.ACCOUNT<AC.WORKING.BALANCE> ;*Tus Start
    CALL EB.READ.HVT('EB.CONTRACT.BALANCES',Y.VAR.ACC.NO,R.ECB,ECB.ERR)
    Y.WORK.BAL = R.ECB<ECB.WORKING.BALANCE> ;*Tus End

    Y.TAX.AMT = R.NEW(FT.LOCAL.REF)<1,POS.L.TT.TAX.AMT>

    IF Y.TAX.AMT GT Y.ACCT.BAL OR Y.ACCT.BAL LT 0 THEN
        ETEXT = 'EB-AMT.NOT.AVAIL'
        CALL STORE.END.ERROR
    END

*    IF Y.WORK.BAL LT 0 THEN
*        RETURN
*    END

RETURN

*-----------------
CHECK.BALANCE.STO:
*-----------------
    CALL F.READ(FN.ACCOUNT,Y.VAR.ACC.NO,R.ACCOUNT,F.ACCOUNT,ERR)
    Y.ACCT.BAL = R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.AV.BAL>
*Y.WORK.BAL = R.ACCOUNT<AC.WORKING.BALANCE> ;*Tus Start
    CALL EB.READ.HVT('EB.CONTRACT.BALANCES',Y.VAR.ACC.NO,R.ECB,ECB.ERR)
    Y.WORK.BAL = R.ECB<ECB.WORKING.BALANCE> ;*Tus End
    IF Y.DEBIT.AMOUNT GT Y.ACCT.BAL THEN
        ETEXT = 'EB-AMT.NOT.AVAIL'
        CALL STORE.END.ERROR
    END

RETURN

*---------------------
CHECK.BALANCE.STO.DUP:
*---------------------
    CALL F.READ(FN.ACCOUNT,Y.VAR.ACC.NO,R.ACCOUNT,F.ACCOUNT,ERR)
    Y.ACCT.BAL = R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.AV.BAL>
*Y.WORK.BAL = R.ACCOUNT<AC.WORKING.BALANCE> ;*Tus Start
    CALL EB.READ.HVT('EB.CONTRACT.BALANCES',Y.VAR.ACC.NO,R.ECB,ECB.ERR)
    Y.WORK.BAL = R.ECB<ECB.WORKING.BALANCE> ;*Tus End
    IF Y.DEBIT.AMOUNT GT Y.ACCT.BAL THEN
        ETEXT = 'EB-AMT.NOT.AVAIL'
        CALL STORE.END.ERROR
    END

RETURN

*-----------------
CHECK.BALANCE.BULK:
*-----------------

    CALL F.READ(FN.ACCOUNT,Y.VAR.ACC.NO,R.ACCOUNT,F.ACCOUNT,ERR)
    Y.ACCT.BAL = R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.AV.BAL>
*Y.WORK.BAL = R.ACCOUNT<AC.WORKING.BALANCE> ;*Tus Start
    CALL EB.READ.HVT('EB.CONTRACT.BALANCES',Y.VAR.ACC.NO,R.ECB,ECB.ERR)
    Y.WORK.BAL = R.ECB<ECB.WORKING.BALANCE> ;*Tus End
    Y.CRD.AMT = R.NEW(FT.BUL76.CR.AMOUNT)
    CHANGE @VM TO @FM IN Y.CRD.AMT
    Y.TOT.CRE.AMT = SUM(Y.CRD.AMT)

    Y.CNTR = 1
    Y.CNT = DCOUNT(Y.CRD.AMT,@VM)
    LOOP
    WHILE Y.CNTR LE Y.CNT

        Y.CR.AMT = Y.CRD.AMT<1,Y.CNTR>
        IF Y.CR.AMT LT 0 THEN
            AF = FT.BUL76.CR.AMOUNT
            AV = Y.CNTR
            ETEXT = 'FT-INP.CANT.NEGATIVE'
            CALL STORE.END.ERROR
        END
        Y.CNTR += 1
    REPEAT

    IF Y.TOT.CRE.AMT GT Y.ACCT.BAL THEN
        AF = FT.BUL76.DR.ACCOUNT
        ETEXT = 'EB-AMT.NOT.AVAIL'
        CALL STORE.END.ERROR
    END
RETURN
END
*---------------------------------------------*END OF SUBROUTINE*-------------------------------------------
