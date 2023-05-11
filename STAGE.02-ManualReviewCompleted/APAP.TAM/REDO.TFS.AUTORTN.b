* @ValidationCode : MjotMTIxODUxMTM2MzpDcDEyNTI6MTY4MjY4MTkxOTY2MDpJVFNTOi0xOi0xOjA6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 28 Apr 2023 17:08:39
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM

SUBROUTINE REDO.TFS.AUTORTN
    
*-----------------------------------------------------------------------------------------------------
* Modification History:
*
* Date             Who                   Reference      Description
* 28.04.2023       Conversion Tool       R22            Auto Conversion     - No changes
* 28.04.2023       Shanmugapriya M       R22            Manual Conversion   - FM TO @FM, VM TO @VM
*
*------------------------------------------------------------------------------------------------------
    

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_System
    $INSERT I_F.REDO.TFS.PROCESS
    $INSERT I_F.REDO.AZ.FUND.PARAM
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.T24.FUND.SERVICES

    GOSUB INIT
    GOSUB PROCESS
RETURN

*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------


    FN.REDO.TFS.PROCESS = 'F.REDO.TFS.PROCESS'
    F.REDO.TFS.PROCESS = ''
    CALL OPF(FN.REDO.TFS.PROCESS,F.REDO.TFS.PROCESS)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.REDO.AZ.FUND.PARAM = 'F.REDO.AZ.FUND.PARAM'

    LOC.REF.APPLICATION="T24.FUND.SERVICES"
    LOC.REF.FIELDS='L.TT.PROCESS':@VM:'L.NO.OF.INSTAL':@VM:'L.FT.ADD.INFO'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.TT.PROCESS=LOC.REF.POS<1,1>
    POS.L.NO.OF.INSTAL = LOC.REF.POS<1,2>
    POS.L.FT.ADD.INFO = LOC.REF.POS<1,3>

    VAR.ID = 'SYSTEM'
    CALL CACHE.READ(FN.REDO.AZ.FUND.PARAM,VAR.ID,R.FUND.PARAM,PARAM.ERR)

RETURN

*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
    Y.DATA = ""
    Y.REDO.TFS.PROCESS.ID = FIELD(System.getVariable("CURRENT.ID"),"*",1)
    CALL F.READ(FN.REDO.TFS.PROCESS,Y.REDO.TFS.PROCESS.ID,R.REDO.TFS.PROCESS,F.REDO.TFS.PROCESS,PRO.ERR)
    Y.PRIMARY.ACCOUNT = R.REDO.TFS.PROCESS<TFS.PRO.PRIMARY.ACCT>
    CALL F.READ(FN.AZ.ACCOUNT,Y.PRIMARY.ACCOUNT,R.AZ.ACCOUNT,F.AZ.ACCOUNT,AZ.ERR)
    IF R.AZ.ACCOUNT THEN
        VAR.CURRENCY = R.AZ.ACCOUNT<AZ.CURRENCY>
        FUND.CURRENCY = R.FUND.PARAM<REDO.FUND.CURRENCY>
        CHANGE @VM TO @FM IN FUND.CURRENCY
        LOCATE VAR.CURRENCY IN FUND.CURRENCY SETTING CUR.FUND THEN
            VAR.PRIMARY.ACCOUNT = R.FUND.PARAM<REDO.FUND.ACCT.NUMBER,CUR.FUND>
        END
    END
    R.NEW(TFS.PRIMARY.ACCOUNT)<1,1> = VAR.PRIMARY.ACCOUNT
    Y.CNT=DCOUNT(R.REDO.TFS.PROCESS<TFS.PRO.TRANSACTION>,@VM)
    Y.COUNT=1
    LOOP
    WHILE Y.COUNT LE Y.CNT
        GOSUB CHECK.TXN.PROCESS
        BEGIN CASE
            CASE Y.TXN EQ 'CASHDEP' OR Y.TXN EQ 'CASHDEPOSIT' OR Y.TXN EQ 'FCASHDEP' OR Y.TXN EQ 'NET.ENTRY' OR Y.TXN EQ 'CHQDEPOTH' OR Y.TXN EQ 'CHQDEP' OR Y.TXN EQ 'FCHQDEP'
                R.NEW(TFS.SURROGATE.AC)<1,Y.COUNT> = VAR.PRIMARY.ACCOUNT
                R.NEW(TFS.ACCOUNT.DR)<1,Y.COUNT> = R.REDO.TFS.PROCESS<TFS.PRO.ACCOUNT,Y.COUNT>
                R.NEW(TFS.ACCOUNT.CR)<1,Y.COUNT> = VAR.PRIMARY.ACCOUNT
            CASE Y.TXN EQ 'CASHWD' OR Y.TXN EQ 'CASHWITHDRAW' OR Y.TXN EQ 'FCASHWD'
                R.NEW(TFS.SURROGATE.AC)<1,Y.COUNT> = VAR.PRIMARY.ACCOUNT
                R.NEW(TFS.ACCOUNT.CR)<1,Y.COUNT> = R.REDO.TFS.PROCESS<TFS.PRO.ACCOUNT,Y.COUNT>
                R.NEW(TFS.ACCOUNT.DR)<1,Y.COUNT> = VAR.PRIMARY.ACCOUNT
            CASE Y.TXN EQ 'FROM'
                R.NEW(TFS.SURROGATE.AC)<1,Y.COUNT> = R.REDO.TFS.PROCESS<TFS.PRO.ACCOUNT,Y.COUNT>
                R.NEW(TFS.ACCOUNT.CR)<1,Y.COUNT> = VAR.PRIMARY.ACCOUNT
                R.NEW(TFS.ACCOUNT.DR)<1,Y.COUNT> = R.REDO.TFS.PROCESS<TFS.PRO.ACCOUNT,Y.COUNT>
            CASE Y.TXN EQ 'TO'
                R.NEW(TFS.SURROGATE.AC)<1,Y.COUNT> = R.REDO.TFS.PROCESS<TFS.PRO.ACCOUNT,Y.COUNT>
                R.NEW(TFS.ACCOUNT.DR)<1,Y.COUNT> = VAR.PRIMARY.ACCOUNT
                R.NEW(TFS.ACCOUNT.CR)<1,Y.COUNT> = R.REDO.TFS.PROCESS<TFS.PRO.ACCOUNT,Y.COUNT>
        END CASE
        R.NEW(TFS.NARRATIVE)<1,Y.COUNT>=R.REDO.TFS.PROCESS<TFS.PRO.CONCEPT,Y.COUNT>
        GOSUB CHECK.AMOUNT
        Y.COUNT++
    REPEAT
    R.NEW(TFS.LOCAL.REF)<1,POS.L.TT.PROCESS>  = Y.REDO.TFS.PROCESS.ID
    R.NEW(TFS.LOCAL.REF)<1,POS.L.FT.ADD.INFO> = Y.PRIMARY.ACCOUNT
RETURN
*--------------------------------------------------------------------------------------------------
CHECK.TXN.PROCESS:
*--------------------------------------------------------------------------------------------------
    Y.TXN = R.REDO.TFS.PROCESS<TFS.PRO.TRANSACTION,Y.COUNT>
    Y.CCY = R.REDO.TFS.PROCESS<TFS.PRO.CURRENCY,Y.COUNT>
    R.NEW(TFS.CURRENCY)<1,Y.COUNT> = Y.CCY

    BEGIN CASE

        CASE Y.TXN EQ 'CASHDEPOSIT'
            IF Y.CCY EQ LCCY THEN
                R.NEW(TFS.TRANSACTION)<1,Y.COUNT> = 'CASHDEP'
            END ELSE
                R.NEW(TFS.TRANSACTION)<1,Y.COUNT> = 'FCASHAZ'
            END

        CASE Y.TXN EQ 'CHQDEP'
            IF Y.CCY EQ LCCY THEN
                R.NEW(TFS.TRANSACTION)<1,Y.COUNT> = 'CHQDEP'
            END ELSE
                R.NEW(TFS.TRANSACTION)<1,Y.COUNT> = 'FCHQDEP'
            END

        CASE Y.TXN EQ 'FROM'
            R.NEW(TFS.TRANSACTION)<1,Y.COUNT> = 'FROM'

    END CASE

RETURN
*---------------------------------------------------------------------------------------------------
CHECK.AMOUNT:
*--------------------------------------------------------------------------------------------------
    R.NEW(TFS.AMOUNT.LCY)<1,Y.COUNT>=R.REDO.TFS.PROCESS<TFS.PRO.AMOUNT,Y.COUNT>
    IF Y.TXN NE 'CASHDEPOSIT' THEN
        R.NEW(TFS.AMOUNT)<1,Y.COUNT> = R.REDO.TFS.PROCESS<TFS.PRO.AMOUNT,Y.COUNT>
    END ELSE
        R.NEW(TFS.LOCAL.REF)<1,POS.L.NO.OF.INSTAL> = TRIMB(FMT(R.REDO.TFS.PROCESS<TFS.PRO.AMOUNT,Y.COUNT>,'L2,#15'))
*        R.NEW(TFS.ACTUAL.DEPOSIT) = R.REDO.TFS.PROCESS<TFS.PRO.AMOUNT,Y.COUNT> ;* Fix for PACS00347212
    END
RETURN
*--------------------------------------------------------------------------------------------------
END
