* @ValidationCode : MjotNzg4ODM1Nzk1OkNwMTI1MjoxNjgyNjU4ODI4Mjc1OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 28 Apr 2023 10:43:48
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
$PACKAGE APAP.TAM
*-----------------------------------------------------------------------------
* <Rating>-36</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.VAL.TFS.TXN.TYPE
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS.
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.VAL.TXN.CODE
*--------------------------------------------------------------------------------------------------------
*Description       : This is a VALIDATION routine, attached to the field TRANSACTION, the routine
*                    populates the value in NET.ENTRY
*Linked With       : Version T24.FUND.SERVICES,FCY.COLLECT & T24.FUND.SERVICES,LCY.COLLECT
*In  Parameter     : N/A
*Out Parameter     : N/A
*Files  Used       : T24.FUND.SERVICES                   As          I       Mode
*
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date               Who                  Reference                 Description
*   ------             -----               -------------              -------------
* 03 Sep 2011          Ganesh R          PACS00112724                Initial Creation
*
** 18-04-2023 R22 Auto Conversion FM, VM, SM TO @FM, @VM, @SM
** 18-04-2023 Skanda R22 Manual Conversion - No changes
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COMPANY
    $INSERT I_F.TELLER.ID
    $INSERT I_F.TELLER
    $INSERT I_F.ACCOUNT
* $INSERT USPLATFORM.BP I_F.T24.FUND.SERVICES ;* removed USPLATFORM.BP
* $INSERT USPLATFORM.BP I_F.TFS.PARAMETER ;* removed USPLATFORM.BP
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.TFS.PARAMETER
*--------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
* This is the para from where the execution of the code starts

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.TFS.PARAM = 'F.TFS.PARAMETER'
    F.TFS.PARAM  = ''
    CALL OPF(FN.TFS.PARAM,F.TFS.PARAM)

    FN.TELLER.USER = 'F.TELLER.USER'
    F.TELLER.USER  = ''
    CALL OPF(FN.TELLER.USER,F.TELLER.USER)

    GOSUB PROCESS.PARA

RETURN
*--------------------------------------------------------------------------------------------------------
**********
PROCESS.PARA:
**********

    IF AF EQ TFS.CREATE.NET.ENTRY THEN
        CALL REDO.TFS.WAIVE.CHARGE.NO
    END
    Y.GET.TXN.TYPES = R.NEW(TFS.TRANSACTION)
    Y.GET.TXN.COUNT = DCOUNT(Y.GET.TXN.TYPES,@VM)
    IF Y.GET.TXN.COUNT GT 1 THEN
        R.NEW(TFS.NET.ENTRY) = 'CREDIT'
    END
    ELSE
        R.NEW(TFS.NET.ENTRY) = 'NO'
    END
    Y.NET.ENTRY = R.NEW(TFS.NET.ENTRY)
* PACS00142988 - 20111018 - S
    IF Y.GET.TXN.COUNT LE 1 THEN
        GOSUB VAL.NOT.NETENT
    END
* PACS00142988 - 20111018 - E

    Y.ACCT.ID = R.NEW(TFS.PRIMARY.ACCOUNT)
    CALL F.READ(FN.ACCOUNT,Y.ACCT.ID,R.ACCOUNT,F.ACCOUNT,ACCT.ERR)
    Y.CURR = R.ACCOUNT<AC.CURRENCY>
    Y.SUB.CODE = R.COMPANY(EB.COM.SUB.DIVISION.CODE)
    Y.TFS.PARAM.ID = R.COMPANY(EB.COM.FINANCIAL.COM)
    CALL CACHE.READ(FN.TFS.PARAM,Y.TFS.PARAM.ID,R.TFS.PARAM,TFS.ERR)
    Y.GET.WASH.CATEG = R.TFS.PARAM<TFS.PAR.NET.ENTRY.WASHTHRU>
    CALL F.READ(FN.TELLER.USER,OPERATOR,R.TELLER.USER,F.TELLER.USER,TELL.ERR)
    TILL.LIST = R.TELLER.USER
    TILL.COUNT = DCOUNT(TILL.LIST,@VM)
    TILL.INIT = 1
    R.ACCT = ''
    LOOP
        REMOVE TILL.ID FROM TILL.LIST SETTING TIL.POS
    WHILE TILL.INIT LE TILL.COUNT
        IF NOT(R.ACCT) THEN
            WASH.TILL.ID = Y.CURR:Y.GET.WASH.CATEG:TILL.ID:Y.SUB.CODE
*            WASH.TILL.ID = Y.CURR:Y.GET.WASH.CATEG:'0009':'0017'
            CALL F.READ(FN.ACCOUNT,WASH.TILL.ID,R.ACCT,F.ACCOUNT,ACC.ERR)
        END
        TILL.INIT++
    REPEAT

    IF NOT(R.ACCT) THEN
        Y.INP.WASH.ID = Y.CURR:Y.GET.WASH.CATEG:Y.SUB.CODE
*        Y.INP.WASH.ID = Y.CURR:Y.GET.WASH.CATEG:'0009'
        CALL TT.GET.MATCH.INT.ACC(Y.INP.WASH.ID)
        Y.LEN = LEN(Y.INP.WASH.ID)
        Y.CCY = Y.INP.WASH.ID[1,3]
        R.LEN = Y.LEN-3
        Y.REMAIN.VAL = Y.INP.WASH.ID[4,R.LEN]
        IF Y.CCY NE Y.CURR THEN
            WASH.TILL.ID = Y.CURR:Y.REMAIN.VAL
        END
    END
    Y.TFS.INIT = 1
    LOOP
        REMOVE Y.TFS.TYPE FROM Y.GET.TXN.TYPES SETTING Y.TFS.POS
    WHILE Y.TFS.INIT LE Y.GET.TXN.COUNT
        IF Y.GET.TXN.COUNT GT 1 AND (Y.TFS.TYPE EQ 'CHQDEP' OR Y.TFS.TYPE EQ 'CASHDEPD')  THEN
            R.NEW(TFS.ACCOUNT.CR)<1,Y.TFS.INIT> = WASH.TILL.ID
        END
        IF Y.GET.TXN.COUNT GT 1 AND Y.TFS.TYPE EQ 'NET.ENTRY' THEN
            R.NEW(TFS.ACCOUNT.DR)<1,Y.TFS.INIT> = WASH.TILL.ID
        END
        Y.TFS.INIT++
    REPEAT
RETURN

VAL.NOT.NETENT:

    LOCATE 'NET.ENTRY' IN Y.GET.TXN.TYPES<1,1> SETTING Y.TXN.POS THEN
        AF = TFS.TRANSACTION
        ETEXT = 'EB-TFS.TXN.CANT.BE.USED.IN.DEPCHQ'
        CALL STORE.END.ERROR
    END

RETURN

END
