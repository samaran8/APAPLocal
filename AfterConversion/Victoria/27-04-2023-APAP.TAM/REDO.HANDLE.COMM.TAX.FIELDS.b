* @ValidationCode : MjoxNzAxMzEzOTkwOkNwMTI1MjoxNjgxMTg4ODUwNzk4OklUU1M6LTE6LTE6MDoxOnRydWU6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 11 Apr 2023 10:24:10
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.HANDLE.COMM.TAX.FIELDS
*---------------------------------------------------------------------------
*
*    COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*    DEVELOPED BY: JOAQUIN COSTA C. - jcostac69@hotmail.com
*    PROGRAM NAME: REDO.HANDLE.COMM.TAX.FIELDS
*
*----------------------------------------------------------------------
*
*  DESCRIPTION: SERVICE subroutine that calculates and updates COMM/TAX
*               CORE and local fields, NET.AMOUNT.
*
*----------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE           WHO         REFERENCE         DESCRIPTION
*
* 27-FEB-2011   J.COSTA C.   PACS00172909      Handle different COMM/TAX scenarios
*
** 11-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 11-04-2023 Skanda R22 Manual Conversion - Line No 206
*----------------------------------------------------------------------
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_TT.EQUATE
*
    $INSERT I_F.TELLER
    $INSERT I_F.ACCOUNT
*

    GOSUB INIT
    GOSUB CHECK.PRELIM.CONDITIONS
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END
*
RETURN
*
* ======
PROCESS:
* ======
*
    R.NEW(TT.TE.WAIVE.CHARGES)    = ""    ;* Waive charge initiated after every change in order to update the accounting
    CALL TT.PERFORM.DEF.PROCESSING
*
    WSAVE.AF   = AF
    WSAVE.COMI = COMI
    AF         = TT.TE.ACCOUNT.1
    COMI       = R.NEW(TT.TE.ACCOUNT.1)
    CALL TT.GENERAL.LIBRARY(CALL.ACC.ENRI)
    AF   = WSAVE.AF
    COMI = WSAVE.COMI
*
    CALL TT.GENERAL.LIBRARY(CALL.CALCULATE.NET.AMOUNT)
*
    GOSUB UPDATE.TRAN.AMOUNT.FIELDS
*
    WCHG.CODE   = R.NEW(TT.TE.CHARGE.CODE)
    IF WCHG.CODE NE "" THEN
        GOSUB UPDATE.WAIVE.SCREEN.FIELDS
    END
*
RETURN
*
* =========================
UPDATE.WAIVE.SCREEN.FIELDS:
* =========================
*

    WCHG.AMOUNT.LOC = R.NEW(TT.TE.CHRG.AMT.LOCAL)
    WCHG.AMOUNT.FCY = R.NEW(TT.TE.CHRG.AMT.FCCY)
    WCOMM.CODE      = R.NEW(TT.TE.LOCAL.REF)<1,COMM.COD.POS>
    WTAX.CODE       = R.NEW(TT.TE.LOCAL.REF)<1,TAX.CODE.POS>
*Charge Account.1
*
    Y.CHARGE.ACCT = R.NEW(TT.TE.CHARGE.ACCOUNT)
*
    CHANGE @VM TO @FM IN WCHG.CODE
    CHANGE @VM TO @FM IN WCHG.AMOUNT.LOC
    CHANGE @VM TO @FM IN WCHG.AMOUNT.FCY
*Charge Account.1
    CHANGE @VM TO @FM IN Y.CHARGE.ACCT

    CHANGE @SM TO @FM IN WCOMM.CODE
    CHANGE @SM TO @FM IN WTAX.CODE
*
    WX = 0
    WY = 0

    LOOP
        REMOVE TR.CODE FROM WCHG.CODE SETTING TR.POS
    WHILE TR.CODE : TR.POS DO
        REMOVE WAMT.CHRG.LOC FROM WCHG.AMOUNT.LOC SETTING AM.LOC.POS
        REMOVE WAMT.CHRG.FCY FROM WCHG.AMOUNT.FCY SETTING AM.FCY.POS

* Charge Account.1
* LEER CTA Y RECUPERAR MONEDA
        REMOVE Y.ACCT.ID FROM Y.CHARGE.ACCT SETTING Y.ACCT.POS

        WAMT.CHRG = ''

        GOSUB GET.AMT
        GOSUB PROCESS.COM.TAX.FLD
    REPEAT
RETURN
********************
PROCESS.COM.TAX.FLD:
********************
    LOCATE TR.CODE IN WCOMM.CODE<1> SETTING WC.POS THEN
        R.NEW(TT.TE.LOCAL.REF)<1,COMM.AMT.POS,WC.POS> = WAMT.CHRG
        R.NEW(TT.TE.LOCAL.REF)<1,WV.COMM.POS,WC.POS> = "NO"
    END ELSE
        LOCATE TR.CODE IN WTAX.CODE<1> SETTING WT.POS THEN
            R.NEW(TT.TE.LOCAL.REF)<1,TAX.AMT.POS,WT.POS> = WAMT.CHRG
            R.NEW(TT.TE.LOCAL.REF)<1,WV.TAX.POS,WT.POS> = "NO"
        END
    END
RETURN  ;* From UPDATE.WAIVE.SCREEN.FIELDS
*
* ========================
UPDATE.TRAN.AMOUNT.FIELDS:
* ========================
*
    IF R.NEW(TT.TE.DR.CR.MARKER) EQ "DEBIT" THEN
        IF R.NEW(TT.TE.CURRENCY.1) EQ LCCY THEN
            R.NEW(TT.TE.LOCAL.REF)<1,DEBIT.POS>  = R.NEW(TT.TE.AMOUNT.LOCAL.1)
        END ELSE
            R.NEW(TT.TE.LOCAL.REF)<1,DEBIT.POS>  = R.NEW(TT.TE.AMOUNT.FCY.1)
        END
        R.NEW(TT.TE.LOCAL.REF)<1,CREDIT.POS> = R.NEW(TT.TE.NET.AMOUNT)
    END ELSE
        IF R.NEW(TT.TE.CURRENCY.2) EQ LCCY THEN
            R.NEW(TT.TE.LOCAL.REF)<1,CREDIT.POS>  = R.NEW(TT.TE.AMOUNT.LOCAL.2)
        END ELSE
            R.NEW(TT.TE.LOCAL.REF)<1,CREDIT.POS>  = R.NEW(TT.TE.AMOUNT.FCY.2)
        END
* 2012APR18 - VNL - S
        IF R.NEW(TT.TE.CURRENCY.1) EQ LCCY THEN
            R.NEW(TT.TE.LOCAL.REF)<1,CREDIT.POS>  = R.NEW(TT.TE.AMOUNT.LOCAL.1)
        END ELSE
            R.NEW(TT.TE.LOCAL.REF)<1,CREDIT.POS>  = R.NEW(TT.TE.AMOUNT.FCY.1)
        END
* 2012APR18 - VNL - E
        R.NEW(TT.TE.LOCAL.REF)<1,DEBIT.POS> = R.NEW(TT.TE.NET.AMOUNT)
    END
*
RETURN
*
*-------
GET.AMT:
*-------
*

    CALL F.READ(FN.ACCOUNT,Y.ACCT.ID,R.ACCOUNT,F.ACCOUNT,Y.ACCT.ERR)
    IF R.ACCOUNT THEN
        IF R.ACCOUNT<AC.CURRENCY> EQ LCCY THEN ;* R22 Auto conversion

            WAMT.CHRG = WAMT.CHRG.LOC
        END
        ELSE
            WAMT.CHRG =  WAMT.CHRG.FCY
        END
    END
*
RETURN  ;* From GET.AMT
*
*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------
*
    PROCESS.GOAHEAD = 1
    Y.ERR.MSG       = ""
*
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
*
    LOC.FIELDS      = 'L.TT.BASE.AMT'
    LOC.FIELDS<2>   = "L.TT.COMM.CODE"
    LOC.FIELDS<3>   = "L.TT.TAX.CODE"
    LOC.FIELDS<4>   = "L.TT.COMM.AMT"
    LOC.FIELDS<5>   = "L.TT.TAX.AMT"
    LOC.FIELDS<6>   = "L.TT.TRANS.AMT"
    LOC.FIELDS<7>   = "L.TT.WV.TAX"
    LOC.FIELDS<8>   = "L.TT.WV.COMM"
    LOC.FIELDS<9>   = "L.TT.WV.TAX.AMT"
    LOC.FIELDS<10>  = "L.DEBIT.AMOUNT"
    LOC.FIELDS<11>  = "L.CREDIT.AMOUNT"
    CHANGE @FM TO @VM IN LOC.FIELDS
*
    LOC.REF.POS     = ''
*
    CALL MULTI.GET.LOC.REF(APPLICATION,LOC.FIELDS,LREF.POS) ;
    POS.BASE.AMT     = LREF.POS<1,1>
    COMM.COD.POS     = LREF.POS<1,2>
    TAX.CODE.POS     = LREF.POS<1,3>
    COMM.AMT.POS     = LREF.POS<1,4>
    TAX.AMT.POS      = LREF.POS<1,5>
    TRANS.AMT.POS    = LREF.POS<1,6>
    WV.TAX.POS       = LREF.POS<1,7>
    WV.COMM.POS      = LREF.POS<1,8>
    WV.TAX.AMT.POS   = LREF.POS<1,9>
    DEBIT.POS        = LREF.POS<1,10>
    CREDIT.POS       = LREF.POS<1,11>
*
    WPOS.LR    = TT.TE.LOCAL.REF
*
    R.NEW(TT.TE.LOCAL.REF)<1,WV.TAX.AMT.POS> = ""
RETURN
*
*-----------------------
CHECK.PRELIM.CONDITIONS:
*-----------------------
    LOOP.CNT  = 1   ;   MAX.LOOPS = 1
*
    LOOP
    WHILE LOOP.CNT LE MAX.LOOPS AND PROCESS.GOAHEAD DO
        BEGIN CASE

            CASE LOOP.CNT EQ 1

        END CASE
*       Increase
        LOOP.CNT += 1
*
    REPEAT
*
RETURN

END
