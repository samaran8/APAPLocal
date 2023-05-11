* @ValidationCode : Mjo1OTEwNDU1MTpDcDEyNTI6MTY4MTg5MTQ0MzA4MzozMzNzdTotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 19 Apr 2023 13:34:03
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.VVR.CALC.AMTS.LCYFCY
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: SHANKAR RAJU
* PROGRAM NAME: REDO.VVR.CALC.AMTS.LCYFCY
* PACS        : PACS00172912
*----------------------------------------------------------------------
*
*    Validation routine attached to TT-FX versions.
*
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*  DATE         WHO             REFERENCE          DESCRIPTION
*
*  02.04.2012   NAVA V.         GROUP7 TT-FX       Based on REDO.VVR.CALC.AMOUNTS
*  21.02.2013   Pradeep S       PACS00250002       Amount rounding
*
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*19/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION          FM TO @FM, VM TO @VM
*19/04/2023         SURESH           MANUAL R22 CODE CONVERSION       CALL routine format modified
*----------------------------------------------------------------------
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
*
    $INSERT I_TT.COMMON
    $INSERT I_TT.EQUATE
*
    $INSERT I_F.TELLER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.TELLER.TRANSACTION
    $INSERT I_F.FT.COMMISSION.TYPE
*
    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB CHECK.PRELIM.CONDITIONS
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END
*
RETURN
*
*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
*
    R.NEW(TT.TE.CHARGE.CUSTOMER)  = ""
    R.NEW(TT.TE.CHARGE.ACCOUNT)   = ""
    R.NEW(TT.TE.CHARGE.CATEGORY)  = ""
    R.NEW(TT.TE.CHRG.DR.TXN.CDE)  = ""
    R.NEW(TT.TE.CHRG.CR.TXN.CDE)  = ""
    R.NEW(TT.TE.CHRG.AMT.LOCAL)   = ""
    R.NEW(TT.TE.CHRG.AMT.FCCY)    = ""
    R.NEW(TT.TE.CHARGE.CODE)      = ""
    R.NEW(TT.TE.NET.AMOUNT)       = ""
    R.NEW(TT.TE.DEALER.DESK)      = "00"
*
    R.NEW(TT.TE.AMOUNT.FCY.2)<1,1>                 = Y.BASE.AMT
*
    IF R.NEW(TT.TE.ACCOUNT.1) EQ "" THEN
        R.NEW(TT.TE.AMOUNT.LOCAL.1)<1,1>           = Y.BASE.AMT
        R.NEW(TT.TE.AMOUNT.LOCAL.2)<1,1>           = Y.BASE.AMT
        CALL TT.PERFORM.DEF.PROCESSING
        CALL TT.GENERAL.LIBRARY(CALL.CALCULATE.NET.AMOUNT)
    END
*
*
*PACS00250002 - S
    Y.LCCY.AMT = R.NEW(TT.TE.AMOUNT.FCY.2)<1,1> * R.NEW(TT.TE.DEAL.RATE)
    CAL<2> = 'U'
    CALL EB.ROUND.AMOUNT(LCCY,Y.LCCY.AMT,CAL,'')
    R.NEW(TT.TE.AMOUNT.LOCAL.1)<1,1>  = Y.LCCY.AMT
*
*PACS00250002 - E
    CALL APAP.TAM.REDO.HANDLE.COMM.TAX.FIELDS ;*MANUAL R22 CODE CONVERSION
*
*
RETURN
*
*----------------------------------------------------------------------
INITIALISE:
*----------------------------------------------------------------------
*
    PROCESS.GOAHEAD = "1"
    LOOP.CNT        = 1
    MAX.LOOPS       = 1
*
    Y.BASE.AMT         = ''
*
    LOC.REF.APPLICATION = 'TELLER'
    LOC.REF.FIELDS      = 'L.TT.BASE.AMT'
    LOC.REF.FIELDS<2>   = 'L.DEBIT.AMOUNT'
    LOC.REF.FIELDS<3>   = 'L.CREDIT.AMOUNT'
    CHANGE @FM TO @VM IN LOC.REF.FIELDS
*
    LREF.POS = ''
*
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LREF.POS)
    POS.BASE.AMT     = LREF.POS<1,1>
    DEBIT.POS        = LREF.POS<1,2>
    CREDIT.POS       = LREF.POS<1,3>
*
    IF COMI NE "" THEN
        Y.BASE.AMT       = COMI
    END
*
RETURN
*
*----------------------------------------------------------------------------------------------------------
OPEN.FILES:
*~~~~~~~~~~
*
    FN.TELLER.TRANSACTION = 'F.TELLER.TRANSACTION'
    F.TELLER.TRANSACTION = ''

    FN.FT.COMMISSION.TYPE = 'F.FT.COMMISSION.TYPE'
    F.FT.COMMISSION.TYPE = ''

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    CALL OPF(FN.ACCOUNT, F.ACCOUNT);
*
RETURN
*
* ======================
CHECK.PRELIM.CONDITIONS:
* ======================
*
    LOOP
    WHILE LOOP.CNT LE MAX.LOOPS AND PROCESS.GOAHEAD DO
        BEGIN CASE

            CASE LOOP.CNT EQ 1
*
                IF MESSAGE EQ "VAL" THEN
                    PROCESS.GOAHEAD = ""
                END
*
        END CASE
        LOOP.CNT +=1
*
    REPEAT
*
RETURN
*
END
