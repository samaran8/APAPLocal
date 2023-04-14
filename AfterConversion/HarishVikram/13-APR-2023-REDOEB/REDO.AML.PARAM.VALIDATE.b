* @ValidationCode : MjotMTI1ODIzOTAzNDpDcDEyNTI6MTY4MTM2NzcyMjgwNjpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 12:05:22
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
$PACKAGE APAP.REDOEB
SUBROUTINE REDO.AML.PARAM.VALIDATE
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.AML.PARAM.VALIDATE
*--------------------------------------------------------------------------------------------------------
*Description       : REDO.AML.PARAM.VALIDATE is a validation routine attached to the TEMPLATE
*Linked With       : Template - REDO.AML.PARAM
*In  Parameter     : NA
*Out Parameter     : NA
*Files  Used       : REDO.AML.PARAM         As              I               Mode
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date               Who                  Reference                  Description
*   ------            ------               -------------               -------------
*  08-04-11             RIYAS            ODR-2009-10-0472          REDO.AML.PARAM Validation Routine
* 13-APR-2023     Conversion tool    R22 Auto conversion        F.READ to CACHE.READ
* 13-APR-2023      Harishvikram C   Manual R22 conversion        No changes
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CURRENCY

    $INSERT I_F.REDO.AML.PARAM

    FN.CURRENCY='F.CURRENCY'
    F.CURRENCY=''
    CALL OPF(FN.CURRENCY,F.CURRENCY)

    FN.REDO.AML.PARAM='F.REDO.AML.PARAM'
    F.REDO.AML.PARAM=''
    CALL OPF(FN.REDO.AML.PARAM,F.REDO.AML.PARAM)

    LOC.REF.APPLICATION="CURRENCY"
    LOC.REF.FIELDS='L.CU.AMLBUY.RT'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    LOC.AMLBUY.RT=LOC.REF.POS<1>
*-------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
* This is the para from where the execution of the code starts

    GOSUB PROCESS.PARA

RETURN
*--------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************
* This is the main processing para
    Y.CCY = R.NEW(AML.PARAM.AML.CCY)
    Y.LIMIT.FCY = R.NEW(AML.PARAM.AMT.LIMIT.FCY)
    Y.LIMIT.LCY = R.NEW(AML.PARAM.AMT.LIMIT.LCY)
    CALL CACHE.READ(FN.CURRENCY, Y.CCY, R.CURRENCY, CUR.ERR) ;*R22 Auto conversion
    Y.EXC.AMT=R.CURRENCY<EB.CUR.LOCAL.REF,LOC.AMLBUY.RT>
    Y.FIN.AMT=Y.LIMIT.FCY*Y.EXC.AMT
    R.NEW(AML.PARAM.AMT.LIMIT.LCY)=Y.FIN.AMT
RETURN
*--------------------------------------------------------------------------------------------------------
END       ;* End of Program
