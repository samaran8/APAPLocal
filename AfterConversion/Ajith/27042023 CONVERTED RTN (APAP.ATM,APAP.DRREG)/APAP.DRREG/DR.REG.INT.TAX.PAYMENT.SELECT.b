* @ValidationCode : MjoxNTc3OTAwNzUyOkNwMTI1MjoxNjgwNjc3ODg4ODExOmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 05 Apr 2023 12:28:08
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.DRREG
SUBROUTINE DR.REG.INT.TAX.PAYMENT.SELECT
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   : gangadhar@temenos.com
* Program Name   : DR.REG.INT.TAX.PAYMENT
* Date           : 16-May-2013
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the Impuesto al Pago de Intereses
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date          Author              Modification Description
*
* 01-Aug-2014     V.P.Ashokkumar      PACS00305231 - Modified the select statement
*DATE               WHO                       REFERENCE                 DESCRIPTION
*05-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   NO CHANGE
*05-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE

*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_DR.REG.INT.TAX.PAYMENT.COMMON
    $INSERT I_DR.REG.INT.TAX.COMMON
    $INSERT I_F.DR.REG.INT.TAX.PAY.PARAM
    $INSERT I_F.DATES

    IF NOT(CONTROL.LIST) THEN
        GOSUB BUILD.CONTROL.LIST
    END
    GOSUB GET.SELECT.VAL
    GOSUB SEL.PROCESS
RETURN

BUILD.CONTROL.LIST:
*******************

    CALL EB.CLEAR.FILE(FN.DR.REG.INT.TAX.PAYMENT.WORKFILE,F.DR.REG.INT.TAX.PAYMENT.WORKFILE)
    CONTROL.LIST<-1> = "ACCOUNT.DETAILS"

RETURN

GET.SELECT.VAL:
**************
    PARAM.VAL3 = ''
    IF NOT(ST.DATE) AND NOT(ED.DATE) THEN
        PARAM.VAL3 = "INT.POST.DATE EQ ":YLWORK.DAY:" AND WITH TOTAL.INTEREST GT 0"
    END ELSE
        PARAM.VAL3 = "INT.POST.DATE GE ":ST.DATE:" AND WITH INT.POST.DATE LE ":ED.DATE:" AND WITH TOTAL.INTEREST GT 0"
    END
RETURN

SEL.PROCESS:
************
    LIST.PARAMETER = ""

    BEGIN CASE
        CASE CONTROL.LIST<1,1> EQ "ACCOUNT.DETAILS"
            LIST.PARAMETER<2> = "F.STMT.ACCT.CR"
            LIST.PARAMETER<3> = PARAM.VAL3
            CALL BATCH.BUILD.LIST(LIST.PARAMETER, "")
        CASE 1
            DUMMY.LIST = ""
            CALL BATCH.BUILD.LIST("",DUMM.LIST)
    END CASE

RETURN
END
