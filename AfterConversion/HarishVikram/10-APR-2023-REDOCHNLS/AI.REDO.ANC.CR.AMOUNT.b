* @ValidationCode : MjoxMDkxMDUwMTczOkNwMTI1MjoxNjgxMTA3NDU0OTI2OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Apr 2023 11:47:34
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
$PACKAGE APAP.REDOCHNLS
SUBROUTINE AI.REDO.ANC.CR.AMOUNT
*--------------------------------------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : AI.REDO.ANC.CR.AMOUNT
*--------------------------------------------------------------------------------------------------------
*Description  : This is a ANC routine to null the value if amount is 0
*In Parameter : N/A
*Out Parameter: N/A
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 10-APR-2023     Conversion tool    R22 Auto conversion       if condition added
* 10-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*--------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_System

    GOSUB PROCESS
RETURN
*--------------------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------------------------
PROCESS:
*********
    Y.CREDIT.AMT = System.getVariable('CURRENT.ARC.AMT')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto conversion - start
        Y.CREDIT.AMT = ""
    END					;*R22 Auto conversion - end
    IF Y.CREDIT.AMT EQ 0 THEN
        R.NEW(FT.CREDIT.AMOUNT) = ''
    END ELSE
        R.NEW(FT.CREDIT.AMOUNT) = Y.CREDIT.AMT
    END
RETURN
END
