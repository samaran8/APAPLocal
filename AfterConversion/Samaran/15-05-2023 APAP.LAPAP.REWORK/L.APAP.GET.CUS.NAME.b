* @ValidationCode : MjotMTUxMDM3NjMxMTpDcDEyNTI6MTY4NDE0MjQ4MjA2OTpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 15 May 2023 14:51:22
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
$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.GET.CUS.NAME(Y.INP.DEAL)
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE
* 21-APRIL-2023      Conversion Tool       R22 Auto Conversion - Include to Insert
* 21-APRIL-2023      Harsha                R22 Manual Conversion - CALL ROUTINE FORMAT MODIFIED
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System
    $USING APAP.REDORETAIL

*--PARA ABRIR EL ACHIVO FBNK.CUSTOMER
*CALL REDO.CUST.IDENTITY.REF(Y.INP.DEAL, Y.ALT.ID, Y.CUS.NAME)
    CALL APAP.REDORETAIL.redoCustIdentityRef(Y.INP.DEAL, Y.ALT.ID, Y.CUS.NAME)   ;*R22 MANUAL CODE CONVERSION
    

    Y.NAME.1         = Y.CUS.NAME[1,35]
    Y.NAME.2         = Y.CUS.NAME[36,LEN(Y.CUS.NAME)]

    Y.INP.DEAL = Y.NAME.1 : " " : Y.NAME.2

RETURN

END
