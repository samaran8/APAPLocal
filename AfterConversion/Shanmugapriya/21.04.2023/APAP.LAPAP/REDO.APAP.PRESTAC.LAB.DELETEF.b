* @ValidationCode : MjoxMjc4MDAzMTUxOkNwMTI1MjoxNjgyMDEzNzgwNDk2OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 20 Apr 2023 23:33:00
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
$PACKAGE APAP.LAPAP
SUBROUTINE REDO.APAP.PRESTAC.LAB.DELETEF
    
*-----------------------------------------------------------------------------------------------------
* Modification History:
*
* Date             Who                   Reference      Description
* 21.04.2023       Conversion Tool       R22            Auto Conversion     - $INCLUDE TO $INSERT
* 21.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*
*------------------------------------------------------------------------------------------------------

    $INSERT I_F.DATES           ;** R22 Auto conversion - $INCLUDE TO $INSERT

    EXECUTE 'COPY FROM ../interface/FLAT.INTERFACE/TRANSPRESTALAB PAGO.PRESTACIONES.LABORALES.TXT TO ../interface/FLAT.INTERFACE/TRANSPRESTALAB/TEMP OVERWRITING DELETING'

RETURN

END
