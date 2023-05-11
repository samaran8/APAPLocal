* @ValidationCode : MjotNTc3ODU5MzA6Q3AxMjUyOjE2ODMxMDk1MzA4NzM6SVRTUzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 03 May 2023 15:55:30
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
SUBROUTINE REDO.CCRG.ALLOWED.APP.VAL.RTN
*-----------------------------------------------------------------------------
*!** Simple SUBROUTINE template
* @author:    anoriega@temenos.com
* @stereotype subroutine: Validate Routine
* @package:   REDO.CCRG
*!
*-----------------------------------------------------------------------------
*  This routine validate the application to add will be only CUSTOMER
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* Modification History:
*
* Date             Who                   Reference      Description
* 05.04.2023       Conversion Tool       R22            Auto Conversion     - No changes
* 05.04.2023       Shanmugapriya M       R22            Manual Conversion   - Add call routine prefix
*
*------------------------------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.CCRG.RISK.LIMIT.PARAM
*-----------------------------------------------------------------------------

    GOSUB PROCESS
RETURN

*-----------------------------------------------------------------------------
PROCESS:
* Validate application value inputted
    Y.APPLICATION = COMI
* CALL APAP.TAM.REDO.CCRG.ALLOWED.APP('VAL.RTN',Y.APPLICATION) ;** R22 Manual conversion - CALL method format changed
    CALL APAP.TAM.redoCcrgAllowedApp('VAL.RTN',Y.APPLICATION)
RETURN
END
