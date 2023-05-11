* @ValidationCode : MjotMjA4MDQzOTUxODpVVEYtODoxNjgyNTA2MTIxNjMyOkFkbWluOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 26 Apr 2023 16:18:41
* @ValidationInfo : Encoding          : UTF-8
* @ValidationInfo : User Name         : Admin
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.CL.AUTHORISE
*-----------------------------------------------------------------------------
* Developer    : Luis Fernando Pazmino (lpazminodiaz@temenos.com)
* Date         : 15.06.2011
* Description  : Register the account entries for COLLATERAL before authorizing
*-----------------------------------------------------------------------------
* Modification History:
*
* Version   Date            Who               Reference      Description
* 1.0       11.07.2011      lpazmino          CR.180         Initial Version
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            CALL ROUTINE METHOD ADDED
*-----------------------------------------------------------------------------
* Input/Output:   NA/NA
* Dependencies: NA
*-----------------------------------------------------------------------------

* <region name="INCLUDES">
    $INSERT I_COMMON
    $INSERT I_EQUATE

    $INSERT I_F.COLLATERAL

* </region>

    GOSUB PROCESS

* <region name="PROCESS" description="Process">
RETURN

PROCESS:
    IF R.OLD(COLL.CURR.NO) EQ '' THEN
* Nueva Garantia registrada

        CALL APAP.REDOFCFI.redoFcClAccounting('NEW');* MANUAL R22 CODE CONVERSION
    END
RETURN
* </region>
END
