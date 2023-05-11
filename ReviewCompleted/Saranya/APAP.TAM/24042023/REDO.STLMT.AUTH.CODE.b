* @ValidationCode : MjotMTU0NDA1MDQ4NTpDcDEyNTI6MTY4MjUxODg3NDc0MzpJVFNTOi0xOi0xOi04OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 26 Apr 2023 19:51:14
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : -8
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
*---------------------------------------------------------------------------------------
*MODIFICATION HISTORY:
*DATE           WHO                 REFERENCE               DESCRIPTION
*24-APR-2023    CONVERSION TOOL     R22 AUTO CONVERSION     NO CHANGE
*24-APR-2023    VICTORIA S          R22 MANUAL CONVERSION   CALL method format changed
*----------------------------------------------------------------------------------------
SUBROUTINE  REDO.STLMT.AUTH.CODE
*--------------------------------------------------------------------------
*Company Name      : APAP Bank
*Developed By      : Temenos Application Management
*Program Name      : REDO.STLMT.AUTH.CODE
*Date              : 23.11.2010
*-------------------------------------------------------------------------
* Incoming/Outgoing Parameters
*-------------------------------
* In  : --N/A--
* Out : --N/A--
*-----------------------------------------------------------------------------
* Revision History:
* -----------------
* Date                   Name                   Reference               Version
* -------                ----                   ----------              --------
*23/11/2010      saktharrasool@temenos.com   ODR-2010-08-0469       Initial Version
*------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.VISA.STLMT.FILE.PROCESS.COMMON
    $INSERT I_F.LATAM.CARD.ORDER
    $INSERT I_F.REDO.CARD.BIN



    IF ERROR.MESSAGE NE '' THEN
        RETURN
    END


    GOSUB INIT
RETURN

*------------------------------------------------------------------------------------
INIT:
*------------------------------------------------------------------------------------


    CALL APAP.TAM.REDO.STLMT.VAL.DELAY ;* R22 Manual conversion - CALL method format changed
    Y.FIELD.VALUE=TRIM(Y.FIELD.VALUE,' ','B')
    IF Y.FIELD.VALUE EQ '' THEN
        ERROR.MESSAGE="NO.AUTH.CODE"
    END ELSE
        AUTH.CODE=Y.FIELD.VALUE
    END
RETURN
END
