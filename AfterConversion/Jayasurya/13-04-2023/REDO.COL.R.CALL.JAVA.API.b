* @ValidationCode : MjoxMzUyMTQ1MTgyOkNwMTI1MjoxNjgxMzAzNDc4MTMyOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 18:14:38
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*12-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION               FM TO @FM
*12-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
SUBROUTINE REDO.COL.R.CALL.JAVA.API(Y.EB.API.ID, Y.REQUEST, Y.RESPONSE)
*-----------------------------------------------------------------------------
* Call API on JAVA
* @author youremail@temenos.com
* @stereotype subroutine
* @package infra.eb
*
* Parameters:
*                  Y.EB.API.ID (in)  Id from EB.API that represents the api to be call
*                  Y.REQUEST   (in)  parameters to deliver to the java api
*                  Y.REPONSE   (out) blank then everything ok, otherwise an error occurs
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------

    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
    Y.CALLJ.ERROR = ''
    ETEXT = ""
    CALL EB.CALL.JAVA.API(Y.EB.API.ID, Y.REQUEST, Y.RESPONSE, Y.CALLJ.ERROR)
    IF ETEXT NE "" THEN
        Y.RESPONSE = ETEXT
    END
    GOSUB HANDLE.FAILURE

RETURN
*-----------------------------------------------------------------------------
HANDLE.FAILURE:
*-----------------------------------------------------------------------------
* Return the CALLJ error (if any)

    IF (Y.CALLJ.ERROR NE '') THEN
        Y.RESPONSE = 'Error in invoking Java interface - CALLJ error code :' : Y.CALLJ.ERROR
    END ELSE
        IF (Y.RESPONSE NE "") THEN
            Y.STATUS = CHANGE(Y.RESPONSE, '|', @FM)
            IF Y.STATUS<1> EQ '0' THEN
                Y.RESPONSE = ""       ;* EVERY THING OK
            END
        END
    END

RETURN


RETURN

END
