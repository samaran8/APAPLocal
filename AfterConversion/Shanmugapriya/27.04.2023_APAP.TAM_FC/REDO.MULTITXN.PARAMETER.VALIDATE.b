* @ValidationCode : MjoyODIyNTIwNDk6Q3AxMjUyOjE2ODI1Mjg0NjYxMjM6SVRTUzotMTotMTowOjE6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 26 Apr 2023 22:31:06
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
* Version 2 02/06/00  GLOBUS Release No. G11.0.00 29/06/00
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE REDO.MULTITXN.PARAMETER.VALIDATE
*-----------------------------------------------------------------------------
*** Template FOR validation routines
* @author youremail@temenos.com
* @stereotype validator
* @package infra.eb
*!
*-----------------------------------------------------------------------------
*** <region name= Modification History>
*-----------------------------------------------------------------------------
* 07/06/06 - BG_100011433
*            Creation
*-----------------------------------------------------------------------------
*Modification History
*DATE                WHO                         REFERENCE                DESCRIPTION
*18-04-2023       Conversion Tool        R22 Auto Code conversion          No Changes
*18-04-2023       Samaran T               R22 Manual Code Conversion       No Changes
*-----------------------------------------------------------------------------------------
*** </region>
*** <region name= Main section>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.MULTITXN.PARAMETER

    GOSUB INITIALISE
    GOSUB PROCESS.MESSAGE
RETURN
*** </region>
*-----------------------------------------------------------------------------
VALIDATE:
* TODO - Add the validation code here
* Set AF, AV and AS to the field, multi value and sub value and invoke STORE.END.ERROR
* Set ETEXT to point to the EB.ERROR.TABLE

*      AF = MY.FIELD.NAME                 <== Name of the field
*      ETEXT = 'EB-EXAMPLE.ERROR.CODE'    <== The error code
*      CALL STORE.END.ERROR               <== Needs to be invoked per error


    AF = (RMP.ACTUAL.CATEG)
    CALL FT.NULLS.CHK
    IF END.ERROR THEN
        RETURN
    END ELSE

        AF = (RMP.ACTUAL.CATEG)
        CALL DUP
        IF END.ERROR THEN

            RETURN
        END

    END



    AF = (RMP.NEW.CATEG)
    CALL FT.NULLS.CHK
    IF END.ERROR THEN
        RETURN
*    END ELSE
    END

*        AF=(RMP.NEW.CATEG)
*        CALL DUP
*        IF END.ERROR THEN

*            RETURN
*        END
*    END

    AF = (RMP.CATEG.CASH)
    CALL FT.NULLS.CHK
    IF END.ERROR THEN
        RETURN
    END ELSE

        AF=(RMP.CATEG.CASH)
        CALL DUP
        IF END.ERROR THEN

            RETURN
        END
    END
    AF = (RMP.CATEG.CHECK)
    CALL FT.NULLS.CHK
    IF END.ERROR THEN
        RETURN
    END ELSE



        AF=(RMP.CATEG.CHECK)
        CALL DUP
        IF END.ERROR THEN

            RETURN
        END
    END

    IF R.NEW(RMP.ACTUAL.CATEG) EQ R.NEW(RMP.NEW.CATEG) THEN
        AF = RMP.NEW.CATEG
        ETEXT = 'EB-DUPLICATE.ACTUAL.AND.NEW.CATEG'
        CALL STORE.END.ERROR

    END




RETURN
*-----------------------------------------------------------------------------
*** <region name= Initialise>
INITIALISE:
***

*
RETURN
*** </region>
*-----------------------------------------------------------------------------
*** <region name= Process Message>
PROCESS.MESSAGE:
    BEGIN CASE
        CASE MESSAGE EQ ''          ;* Only during commit.
            BEGIN CASE
                CASE V$FUNCTION EQ 'D'
                    GOSUB VALIDATE.DELETE
                CASE V$FUNCTION EQ 'R'
                    GOSUB VALIDATE.REVERSE
                CASE OTHERWISE  ;* The real VALIDATE.
                    GOSUB VALIDATE
            END CASE
        CASE MESSAGE EQ 'AUT' OR MESSAGE EQ 'VER'       ;* During authorisation and verification.
            GOSUB VALIDATE.AUTHORISATION
    END CASE
*

RETURN
*** </region>
*-----------------------------------------------------------------------------
*** <region name= VALIDATE.DELETE>
VALIDATE.DELETE:
* Any special checks for deletion

RETURN
*** </region>
*-----------------------------------------------------------------------------
*** <region name= VALIDATE.REVERSE>
VALIDATE.REVERSE:
* Any special checks for reversal

RETURN
*** </region>
*-----------------------------------------------------------------------------
*** <region name= VALIDATE.AUTHORISATION>
VALIDATE.AUTHORISATION:
* Any special checks for authorisation

RETURN
*** </region>
*-----------------------------------------------------------------------------
END
