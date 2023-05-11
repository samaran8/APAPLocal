* @ValidationCode : MjotMTQzMjI1MjMyOkNwMTI1MjoxNjgyNTkxNTY5Mzk2OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 27 Apr 2023 16:02:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOBATCH
* Version 2 02/06/00  GLOBUS Release No. G11.0.00 29/06/00
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE REDO.BCR.REPORT.EXEC.VALIDATE
*-----------------------------------------------------------------------------
*** Template FOR REDO.BCR.REPORT.EXEC.VALIDATE
* @author hpasquel@temenos.com
* @stereotype validator
* @package infra.eb
*!
*-----------------------------------------------------------------------------
*** <region name= Modification History>
*-----------------------------------------------------------------------------
* 07/06/06 - BG_100011433
*            Creation
* Date                  who                   Reference
* 17-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION - ! TO *
* 17-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -CALL RTN METHOD ADDED
*-----------------------------------------------------------------------------
*** </region>
*** <region name= Main section>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.BCR.REPORT.EXEC
    $USING APAP.TAM
 

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


* CALL APAP.TAM.REDO.R.INTERFACE.PARAM.VALIDATE("REDO.BCR.REPORT.EXEC") ;*MANUAL R22 CODE CONVERSION
    CALL APAP.TAM.redoRInterfaceParamValidate("REDO.BCR.REPORT.EXEC") ;*MANUAL R22 CODE CONVERSION

    IF ETEXT NE "" THEN
        T.TEXT = ETEXT
        E = ETEXT       ;* Stop process
        CALL STORE.END.ERROR
        RETURN
    END

    E=''
    IF E NE '' THEN
        RETURN
    END

    R.NEW(REDO.BCR.REP.EXE.DIR.PATH) = CHANGE(R.NEW(REDO.BCR.REP.EXE.DIR.PATH),"\","")

    IF R.NEW(REDO.BCR.REP.EXE.RUN.PROCESS) EQ 'SI' THEN
* << TSA.SERVICE REDO.BCR.REPORT.GEN must be stopped to allow execute a new action
* CALL APAP.TAM.REDO.R.BCR.REPORT.CHECK.TSA(Y.RESPONSE) ;*MANUAL R22 CODE CONVERSION
        CALL APAP.TAM.redoRBcrReportCheckTsa(Y.RESPONSE) ;*MANUAL R22 CODE CONVERSION
        IF Y.RESPONSE EQ '1' THEN
            E = 'ST-REDO.BCR.EXECUTION.SERVICE'
        END
        IF E NE '' THEN ;* May be, the record does not exist into TSA.SERVICE
            RETURN
        END
* >>
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
        CASE MESSAGE EQ ''          ;* Only during commit
            BEGIN CASE
                CASE V$FUNCTION EQ 'D'
                    GOSUB VALIDATE.DELETE
                CASE V$FUNCTION EQ 'R'
                    GOSUB VALIDATE.REVERSE
                CASE OTHERWISE  ;* The real VALIDATE
                    GOSUB VALIDATE
            END CASE
        CASE MESSAGE EQ 'AUT' OR MESSAGE EQ 'VER'       ;* During authorisation and verification
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
