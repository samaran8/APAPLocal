* @ValidationCode : MjotMjEwODY3NTg4NDpDcDEyNTI6MTY4MTcwOTYyMzQ2ODpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 11:03:43
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
$PACKAGE APAP.REDOBATCH
* Version 9 16/05/01  GLOBUS Release No. 200511 31/10/05
*-----------------------------------------------------------------------------
* <Rating>-54</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.BCR.REPORT.EXEC.RECORD
*-----------------------------------------------------------------------------
* APPLICATION ROUTINE. Allows:
*            - Validate if the user can apply the function
*            - When the record is created, the system get the parameters from REDO.INTERFACE.PARAM as default values
*            - On Browser, to manage the logic for the HOT.FIELD - GET.INT.PARAM
*            - On Browser, GET.INT.PARAM, allows to reload the parameters from REDO.INTERFACE.PARAM and overwrite
*              the current values (please check routine : REDO.R.BCR.REP.COPY.PARAM)
*
* @author hpasquel@temenos.com
* @stereotype recordcheck
* @package REDO.BCR
* @uses E
* @uses AF
* @uses V$FUNCTION
* @uses OFS$BROWSER     1 if we are running on Browser
* @uses R.NEW
* @uses OFS$HOT.FIELD   in case HOT.FIELD allows to get the list of fields those are marked as HOT.FIELD
* @uses R.GTS   in case HOT.FIELD allows to get the current values (what the user has changed)
*!
*-------------------------------------------------------------------------------------
*Modification
* Date                  who                   Reference              
* 17-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION = TO EQ AND ++ TO += 1 AND FM TO @FM
* 17-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*-------------------------------------------------------------------------------------
*----------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.BCR.REPORT.EXEC

* Check if the record is okay to input to
    GOSUB CHECK.RECORD
    IF E EQ '' THEN
        GOSUB SET.ENRICHMENTS
        GOSUB POPULATE.COMMONS
    END

RETURN
*-----------------------------------------------------------------------
POPULATE.COMMONS:
*--------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
SET.ENRICHMENTS:
*--------------------------------------------------------------------
    CALL EB.SET.FIELD.ENRICHMENT(FIELD.NUMBER, FIELD.ENRICHMENT)
RETURN
*--------------------------------------------------------------------
CHECK.RECORD:
*--------------------------------------------------------------------
    BEGIN CASE

        CASE V$FUNCTION MATCHES 'V'
            E = 'EB-NO.FUNCTION'
            RETURN
* I Function <<
        CASE V$FUNCTION MATCHES 'I'
            GOSUB CHECK.I.FUNCTION

* R.NEW(REDO.BCR.REP.EXE.CURR.NO) will be used as flag
            IF R.NEW(REDO.BCR.REP.EXE.CURR.NO) EQ '' THEN
                R.NEW(REDO.BCR.REP.EXE.CURR.NO) = 1
            END
* >>
    END CASE

RETURN
*-----------------------------------------------------------------------------
CHECK.I.FUNCTION:
*-----------------------------------------------------------------------------
    IF NOT(OFS$BROWSER) AND R.NEW(REDO.BCR.REP.EXE.CURR.NO) EQ '' THEN
        CALL REDO.R.BCR.REP.COPY.PARAM(ID.NEW)
        TEXT = "Paratemers were gotten from REDO.INTERFACE.PARAM"
        CALL OVE
        RETURN
    END

    IF OFS$BROWSER EQ 1 THEN
* The record is being created
        IF OFS$GETRECORD EQ 1 AND R.NEW(REDO.BCR.REP.EXE.CURR.NO) EQ '' THEN
            CALL REDO.R.BCR.REP.COPY.PARAM(ID.NEW)
        END ELSE
            GOSUB RELOAD.PARAM
        END
    END
RETURN
*-----------------------------------------------------------------------------
RELOAD.PARAM:
*-----------------------------------------------------------------------------
    IF FIELD(OFS$HOT.FIELD,'.', 2, 99) EQ "GET.INT.PARAM" THEN
        ySearchPos = "" : REDO.BCR.REP.EXE.GET.INT.PARAM
        yTotFields = DCOUNT(R.GTS, @FM)
        ySearchPos = 11
        yPos = 1
* Delete all changed, because they are going to be replaced from the parameters
        LOOP
        WHILE DCOUNT(R.GTS, @FM) GT 1
            IF R.GTS<yPos,3> NE ySearchPos THEN
                DEL R.GTS<yPos>
            END ELSE
                yPos += 1
            END
        REPEAT

        IF R.GTS<1,3> NE ySearchPos THEN
            E = "Environemt problem, HOT.FIELD is GET.INT.PARAM but was not found o R.GTS"
            RETURN
        END
*
        getIntParam = R.GTS<1,2>
        IF getIntParam EQ 'SI' THEN
            CALL REDO.R.BCR.REP.COPY.PARAM(ID.NEW)
            R.GTS<1,2> = 'NO'
            TEXT = "Paratemers were gotten from REDO.INTERFACE.PARAM"
            CALL OVE
        END
    END
RETURN
*-----------------------------------------------------------------------------
END
