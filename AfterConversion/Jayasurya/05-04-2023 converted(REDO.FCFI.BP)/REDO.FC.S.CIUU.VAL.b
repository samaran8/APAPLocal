* @ValidationCode : MjoxMTg3MDA0MDQ0OkNwMTI1MjoxNjgwNjc3OTQxNzUwOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 12:29:01
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.S.CIUU.VAL

* Subroutine Type : ROUTINE
* Attached to     : REDO.CREATE.ARRANGEMENT validate
* Attached as     : HOT FIELD
* Primary Purpose : If the value from REDO.FC.DEST.LOAN then REDO.FC.CIUU.CATEG has to deleted
*
* Incoming:
* ---------
*
*
* Outgoing:
* ---------
*
*
* Error Variables:
*
*-----------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Jorge Valarezo - RTAM
* Date            : 22 Jun 2011
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM SM TO @SM and I++ to I=+1
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.CREATE.ARRANGEMENT
    $INSERT I_GTS.COMMON

    GOSUB INITIALISE
    GOSUB OPEN.FILES

    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS.MAIN
    END

RETURN  ;* Program RETURN
*-----------------------------------------------------------------------------------
PROCESS.MAIN:
*============


    R.NEW (REDO.FC.CIUU.CATEG) = ""
    LOOP
    WHILE Y.FIELD.POS GE 1

        IF R.GTS<Y.FIELD.POS,3> EQ Y.FIELD.SEARCH THEN
            DEL R.GTS<Y.FIELD.POS>
            Y.FIELD.POS = -1
        END
        ELSE
            Y.FIELD.POS -= 1
        END

    REPEAT

RETURN
*------------------------
INITIALISE:
*=========



    PROCESS.GOAHEAD = 1

    CAMPO.ACTUAL = OFS$HOT.FIELD
    NOMBRE.CAMPO = 'DEST.LOAN'
    IF CAMPO.ACTUAL NE NOMBRE.CAMPO THEN
        PROCESS.GOAHEAD = ""
    END


    Y.FIELD.POS = DCOUNT ( R.GTS,@FM )

    Y.FIELD.SEARCH = "" : REDO.FC.CIUU.CATEG

RETURN


*------------------------
OPEN.FILES:
*=========

RETURN
*------------
END
