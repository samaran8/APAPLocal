* @ValidationCode : Mjo2NzM1MjA4MjpDcDEyNTI6MTY4MDYwNzEyOTY0NDpJVFNTOi0xOi0xOjU0ODoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Apr 2023 16:48:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 548
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.S.MAPPING(P.COLL.CODE, Y.ID.COLL, Y.AV)

*
* Subroutine Type : ROUTINE
* Attached to     : ROUTINE RCA
* Attached as     : VALIDATION ROUTINE
* Primary Purpose : ON CHARGE TO POPULATED FIELDS IN TABLE PASSED AS PARAMETER
*
* Incoming:
* ---------
*
* Outgoing:
* ---------
*
*
* Error Variables:
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : mgudino - TAM Latin America
* Date            : 11 AGU 2011
*
* Modified by     : Jorge Valarezo - TAM Latin America
* Date            : 14.05.2012
* Notes           : PAC00169926 setup exception for mapping on BR
* 04-APRIL-2023      Harsha                R22 Auto Conversion  - VM to @VM and SM to @SM 
* 04-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
*----------------------------------------------------------------------------
*-----------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.REDO.CREATE.ARRANGEMENT
    $INSERT I_F.REDO.APP.MAPPING
    $INSERT I_GTS.COMMON
    $INSERT I_F.COLLATERAL
    $INSERT I_System
    $INSERT I_F.COLLATERAL.RIGHT


    GOSUB INITIALISE

    IF PROCESS.GOAHEAD THEN
        GOSUB OPEN.FILES
        GOSUB PROCESS.MAIN
    END

RETURN  ;* Program RETURN

*----------------------------------------------------------------------------------------
PROCESS.MAIN:
*==============

* Get the property name in Y.APP.NAME-> RCA
    Y.APP.NAME = APPLICATION

*  Open Mapping
    CALL F.READ(FN.REDO.APP.MAPPING, Y.COLL.CODE, R.MAPPING, F.REDO.APP.MAPPING, Y.ERR)
    IF Y.ERR THEN
* JP20110829 If there is an error => just return
        RETURN
    END

    CALL F.READ(FN.COLLATERAL,Y.ID.COLL,R.COLL,F.COLLATERAL,ERR.MSJ)
    IF ERR.MSJ THEN
        RETURN
    END

    GOSUB PROCESS.MAPPING

    GOSUB PROCESS.MAPPING.COLL.RIGHT

RETURN

*-----------------------------------------------------------------------------------------------
PROCESS.MAPPING:
*==================

    Y.FIELD.FROMS = DCOUNT(R.MAPPING<REDO.APP.FIELD.TO>, @VM)
    Y.APP.FROM = R.MAPPING<REDO.APP.APP.TO, 1>
    FOR FIELD.POS = 1 TO Y.FIELD.FROMS
        Y.LOCAL.REF = 'LOCAL.REF'
        Y.FIELD.FROM = R.MAPPING<REDO.APP.FIELD.TO, FIELD.POS>
        Y.FIELD.TO = R.MAPPING<REDO.APP.FIELD.FROM, FIELD.POS>
        Y.FIELD.NM = Y.FIELD.TO
        Y.FIELD.FROM.NM = Y.FIELD.FROM
        CALL EB.GET.APPL.FIELD(RCA.APPLICATION,Y.FIELD.TO,'',Y.APP.ERR)

        Y.LOCAL = SUBSTRINGS(Y.FIELD.FROM,1,2)

        IF Y.LOCAL EQ 'L.' THEN
            LOC.REF.APPLICATION = Y.APP.FROM
            LOC.REF.FIELDS = Y.FIELD.FROM
            CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
            Y.FIELD.FROM = LOC.REF.POS<1,1>
            CALL EB.GET.APPL.FIELD(Y.APP.FROM,Y.LOCAL.REF,'',Y.APP.ERR)

*IF NOT(R.NEW(Y.LOCAL.REF)<1,Y.FIELD.TO>) THEN
            R.NEW(Y.FIELD.TO)<1,Y.AV> = R.COLL<Y.LOCAL.REF,Y.FIELD.FROM>
*END

        END ELSE
* IF NOT(R.NEW(Y.FIELD.TO)) THEN
            CALL EB.GET.APPL.FIELD(Y.APP.FROM,Y.FIELD.FROM,'',Y.APP.ERR)
            IF  Y.FIELD.NM EQ 'ADDRESS.BR' THEN
                CHANGE @VM TO @SM IN R.COLL<Y.FIELD.FROM>
            END
            IF Y.FIELD.FROM.NM EQ 'NOTES' THEN
                CHANGE @VM TO "-" IN R.COLL<Y.FIELD.FROM>
                CHANGE @SM TO "-" IN R.COLL<Y.FIELD.FROM>
            END
            R.NEW(Y.FIELD.TO)<1,Y.AV> =  R.COLL<Y.FIELD.FROM>
*END
        END
    NEXT

RETURN

*----------------------------------------------------------------------------------------

PROCESS.MAPPING.COLL.RIGHT:
*==================

    Y.ID.COLL.RIGHT = Y.ID.COLL
    Y.POS = INDEX(Y.ID.COLL.RIGHT, ".", 2)
    Y.ID.COLL.RIGHT = SUBSTRINGS(Y.ID.COLL.RIGHT,1, Y.POS - 1 )

    LOCATE Y.ID.COLL.RIGHT IN R.NEW(REDO.FC.ID.COLLATERL.RIGHT)<1,1> SETTING POS.COLL THEN
        RETURN
    END

    CALL F.READ(FN.COLLATERAL.RIGHT,Y.ID.COLL.RIGHT,R.COLL.RIGHT,F.COLLATERAL.RIGHT,ERR.MSJ)
    IF ERR.MSJ THEN
        RETURN
    END

    IF R.COLL.RIGHT AND Y.ID.COLL.RIGHT  THEN
        Y.MAX = DCOUNT(R.NEW(REDO.FC.ID.COLLATERL.RIGHT), @VM) + 1
        R.NEW(REDO.FC.ID.COLLATERL.RIGHT)<1,Y.MAX> = Y.ID.COLL.RIGHT
        R.NEW(REDO.FC.COLL.RIGHT.CODE)<1,Y.MAX> = R.COLL.RIGHT<COLL.RIGHT.COLLATERAL.CODE>

        Y.CONT.LIM.REF = DCOUNT(R.COLL.RIGHT<COLL.RIGHT.LIMIT.REFERENCE>, @VM)
        FOR Y.LIM.REF = 1 TO Y.CONT.LIM.REF
            Y.VAL.LIM.REF = R.COLL.RIGHT<COLL.RIGHT.LIMIT.REFERENCE,Y.LIM.REF>
            GOSUB GET.LIM.REF
            R.NEW(REDO.FC.LIMIT.REFERENCE)<1,Y.MAX,Y.LIM.REF> = Y.VAL.LIM.REF
        NEXT

        R.NEW(REDO.FC.VALIDITY.DATE)<1,Y.MAX> = R.COLL.RIGHT<COLL.RIGHT.VALIDITY.DATE>
        R.NEW(REDO.FC.SEC.HOLD.IDENTIF)<1,Y.MAX> = R.NEW(REDO.FC.CUSTOMER)
    END


RETURN

*----------------------------------------------------------------------------------------
GET.LIM.REF:
*===========
    Y.VAL.LIM.REF2 = FIELD(Y.VAL.LIM.REF,'.',2)
    Y.VAL.LIM.REF3 = FIELD(Y.VAL.LIM.REF,'.',3)

    Y.VAL.LIM.REF2 = Y.VAL.LIM.REF2[4,4]

    Y.VAL.LIM.REF = Y.VAL.LIM.REF2:'.':Y.VAL.LIM.REF3

RETURN

INITIALISE:
*============

*    IF OPERATOR EQ 'MGUDINO00' THEN

*    END

*JP20110829
    IF (V$FUNCTION NE "I")  THEN
        PROCESS.GOAHEAD = 0
        RETURN
    END



    FN.REDO.APP.MAPPING = 'F.REDO.APP.MAPPING'
    F.REDO.APP.MAPPING = ''

    R.RCA = ''
    R.MAPPING = ''
    PROCESS.GOAHEAD = 1
    Y.FIELD.FROMS = ''
    Y.FIELD.FROM = ''
    Y.FIELD.TO = ''

    RCA.APPLICATION = 'REDO.CREATE.ARRANGEMENT'

    Y.COLL.CODE = 'CC-':P.COLL.CODE

    FN.COLLATERAL= "F.COLLATERAL"
    F.COLLATERAL=""

    FN.COLLATERAL.RIGHT= "F.COLLATERAL.RIGHT"
    F.COLLATERAL.RIGHT=""

RETURN

*-----------------------------------------------------------------------------------
OPEN.FILES:
*===========

    CALL OPF(FN.REDO.APP.MAPPING,F.REDO.APP.MAPPING)

    CALL OPF(FN.COLLATERAL, F.COLLATERAL)

    CALL OPF(FN.COLLATERAL.RIGHT, F.COLLATERAL.RIGHT)

RETURN

END
