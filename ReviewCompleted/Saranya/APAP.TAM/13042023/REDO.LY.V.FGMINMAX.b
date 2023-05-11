* @ValidationCode : MjoxOTkyMjE2OTIyOkNwMTI1MjoxNjgxMzc2MDk3OTkyOklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 14:24:57
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.LY.V.FGMINMAX
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: RMONDRAGON
* PROGRAM NAME: REDO.LY.V.FGMINMAX
* ODR NO      : ODR-2011-06-0243
*----------------------------------------------------------------------
*DESCRIPTION: This subroutine is performed in REDO.LY.MODALITY,CREATE version
* The functionality is to invalidate the NULL values in MAX and MIN fields
* for point generation in case GEN POINTS field is parametrized.

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH:  REDO.LY.MODALITY
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*15.07.2011  RMONDRAGON       ODR-2011-06-0243    FIRST VERSION
*23.11.2011  RMONDRAGON       ODR-2011-06-0243        UPDATE
*20.12.2012  RMONDRAGON       ODR-2011-06-0243        UPDATE
*13.04.2023  Conversion Tool       R22            Auto Conversion     - FM TO @FM
*13.04.2023  Shanmugapriya M       R22            Manual Conversion   - No changes
*
*----------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.LY.MODALITY

    IF VAL.TEXT THEN
        GOSUB PROCESS
    END

RETURN

*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
    Y.FORM.GEN = R.NEW(REDO.MOD.FORM.GENERATION)
    Y.GEN.AMT = R.NEW(REDO.MOD.GEN.AMT)

    Y.PREV.VAL = 'N'
    IF Y.GEN.AMT EQ 'Capital' OR Y.GEN.AMT EQ 'Total' THEN
        Y.PREV.VAL = 'Y'
    END

    IF Y.FORM.GEN EQ '2' AND Y.PREV.VAL EQ 'Y' THEN
        GOSUB VAL.FIELD.GEN
        GOSUB VAL.FIELD.MIN
        GOSUB VAL.FIELD.MAX
        GOSUB VAL.BOTH
        COMI = MAX.LIM.PT
    END

RETURN

*-------------
VAL.FIELD.GEN:
*-------------

    GEN.PT = R.NEW(REDO.MOD.GEN.FACTOR)
    AF = REDO.MOD.GEN.FACTOR
    IF GEN.PT EQ '' THEN
        COMI = ''
        ETEXT = 'EB-REDO.CHECK.FIELDS.F':@FM:Y.FORM.GEN
        CALL STORE.END.ERROR
    END

    VAL.IF.N = GEN.PT
    GOSUB VAL.IF.NUM

RETURN

*-------------
VAL.FIELD.MIN:
*-------------

    MIN.LIM.PT = R.NEW(REDO.MOD.MIN.GEN)
    AF = REDO.MOD.MIN.GEN
    IF GEN.PT NE '' AND MIN.LIM.PT EQ '' THEN
        COMI = ''
        ETEXT = 'EB-REDO.CHECK.FIELDS.F':@FM:Y.FORM.GEN
        CALL STORE.END.ERROR
    END

    VAL.IF.N = MIN.LIM.PT
    GOSUB VAL.IF.NUM

RETURN

*-------------
VAL.FIELD.MAX:
*-------------

    MAX.LIM.PT = R.NEW(REDO.MOD.MAX.GEN)
    AF = REDO.MOD.MAX.GEN
    IF GEN.PT NE '' AND MAX.LIM.PT EQ '' THEN
        COMI = ''
        ETEXT = 'EB-REDO.CHECK.FIELDS.F':@FM:Y.FORM.GEN
        CALL STORE.END.ERROR
    END

    VAL.IF.N = MAX.LIM.PT
    GOSUB VAL.IF.NUM

RETURN

*--------
VAL.BOTH:
*--------

    IF MAX.LIM.PT LT MIN.LIM.PT THEN
        AF = REDO.MOD.MAX.GEN
        ETEXT = 'EB-REDO.LY.V.MAX.GEN'
        CALL STORE.END.ERROR
    END

RETURN

*----------
VAL.IF.NUM:
*----------

    IF NOT(NUM(VAL.IF.N)) THEN
        ETEXT = 'EB-REDO.CHECK.FIELDS.F.NONUM'
        CALL STORE.END.ERROR
    END

RETURN

END
