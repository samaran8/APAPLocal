* @ValidationCode : MjoyMDU3Njk4NDQ2OkNwMTI1MjoxNjgxMzc2MDk4MDM4OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 14:24:58
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
SUBROUTINE  REDO.LY.V.GRPPRD
*----------------------------------------------------------------------------------------------------
*DESCRIPTION : This routine is used to validate the amount for generation to be used
*              based on the product group in the REDO.LY.MODALITY table fields
*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : RMONDRAGON
* PROGRAM NAME : REDO.LY.V.GRPPRD
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO                REFERENCE         DESCRIPTION
*28.07.2011    RMONDRAGON         ODR-2011-06-0243     FIRST VERSION
*18.11.2011    RMONDRAGON         ODR-2011-06-0243        UPDATE
*20.07.2012    RMONDRAGON         ODR-2011-06-0243        UPDATE
*13.04.2023    Conversion Tool       R22                Auto Conversion     - FM TO @FM
*13.04.2023    Shanmugapriya M       R22                Manual Conversion   - No changes
*
* -----------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.LY.MODALITY
    $INSERT I_F.REDO.LY.PDT.TYPE
    $INSERT I_GTS.COMMON

    GOSUB INIT

    Y.TYPE = R.NEW(REDO.MOD.TYPE)
    Y.FORM.GEN = R.NEW(REDO.MOD.FORM.GENERATION)

    IF VAL.TEXT THEN
        Y.PROD.GRP = R.NEW(REDO.MOD.PRODUCT.GROUP)
        GOSUB VAL.PROD.GRP
    END ELSE
        Y.PROD.GRP = COMI
    END

    GOSUB PROCESS

RETURN

*---
INIT:
*---

    FN.REDO.LY.PDT.TYPE='F.REDO.LY.PDT.TYPE'
    F.REDO.LY.PDT.TYPE=''
    CALL OPF(FN.REDO.LY.PDT.TYPE,F.REDO.LY.PDT.TYPE)

RETURN

*-------
PROCESS:
*-------

    R.REDO.LY.PDT.TYPE = '' ; PDT.TYPE.ERR = ''
    CALL F.READ(FN.REDO.LY.PDT.TYPE,Y.PROD.GRP,R.REDO.LY.PDT.TYPE,F.REDO.LY.PDT.TYPE,PDT.TYPE.ERR)
    IF R.REDO.LY.PDT.TYPE THEN
        VAR.PROD.TYPE = R.REDO.LY.PDT.TYPE<REDO.PDT.PRODUCT.TYPE>
    END

    GOSUB VAL.PROD.GRP2

    Y.NON.AMT = 'N'
    IF Y.TYPE EQ '6' OR Y.TYPE EQ '7' THEN
        Y.EVENT = R.NEW(REDO.MOD.EVENT)
        IF (Y.TYPE EQ '6' AND (Y.EVENT EQ '4' OR Y.EVENT EQ '5')) OR (Y.TYPE EQ '6' AND VAR.PROD.TYPE NE 'Prestamo') THEN
            T(REDO.MOD.MIN.DISBURSE.AMT)<3> = 'NOINPUT'
        END
        Y.NON.AMT = 'Y'
    END

    IF (VAR.PROD.TYPE EQ 'Otro' OR VAR.PROD.TYPE EQ 'T.Debito' OR VAR.PROD.TYPE EQ 'Ahorro/Certificado') AND Y.NON.AMT EQ 'N' THEN
        T(REDO.MOD.MIN.DISBURSE.AMT)<3> = 'NOINPUT'
        R.NEW(REDO.MOD.GEN.AMT) = 'Total'
        T(REDO.MOD.GEN.AMT)<3> = 'NOINPUT'
    END

    GOSUB PROCESS1

RETURN

*--------
PROCESS1:
*--------

    IF Y.TYPE EQ '1' AND Y.FORM.GEN EQ '1' THEN
        GOSUB DIS.FIELDS.GP4
        GOSUB DIS.FIELDS.GP1
    END

    IF Y.TYPE EQ '1' AND Y.FORM.GEN EQ '2' THEN
        GOSUB DIS.FIELDS.GP4
        GOSUB DIS.FIELDS.GP2
    END

    IF Y.TYPE EQ '2' OR Y.TYPE EQ '3' OR Y.TYPE EQ '4' OR Y.TYPE EQ '5' AND Y.FORM.GEN EQ '1' THEN
        GOSUB DIS.FIELDS.GP5
        GOSUB DIS.FIELDS.GP1
    END

    IF Y.TYPE EQ '2' OR Y.TYPE EQ '3' OR Y.TYPE EQ '4' OR Y.TYPE EQ '5' AND Y.FORM.GEN EQ '2' THEN
        GOSUB DIS.FIELDS.GP5
        GOSUB DIS.FIELDS.GP2
    END

    IF Y.TYPE EQ '8' AND Y.FORM.GEN EQ '1' THEN
        GOSUB DIS.FIELDS.GP3
        T(REDO.MOD.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.MAX.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.INT.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.MAX.GEN)<3>='NOINPUT'
    END

    IF Y.TYPE EQ '6' THEN
        T(REDO.MOD.EX.PROD.AS)<3> = 'NOINPUT'
        T(REDO.MOD.CHANNEL)<3> = 'NOINPUT'
        T(REDO.MOD.APP.TXN)<3> = 'NOINPUT'
        T(REDO.MOD.TXN.CODE)<3> = 'NOINPUT'
*        T(REDO.MOD.CURRENCY)<3> = 'NOINPUT'
        T(REDO.MOD.FORM.GENERATION)<3> = 'NOINPUT'
*        T(REDO.MOD.MIN.DISBURSE.AMT)<3> = 'NOINPUT'
        T(REDO.MOD.GEN.AMT)<3> = 'NOINPUT'
        T(REDO.MOD.LOW.LIM.AMT)<3> = 'NOINPUT'
        T(REDO.MOD.UP.LIM.AMT)<3> = 'NOINPUT'
        T(REDO.MOD.INT.GEN.POINTS)<3>='NOINPUT'
        T(REDO.MOD.INT.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.UP.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.MAX.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.INT.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.MAX.GEN)<3>='NOINPUT'

        Y.EVENT = R.NEW(REDO.MOD.EVENT)
        IF Y.EVENT NE '5' THEN
            T(REDO.MOD.ANTIG)<3> = 'NOINPUT'
        END

    END

    IF Y.TYPE EQ '7' THEN
        T(REDO.MOD.EVENT)<3>='NOINPUT'
        T(REDO.MOD.CHANNEL)<3>='NOINPUT'
        T(REDO.MOD.APP.TXN)<3>='NOINPUT'
        T(REDO.MOD.TXN.CODE)<3>='NOINPUT'
        T(REDO.MOD.FORM.GENERATION)<3>='NOINPUT'
        T(REDO.MOD.ANTIG)<3>='NOINPUT'
        T(REDO.MOD.MIN.DISBURSE.AMT)<3>='NOINPUT'
        T(REDO.MOD.GEN.AMT)<3>='NOINPUT'
        T(REDO.MOD.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.UP.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.GEN.POINTS)<3>='NOINPUT'
        T(REDO.MOD.INT.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.UP.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.MAX.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.INT.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.MAX.GEN)<3>='NOINPUT'
    END

    IF Y.TYPE EQ '8' AND Y.FORM.GEN EQ '2' THEN
        GOSUB DIS.FIELDS.GP3
        T(REDO.MOD.INT.GEN.POINTS)<3>='NOINPUT'
        T(REDO.MOD.INT.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.UP.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.MAX.GEN)<3>='NOINPUT'
    END

RETURN

*--------------
DIS.FIELDS.GP1:
*--------------

    IF VAR.PROD.TYPE EQ 'Otro' OR VAR.PROD.TYPE EQ 'T.Debito' THEN
        T(REDO.MOD.INT.GEN.POINTS)<3>='NOINPUT'
        T(REDO.MOD.INT.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.UP.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.MAX.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.INT.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.MAX.GEN)<3>='NOINPUT'
    END

    IF VAR.PROD.TYPE EQ 'Ahorro/Certificado' THEN
*        T(REDO.MOD.GEN.POINTS)<3> = 'NOINPUT'
*        T(REDO.MOD.LOW.LIM.AMT)<3>='NOINPUT'
*        T(REDO.MOD.UP.LIM.AMT)<3> = 'NOINPUT'
        T(REDO.MOD.INT.GEN.POINTS)<3> = 'NOINPUT'
        T(REDO.MOD.INT.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.UP.LIM.AMT)<3> = 'NOINPUT'
        T(REDO.MOD.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.MAX.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.INT.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.MAX.GEN)<3>='NOINPUT'
    END

RETURN

*--------------
DIS.FIELDS.GP2:
*--------------

    IF VAR.PROD.TYPE EQ 'Otro' OR VAR.PROD.TYPE EQ 'T.Debito' THEN
        T(REDO.MOD.GEN.POINTS)<3> = 'NOINPUT'
        T(REDO.MOD.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.UP.LIM.AMT)<3> = 'NOINPUT'
        T(REDO.MOD.INT.GEN.POINTS)<3>='NOINPUT'
        T(REDO.MOD.INT.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.UP.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.INT.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.MAX.GEN)<3>='NOINPUT'
    END

    IF VAR.PROD.TYPE EQ 'Ahorro/Certificado' THEN
        T(REDO.MOD.GEN.POINTS)<3> = 'NOINPUT'
        T(REDO.MOD.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.UP.LIM.AMT)<3> = 'NOINPUT'
        T(REDO.MOD.INT.GEN.POINTS)<3>='NOINPUT'
        T(REDO.MOD.INT.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.UP.LIM.AMT)<3>='NOINPUT'
*        T(REDO.MOD.GEN.FACTOR)<3>='NOINPUT'
*        T(REDO.MOD.MIN.GEN)<3>='NOINPUT'
*        T(REDO.MOD.MAX.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.INT.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.MAX.GEN)<3>='NOINPUT'
    END

RETURN

*--------------
DIS.FIELDS.GP3:
*--------------

    T(REDO.MOD.EVENT)<3>='NOINPUT'
    T(REDO.MOD.EX.PROD.AS)<3>='NOINPUT'
    T(REDO.MOD.CHANNEL)<3> = 'NOINPUT'
    T(REDO.MOD.APP.TXN)<3>='NOINPUT'
    T(REDO.MOD.TXN.CODE)<3>='NOINPUT'
*    T(REDO.MOD.CURRENCY)<3>='NOINPUT'
    T(REDO.MOD.ANTIG)<3> = 'NOINPUT'
    T(REDO.MOD.MIN.DISBURSE.AMT)<3>='NOINPUT'
    T(REDO.MOD.GEN.POINTS)<3>='NOINPUT'
    T(REDO.MOD.LOW.LIM.AMT)<3>='NOINPUT'
    T(REDO.MOD.UP.LIM.AMT)<3>='NOINPUT'

RETURN

*--------------
DIS.FIELDS.GP4:
*--------------

    T(REDO.MOD.EVENT)<3> = 'NOINPUT'
    T(REDO.MOD.EX.PROD.AS)<3> = 'NOINPUT'
    T(REDO.MOD.APP.TXN)<3> = 'NOINPUT'
*    T(REDO.MOD.CURRENCY)<3> = 'NOINPUT'
    T(REDO.MOD.ANTIG)<3> = 'NOINPUT'

RETURN

*--------------
DIS.FIELDS.GP5:
*--------------

    T(REDO.MOD.EVENT)<3> = 'NOINPUT'
    T(REDO.MOD.EX.PROD.AS)<3> = 'NOINPUT'
    T(REDO.MOD.CHANNEL)<3> = 'NOINPUT'
    T(REDO.MOD.APP.TXN)<3> = 'NOINPUT'
    T(REDO.MOD.TXN.CODE)<3> = 'NOINPUT'
*    T(REDO.MOD.CURRENCY)<3> = 'NOINPUT'
*    T(REDO.MOD.FORM.GENERATION)<3> = 'NOINPUT'
    T(REDO.MOD.ANTIG)<3> = 'NOINPUT'

RETURN

*------------
VAL.PROD.GRP:
*------------

*    IF Y.PROD.GRP EQ '' THEN
    IF Y.TYPE NE '7' AND Y.PROD.GRP EQ '' THEN
        AF = REDO.MOD.PRODUCT.GROUP
        ETEXT = 'EB-REDO.CHECK.FIELDS':@FM:Y.TYPE
        CALL STORE.END.ERROR
    END

RETURN

*-------------
VAL.PROD.GRP2:
*-------------

    IF (Y.TYPE EQ '3' OR Y.TYPE EQ '4' OR Y.TYPE EQ '5') AND VAR.PROD.TYPE EQ 'Prestamo' THEN
        AF = REDO.MOD.PRODUCT.GROUP
        ETEXT = 'EB-REDO.LY.PGNOVAL':@FM:Y.TYPE
        CALL STORE.END.ERROR
        RETURN
    END

    IF Y.TYPE EQ '8' AND VAR.PROD.TYPE EQ 'Otro' THEN
        AF = REDO.MOD.PRODUCT.GROUP
        ETEXT = 'EB-REDO.LY.PGNOVAL':@FM:Y.TYPE
        CALL STORE.END.ERROR
    END

RETURN

END
