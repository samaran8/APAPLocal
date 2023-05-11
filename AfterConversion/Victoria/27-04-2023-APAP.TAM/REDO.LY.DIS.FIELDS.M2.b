* @ValidationCode : MjotMjI5NDIxNjc6Q3AxMjUyOjE2ODEyOTUyMTc1OTI6SVRTUzotMTotMTowOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 12 Apr 2023 15:56:57
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
SUBROUTINE REDO.LY.DIS.FIELDS.M2
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: RMONDRAGON
* PROGRAM NAME: REDO.LY.DIS.FIELDS.M
* ODR NO      : ODR-2011-06-0243
*----------------------------------------------------------------------
*DESCRIPTION: This subroutine is performed in REDO.LY.MODALITY,EDIT version
* The functionality is to disable some fields according with the modality
* type to apply for point generation

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH:  REDO.LY.MODALITY
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*02.06.2014   RMONDRAGON    ODR-2011-06-0243  INITIAL CREATION
*12.04.2023  Conversion Tool       R22            Auto Conversion     - No changes
*12.04.2023  Shanmugapriya M       R22            Manual Conversion   - No changes
*
*----------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.LY.MODALITY

    GOSUB OPEN.FILES
    GOSUB READ.MOD
    GOSUB PROCESS1
    GOSUB PROCESS2
    GOSUB PROCESS3

RETURN

*----------
OPEN.FILES:
*----------

    FN.REDO.LY.MODALITY = 'F.REDO.LY.MODALITY'
    F.REDO.LY.MODALITY = ''
    CALL OPF(FN.REDO.LY.MODALITY,F.REDO.LY.MODALITY)

    FN.REDO.LY.MODALITYNAU = 'F.REDO.LY.MODALITY$NAU'
    F.REDO.LY.MODALITYNAU = ''
    CALL OPF(FN.REDO.LY.MODALITYNAU,F.REDO.LY.MODALITYNAU)

RETURN

*--------
READ.MOD:
*--------

    R.MODALITY = ''; MOD.ERR = ''
    CALL F.READ(FN.REDO.LY.MODALITY,COMI,R.MODALITY,F.REDO.LY.MODALITY,MOD.ERR)
    IF R.MODALITY THEN
        GOSUB READ.FIELDS
        RETURN
    END

    CALL F.READ(FN.REDO.LY.MODALITYNAU,COMI,R.MODALITY,F.REDO.LY.MODALITYNAU,MOD.ERR)
    IF R.MODALITY THEN
        GOSUB READ.FIELDS
    END

RETURN

*-----------
READ.FIELDS:
*-----------

    Y.CHANNEL = R.MODALITY<REDO.MOD.CHANNEL>
    Y.APP.TXN = R.MODALITY<REDO.MOD.APP.TXN>
    Y.TXN.CODE = R.MODALITY<REDO.MOD.TXN.CODE>
    Y.CURRENCY = R.MODALITY<REDO.MOD.CURRENCY>
    Y.FORM.GENERATION = R.MODALITY<REDO.MOD.FORM.GENERATION>
    Y.PRODUCT.GROUP = R.MODALITY<REDO.MOD.PRODUCT.GROUP>
    Y.MIN.DISBURSE.AMT = R.MODALITY<REDO.MOD.MIN.DISBURSE.AMT>
    Y.GEN.POINTS = R.MODALITY<REDO.MOD.GEN.POINTS>
    Y.LOW.LIM.AMT = R.MODALITY<REDO.MOD.LOW.LIM.AMT>
    Y.UP.LIM.AMT = R.MODALITY<REDO.MOD.UP.LIM.AMT>
    Y.INT.GEN.POINTS = R.MODALITY<REDO.MOD.INT.GEN.POINTS>
    Y.GEN.FACTOR = R.MODALITY<REDO.MOD.GEN.FACTOR>
    Y.INT.GEN.FACTOR = R.MODALITY<REDO.MOD.INT.GEN.FACTOR>
    Y.EVENT = R.MODALITY<REDO.MOD.EVENT>
    Y.EX.PROD.AS = R.MODALITY<REDO.MOD.EX.PROD.AS>
    Y.GEN.AMT = R.MODALITY<REDO.MOD.GEN.AMT>
    Y.ANTIG = R.MODALITY<REDO.MOD.ANTIG>

RETURN

*--------
PROCESS1:
*--------

    IF Y.CHANNEL EQ '' THEN
        T(REDO.MOD.CHANNEL)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.CHANNEL)<3>=''
    END

    IF Y.APP.TXN EQ '' THEN
        T(REDO.MOD.APP.TXN)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.APP.TXN)<3>=''
    END

    IF Y.TXN.CODE EQ '' THEN
        T(REDO.MOD.TXN.CODE)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.TXN.CODE)<3>=''
    END

    IF Y.CURRENCY EQ '' THEN
        T(REDO.MOD.CURRENCY)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.CURRENCY)<3>=''
    END

    IF Y.FORM.GENERATION EQ '' THEN
        T(REDO.MOD.FORM.GENERATION)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.FORM.GENERATION)<3>=''
    END

    IF Y.MIN.DISBURSE.AMT EQ '' THEN
        T(REDO.MOD.MIN.DISBURSE.AMT)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.MIN.DISBURSE.AMT)<3>=''
    END

RETURN

*--------
PROCESS2:
*--------

    IF Y.GEN.POINTS EQ '' THEN
        T(REDO.MOD.GEN.POINTS)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.GEN.POINTS)<3>=''
    END

    IF Y.LOW.LIM.AMT EQ '' THEN
        T(REDO.MOD.LOW.LIM.AMT)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.LOW.LIM.AMT)<3>=''
    END

    IF Y.UP.LIM.AMT EQ '' THEN
        T(REDO.MOD.UP.LIM.AMT)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.UP.LIM.AMT)<3>=''
    END

    IF Y.INT.GEN.POINTS EQ '' THEN
        T(REDO.MOD.INT.GEN.POINTS)<3>='NOINPUT'
        T(REDO.MOD.INT.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.UP.LIM.AMT)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.INT.GEN.POINTS)<3>=''
        T(REDO.MOD.INT.LOW.LIM.AMT)<3>=''
        T(REDO.MOD.INT.UP.LIM.AMT)<3>=''
    END

    IF Y.GEN.FACTOR EQ '' THEN
        T(REDO.MOD.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.MAX.GEN)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.GEN.FACTOR)<3>=''
        T(REDO.MOD.MIN.GEN)<3>=''
        T(REDO.MOD.MAX.GEN)<3>=''
    END

    IF Y.INT.GEN.FACTOR EQ '' THEN
        T(REDO.MOD.INT.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.INT.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.MAX.GEN)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.INT.GEN.FACTOR)<3>=''
        T(REDO.MOD.INT.MIN.GEN)<3>=''
        T(REDO.MOD.INT.MAX.GEN)<3>=''
    END

RETURN

*--------
PROCESS3:
*--------

    IF Y.EVENT EQ '' THEN
        T(REDO.MOD.EVENT)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.EVENT)<3>=''
    END

    IF Y.EX.PROD.AS EQ '' THEN
        T(REDO.MOD.EX.PROD.AS)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.EX.PROD.AS)<3>=''
    END

    T(REDO.MOD.GEN.AMT)<3>='NOINPUT'

    IF Y.ANTIG EQ '' THEN
        T(REDO.MOD.ANTIG)<3>='NOINPUT'
    END ELSE
        T(REDO.MOD.ANTIG)<3>=''
    END

RETURN

END
