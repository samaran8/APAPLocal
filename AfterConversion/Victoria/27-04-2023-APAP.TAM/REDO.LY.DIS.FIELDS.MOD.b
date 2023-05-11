* @ValidationCode : MjotMTkyODY2MTU2MjpDcDEyNTI6MTY4MTI5NTIxNzYxNTpJVFNTOi0xOi0xOjA6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
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
SUBROUTINE REDO.LY.DIS.FIELDS.MOD
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: S SUDHARSANAN
* PROGRAM NAME: REDO.LY.DIS.FIELDS.MOD
* ODR NO      : ODR-2009-12-0276
*----------------------------------------------------------------------
*DESCRIPTION: This subroutine is performed in REDO.LY.MODALITY,CREATE version
* The functionality is to disable some fields according with the modality
* type to apply for point generation

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH:  REDO.LY.MODALITY
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*28.02.2011    H GANESH
*28.07.2011    RMONDRAGON   ODR-2011-06-0243   UPDATE TO ENTIRE APPLICATION
*12.04.2023  Conversion Tool       R22            Auto Conversion     - No changes
*12.04.2023  Shanmugapriya M       R22            Manual Conversion   - No changes
*
*----------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.LY.MODALITY
    IF VAL.TEXT THEN
        RETURN
    END ELSE
        GOSUB PROCESS
    END
RETURN
*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
    Y.FORM.GEN=COMI
    Y.TYPE = R.NEW(REDO.MOD.TYPE)
    IF Y.TYPE EQ '1' AND Y.FORM.GEN EQ '1' THEN
        T(REDO.MOD.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.MAX.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.INT.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.MAX.GEN)<3>='NOINPUT'
        T(REDO.MOD.EVENT)<3>='NOINPUT'
        T(REDO.MOD.EX.PROD.AS)<3>='NOINPUT'
        R.NEW(REDO.MOD.GEN.FACTOR)=''
        R.NEW(REDO.MOD.MIN.GEN)=''
        R.NEW(REDO.MOD.MAX.GEN)=''
        R.NEW(REDO.MOD.INT.GEN.FACTOR)= ''
        R.NEW(REDO.MOD.INT.MIN.GEN)=''
        R.NEW(REDO.MOD.INT.MAX.GEN)=''
        R.NEW(REDO.MOD.EVENT)=''
        R.NEW(REDO.MOD.EX.PROD.AS)=''
    END
    IF Y.TYPE EQ '1' AND Y.FORM.GEN EQ '2' THEN
        T(REDO.MOD.GEN.POINTS)<3> = 'NOINPUT'
        T(REDO.MOD.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.UP.LIM.AMT)<3> = 'NOINPUT'
        T(REDO.MOD.INT.GEN.POINTS)<3>='NOINPUT'
        T(REDO.MOD.INT.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.UP.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.EVENT)<3>='NOINPUT'
        T(REDO.MOD.EX.PROD.AS)<3>='NOINPUT'
        R.NEW(REDO.MOD.GEN.POINTS)=''
        R.NEW(REDO.MOD.LOW.LIM.AMT)=''
        R.NEW(REDO.MOD.UP.LIM.AMT)=''
        R.NEW(REDO.MOD.INT.GEN.POINTS)= ''
        R.NEW(REDO.MOD.INT.LOW.LIM.AMT)=''
        R.NEW(REDO.MOD.INT.UP.LIM.AMT)=''
        R.NEW(REDO.MOD.EVENT)=''
        R.NEW(REDO.MOD.EX.PROD.AS)=''
    END
    IF Y.TYPE EQ '8' AND Y.FORM.GEN EQ '1' THEN
        T(REDO.MOD.APP.TXN)<3>='NOINPUT'
        T(REDO.MOD.TXN.CODE)<3>='NOINPUT'
        T(REDO.MOD.CURRENCY)<3>='NOINPUT'
        T(REDO.MOD.MIN.DISBURSE.AMT)<3>='NOINPUT'
        T(REDO.MOD.GEN.POINTS)<3>='NOINPUT'
        T(REDO.MOD.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.UP.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.MAX.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.INT.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.INT.MAX.GEN)<3>='NOINPUT'
        T(REDO.MOD.EVENT)<3>='NOINPUT'
        T(REDO.MOD.EX.PROD.AS)<3>='NOINPUT'
        R.NEW(REDO.MOD.APP.TXN)=''
        R.NEW(REDO.MOD.TXN.CODE)=''
        R.NEW(REDO.MOD.CURRENCY)=''
        R.NEW(REDO.MOD.MIN.DISBURSE.AMT)=''
        R.NEW(REDO.MOD.GEN.POINTS)=''
        R.NEW(REDO.MOD.LOW.LIM.AMT)=''
        R.NEW(REDO.MOD.UP.LIM.AMT)=''
        R.NEW(REDO.MOD.GEN.FACTOR)=''
        R.NEW(REDO.MOD.MIN.GEN)=''
        R.NEW(REDO.MOD.MAX.GEN)=''
        R.NEW(REDO.MOD.INT.GEN.FACTOR)= ''
        R.NEW(REDO.MOD.INT.MIN.GEN)=''
        R.NEW(REDO.MOD.INT.MAX.GEN)=''
        R.NEW(REDO.MOD.EVENT)=''
        R.NEW(REDO.MOD.EX.PROD.AS)=''
    END
    IF Y.TYPE EQ '8' AND Y.FORM.GEN EQ '2' THEN
        T(REDO.MOD.APP.TXN)<3>='NOINPUT'
        T(REDO.MOD.TXN.CODE)<3>='NOINPUT'
        T(REDO.MOD.CURRENCY)<3>='NOINPUT'
        T(REDO.MOD.MIN.DISBURSE.AMT)<3>='NOINPUT'
        T(REDO.MOD.GEN.POINTS)<3>='NOINPUT'
        T(REDO.MOD.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.UP.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.GEN.POINTS)<3>='NOINPUT'
        T(REDO.MOD.INT.LOW.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.INT.UP.LIM.AMT)<3>='NOINPUT'
        T(REDO.MOD.GEN.FACTOR)<3>='NOINPUT'
        T(REDO.MOD.MIN.GEN)<3>='NOINPUT'
        T(REDO.MOD.MAX.GEN)<3>='NOINPUT'
        T(REDO.MOD.EVENT)<3>='NOINPUT'
        T(REDO.MOD.EX.PROD.AS)<3>='NOINPUT'
        R.NEW(REDO.MOD.APP.TXN)=''
        R.NEW(REDO.MOD.TXN.CODE)=''
        R.NEW(REDO.MOD.CURRENCY)=''
        R.NEW(REDO.MOD.MIN.DISBURSE.AMT)=''
        R.NEW(REDO.MOD.GEN.POINTS)=''
        R.NEW(REDO.MOD.LOW.LIM.AMT)=''
        R.NEW(REDO.MOD.UP.LIM.AMT)=''
        R.NEW(REDO.MOD.INT.GEN.POINTS)=''
        R.NEW(REDO.MOD.INT.LOW.LIM.AMT)=''
        R.NEW(REDO.MOD.INT.UP.LIM.AMT)=''
        R.NEW(REDO.MOD.GEN.FACTOR)=''
        R.NEW(REDO.MOD.MIN.GEN)=''
        R.NEW(REDO.MOD.MAX.GEN)=''
        R.NEW(REDO.MOD.EVENT)=''
        R.NEW(REDO.MOD.EX.PROD.AS)=''
    END
RETURN
END
