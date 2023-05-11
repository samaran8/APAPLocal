* @ValidationCode : MjoxOTY5MTkyNjY1OkNwMTI1MjoxNjgxMzc2MDk4MTI4OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
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
SUBROUTINE REDO.LY.VALCALPT
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: H GANESH
* PROGRAM NAME: REDO.LY.VALCALPT
* ODR NO      : ODR-2010-09-0012
*----------------------------------------------------------------------
*DESCRIPTION:  This subroutine is performed in REDO.LY.POINTS.TOT,USE version as validation routine
* The functionality is to validate the points to be used are less than available points and make the
* calculation of new available points to be populated in TOT.AVAIL.POINTS field once this points are
* extracted for use



*IN PARAMETER: NA
*OUT PARAMETER: NA
*LINKED WITH: REDO.LY.POINTS.TOT
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*27.10.2010  H GANESH         ODR-2010-09-0012     INITIAL CREATION
*13.04.2023  Conversion Tool       R22            Auto Conversion     - No changes
*13.04.2023  Shanmugapriya M       R22            Manual Conversion   - No changes
*
*----------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_EB.TRANS.COMMON
    $INSERT I_F.REDO.LY.POINTS.TOT



    GOSUB PROCESS
RETURN
*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
    Y.DISPOSE.POINTS=R.NEW(REDO.PT.T.DISPOSE.POINTS)
    Y.TOT.AVAIL.POINTS=R.NEW(REDO.PT.T.TOT.AVAIL.POINTS)
    Y.TOT.USED.POINTS=R.NEW(REDO.PT.T.TOT.USED.POINTS)

    IF Y.DISPOSE.POINTS GT Y.TOT.AVAIL.POINTS THEN
        ETEXT='EB-REDO.DISP.PT.GRT'
        CALL STORE.END.ERROR
        GOSUB END1
    END
    IF cTxn_CommitRequests EQ '1' THEN
        R.NEW(REDO.PT.T.TOT.AVAIL.POINTS)= Y.TOT.AVAIL.POINTS-Y.DISPOSE.POINTS
        R.NEW(REDO.PT.T.TOT.USED.POINTS)= Y.DISPOSE.POINTS+Y.TOT.USED.POINTS
    END
RETURN
*----------------------------------------------------------------------
END1:
*----------------------------------------------------------------------
END
