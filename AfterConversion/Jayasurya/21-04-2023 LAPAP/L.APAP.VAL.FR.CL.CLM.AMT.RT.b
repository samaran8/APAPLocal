* @ValidationCode : MjoxMzk1ODg1NDQ4OkNwMTI1MjoxNjgyMDcxMDc4OTM2OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:27:58
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.LAPAP
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 REM TO DISPLAY.MESSAGE
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.FR.CL.CLM.AMT.RT
    $INSERT  I_COMMON
    $INSERT  I_EQUATE
    $INSERT  I_F.REDO.FRONT.CLAIMS

    Y.AMOUNT = COMI

    IF (Y.AMOUNT GT 99999999.99) THEN
        TEXT = "VALOR MAXIMO PERMITIDO 99,999,999.99"
        CALL DISPLAY.MESSAGE(TEXT, '') ;* AUTO R22 CODE CONVERSION REM TO DISPLAY.MESSAGE
        ETEXT = TEXT
        CALL STORE.END.ERROR
    END
RETURN
END
