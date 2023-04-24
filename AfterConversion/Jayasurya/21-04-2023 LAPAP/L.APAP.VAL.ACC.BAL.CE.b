* @ValidationCode : MjoxNzYxNzExNjA2OkNwMTI1MjoxNjgyMDY5OTM2NzE0OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:08:56
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
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                REM TO DISPLAY.MESSAGE(TEXT, '')
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*---------------------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.ACC.BAL.CE
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    FN.AC = "F.ACCOUNT"
    F.AC = ""
*DEBUG
    VarAC = COMI
*DEBUG
*CALL F.READ(FN.AC, VarAC, R.AC, F.AC, '')
*VarBalance = R.AC<AC.ONLINE.ACTUAL.BAL>
    VarBalance = VarAC
    IF (VarBalance NE 0 AND VarBalance NE '') THEN
*DEBUG
        TEXT = "CUENTA INVALIDA, BALANCE NO ES CERO"
        CALL DISPLAY.MESSAGE(TEXT, '') ;* AUTO R22 CODE CONVERSION REM TO DISPLAY.MESSAGE(TEXT, '')
        ETEXT = TEXT
        PRINT E
        CALL STORE.END.ERROR
    END
RETURN
END
