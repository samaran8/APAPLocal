* @ValidationCode : MjotMTczMzEyMjg0MTpDcDEyNTI6MTY4MjA2OTk4MTIxMDpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:09:41
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
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                REM TO DISPLAY.MESSAGE(TEXT, '') , BP REMOVED
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*------------------------------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.ACC.CATEGORY
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT ;* AUTO R22 CODE CONVERSION END
    FN.AC = "F.ACCOUNT"
    F.AC = ""
    VarAC = COMI
*DEBUG
    CALL F.READ(FN.AC, VarAC, R.AC, F.AC, '')
    VarCategory = R.AC<AC.CATEGORY>
    IF (VarCategory[1,2] EQ "66" OR VarCategory[1,1] EQ "3") THEN
*DEBUG
        TEXT = "CUENTA INVALIDA"
        CALL DISPLAY.MESSAGE(TEXT, '') ;* AUTO R22 CODE CONVERSION REM TO DISPLAY.MESSAGE(TEXT, '')
        ETEXT = TEXT
        PRINT E
        CALL STORE.END.ERROR
    END
RETURN
END
