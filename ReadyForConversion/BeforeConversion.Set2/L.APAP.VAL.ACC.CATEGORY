*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.ACC.CATEGORY
$INSERT T24.BP I_COMMON
$INSERT T24.BP I_EQUATE
$INSERT T24.BP I_F.ACCOUNT
FN.AC = "F.ACCOUNT"
F.AC = ""
VarAC = COMI
*DEBUG
CALL F.READ(FN.AC, VarAC, R.AC, F.AC, '')
VarCategory = R.AC<AC.CATEGORY>
IF (VarCategory[1,2] EQ "66" OR VarCategory[1,1] EQ "3") THEN
*DEBUG
TEXT = "CUENTA INVALIDA"
 CALL REM
 ETEXT = TEXT
 PRINT E
 CALL STORE.END.ERROR
END
RETURN
END
