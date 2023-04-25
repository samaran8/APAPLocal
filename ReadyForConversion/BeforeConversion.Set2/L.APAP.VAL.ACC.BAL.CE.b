*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
 SUBROUTINE L.APAP.VAL.ACC.BAL.CE
 $INSERT T24.BP I_COMMON
 $INSERT T24.BP I_EQUATE
 $INSERT T24.BP I_F.ACCOUNT
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
  CALL REM
  ETEXT = TEXT
  PRINT E
  CALL STORE.END.ERROR
 END
 RETURN
 END
