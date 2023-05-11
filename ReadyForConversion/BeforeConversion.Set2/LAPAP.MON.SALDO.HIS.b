*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
     SUBROUTINE LAPAP.MON.SALDO.HIS

     $INSERT T24.BP I_COMMON
     $INSERT T24.BP I_EQUATE
     $INSERT T24.BP I_F.ACCOUNT

     FN.ACC = "F.ACCOUNT$HIS"
     F.ACC = ""
     CALL OPF(FN.ACC,F.ACC)

     ID = COMI
     CALL F.READ.HISTORY(FN.ACC,ID,R.HIS,F.ACC,ERRH)
     CALL GET.LOC.REF("ACCOUNT","L.AC.AV.BAL",POS)
     SALDO = R.HIS<AC.LOCAL.REF,POS>
     COMI = SALDO

     RETURN

 END
