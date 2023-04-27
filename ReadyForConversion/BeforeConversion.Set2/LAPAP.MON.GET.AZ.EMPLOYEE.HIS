*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
     SUBROUTINE LAPAP.MON.GET.AZ.EMPLOYEE.HIS

     $INSERT T24.BP I_COMMON
     $INSERT T24.BP I_EQUATE
     $INSERT T24.BP I_F.ACCOUNT
     $INSERT T24.BP I_F.CUSTOMER


     FN.ACC = "F.ACCOUNT$HIS"
     F.ACC = ""
     CALL OPF(FN.ACC,F.ACC)


     FN.CUS = "F.CUSTOMER"
     F.CUS = ""
     CALL OPF(FN.CUS,F.CUS)

     ID = COMI


     CALL F.READ.HISTORY(FN.ACC,ID,R.HIS,F.ACC,ERRH)
     CUSTOMER  = R.HIS<AC.CUSTOMER>
     CALL F.READ(FN.CUS,CUSTOMER,R.CUS,F.CUS,ERRCUS)
     FAX = R.CUS<EB.CUS.FAX.1>

     IF FAX NE '' THEN
         COMI = "S"
     END ELSE
         COMI = "N"
     END

     RETURN



 END
