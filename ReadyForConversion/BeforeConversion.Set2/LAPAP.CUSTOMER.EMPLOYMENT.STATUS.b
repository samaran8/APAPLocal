*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
     SUBROUTINE LAPAP.CUSTOMER.EMPLOYMENT.STATUS

     $INSERT I_COMMON
     $INSERT I_EQUATE

     $INSERT T24.BP I_F.CUSTOMER

     FN.CUSTOMER = "F.CUSTOMER"
     F.CUSTOMER = ""
     CALL OPF(FN.CUSTOMER,F.CUSTOMER)

     EMPLOYMENT.STATUS = COMI

     IF EMPLOYMENT.STATUS EQ 04 OR EMPLOYMENT.STATUS EQ 62 THEN
         ETEXT = "VALOR NO VALIDO"
         CALL STORE.END.ERROR
     END

     RETURN

 END
