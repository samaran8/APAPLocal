*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.VAL.TIP.CL
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.CUSTOMER

    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    R.ACC = ""
    ACC.ERR = ""
    CALL OPF(FN.ACC,F.ACC) 

    FN.CUS = "F.CUSTOMER"
    F.CUS =  ""
    R.CUS = ""
    CUS.ERR = ""
    CALL OPF(FN.CUS,F.CUS)

    *CUSTOMER.ID = R.NEW(AC.CUSTOMER)
    CUSTOMER.ID = COMI

    CALL F.READ(FN.CUS,CUSTOMER.ID,R.CUS,F.CUS,CUS.ERR)

    CALL GET.LOC.REF("CUSTOMER", "L.CU.TIPO.CL",AC.POS.1)
    TIPO.CLIENTE =   R.CUS<EB.CUS.LOCAL.REF,AC.POS.1>

    IF TIPO.CLIENTE EQ 'PERSONA JURIDICA' THEN

        MESSAGE = "TIPO DE CLIENTE NO CORRESPONDE CON LA CATEGORIA DE CUENTA"
        E = MESSAGE
        CALL ERR

      *  RETURN

    END

   * RETURN


END
