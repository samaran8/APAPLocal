*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ENQ.IDENTIFICACION
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT T24.BP I_ENQUIRY.COMMON

    FN.CUS = "F.CUSTOMER"
    FV.CUS = ""
    Y.CUS.ID = O.DATA
    Y.POS = ""
    R.PASAPORTE = ""
    R.CEDULA = ""
    R.RNC = ""
    R.ACTA = ""
    R.NUMERO.UNICO = ""
    R.IDENTIFICACION = ""

    CALL OPF(FN.CUS,FV.CUS)

    CALL F.READ(FN.CUS,Y.CUS.ID,R.CUS,FV.CUS,CUS.ERROR)


*****PASAPORTE*****

    R.PASAPORTE = R.CUS<EB.CUS.LEGAL.ID>

    IF R.PASAPORTE NE "" THEN
        O.DATA = R.PASAPORTE
        RETURN
    END

*****RNC*****

    CALL GET.LOC.REF("CUSTOMER","L.CU.RNC",Y.POS)
    R.RNC = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

    IF R.RNC NE "" THEN
        O.DATA = R.RNC
        RETURN
    END

*****CEDULA*****

    CALL GET.LOC.REF("CUSTOMER","L.CU.CIDENT",Y.POS)
    R.CEDULA = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

    IF R.CEDULA NE "" THEN
        O.DATA = R.CEDULA
        RETURN
    END

*****ACTA NACIMIENTO*****

    CALL GET.LOC.REF("CUSTOMER","L.CU.ACTANAC",Y.POS)
    R.ACTA = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

    IF R.ACTA NE "" THEN
        O.DATA = R.ACTA
        RETURN
    END

*****NUMERO UNICO*****

    CALL GET.LOC.REF("CUSTOMER","L.CU.NOUNICO",Y.POS)
    R.NUMERO.UNICO = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

    IF R.NUMERO.UNICO NE "" THEN
        O.DATA = R.NUMERO.UNICO
        RETURN
    END


    RETURN
