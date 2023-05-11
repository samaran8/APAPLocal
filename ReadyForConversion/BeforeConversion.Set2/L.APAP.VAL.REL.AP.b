*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.VAL.REL.AP
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.RELATION


    Y.ACC.ID = COMI
    Y.CUS.ID = ""
    Y.RELACION.CODE = ""
    Y.JOINT.HOLDER = ""
    Y.CADENA = ""

    R.PASAPORTE = ""
    R.CEDULA = ""
    R.RNC = ""
    R.ACTA = ""
    R.NUMERO.UNICO = ""
    R.IDENTIFICACION = ""

    FN.ACC = "F.ACCOUNT"
    FV.ACC = ""

    FN.CUS = "F.CUSTOMER"
    FV.CUS = ""

    FN.REL = "F.RELATION"
    FV.REL = ""

    CALL OPF(FN.ACC,FV.ACC)
    CALL F.READ(FN.ACC,Y.ACC.ID,R.ACC,FV.ACC,ACC.ERROR)



    Y.CANT.RELACIONES = R.ACC<AC.JOINT.HOLDER>
    Y.CNT = DCOUNT(Y.CANT.RELACIONES,@VM)

    COMI = ""

    FOR I = 1 TO Y.CNT

        Y.RELACION.CODE = R.ACC<AC.RELATION.CODE , I >

        Y.CUS.ID = R.ACC<AC.JOINT.HOLDER, I >

        CALL OPF(FN.CUS,FV.CUS)

        CALL F.READ(FN.CUS,Y.CUS.ID,R.CUS,FV.CUS,CUS.ERROR)

        R.PASAPORTE = R.CUS<EB.CUS.LEGAL.ID>

        CALL GET.LOC.REF("CUSTOMER","L.CU.RNC",Y.POS)
        R.RNC = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

        CALL GET.LOC.REF("CUSTOMER","L.CU.CIDENT",Y.POS)
        R.CEDULA = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

        CALL GET.LOC.REF("CUSTOMER","L.CU.ACTANAC",Y.POS)
        R.ACTA = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

        CALL GET.LOC.REF("CUSTOMER","L.CU.NOUNICO",Y.POS)
        R.NUMERO.UNICO = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

        IF R.CEDULA NE "" THEN

            R.IDENTIFICACION = R.CEDULA

        END

        IF R.RNC NE "" THEN

            R.IDENTIFICACION = R.RNC

        END

        IF R.PASAPORTE NE "" THEN

            R.IDENTIFICACION = R.PASAPORTE

        END

        IF R.ACTA NE ""  THEN

            R.IDENTIFICACION = R.ACTA

        END

        IF R.NUMERO.UNICO NE "" THEN

            R.IDENTIFICACION = R.NUMERO.UNICO

        END

        CALL OPF(FN.REL,FV.REL)
        CALL F.READ(FN.REL,Y.RELACION.CODE,R.REL,FV.REL,REL.ERROR)

        R.DES.REL = R.REL<EB.REL.DESCRIPTION>

        IF  R.CUS<EB.CUS.NAME.1> NE "" THEN

            COMI = CADENA :  "*" : R.IDENTIFICACION : ";" : R.CUS<EB.CUS.NAME.1> : ";" : R.DES.REL
            CADENA = CADENA :  "*" : R.IDENTIFICACION : ";" : R.CUS<EB.CUS.NAME.1> : ";" : R.DES.REL

        END
        ELSE
                COMI = "" ;*CADENA :  "*" : R.IDENTIFICACION : ";" : R.CUS<EB.CUS.NAME.1> : ";" : R.DES.REL
         END

    NEXT I

END
