*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.CUS.PEP.MANDATORY.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER


    P.L_CU_PEPS = ""
    P.L_CUS_PEP = ""

    CALL GET.LOC.REF("CUSTOMER","L.CU.PEPS",FT.POS.1)
    P.L_CU_PEPS = R.NEW(LOCAL.REF.FIELD)<1,FT.POS.1>
    CALL GET.LOC.REF("CUSTOMER","L.CUS.PEP",FT.POS.2)
    P.L_CUS_PEP = R.NEW(LOCAL.REF.FIELD)<1,FT.POS.2>

    IF P.L_CU_PEPS EQ "SI" THEN
        IF P.L_CUS_PEP NE "D" AND P.L_CUS_PEP NE "V" THEN
            MESSAGE = 'CAMPO: TIPO PEP, REQUERIDO PARA PERSONAS MARCADAS COMO PEP.'
            E = MESSAGE
*CALL STORE.END.ERROR
            ETEXT = E
            CALL ERR
        END
    END

    RETURN

END
