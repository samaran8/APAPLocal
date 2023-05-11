SUBROUTINE L.APAP.VAL.LBEN.CEDULA.RT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BENEFICIARY

    GOSUB INITIAL
    GOSUB PROCESS

INITIAL:
    Y.APPLICATION = 'BENEFICIARY'
    Y.FIELDS = 'L.BEN.DOC.ARCIB':@VM:'L.BEN.CEDULA':@VM:'L.BEN.COUNTRY':@VM:'L.BEN.GENDER'
    CALL MULTI.GET.LOC.REF(Y.APPLICATION,Y.FIELDS,Y.FIELD.POS)
    L.BEN.DOC.ARCIB.POS = Y.FIELD.POS<1,1>
    L.BEN.CEDULA.POS = Y.FIELD.POS<1,2>
    L.BEN.COUNTRY.POS = Y.FIELD.POS<1,3>
    L.BEN.GENDER.POS = Y.FIELD.POS<1,4>

    Y.IDENTIFICACION = COMI
    Y.TIPO.IDENTIFICACION = R.NEW(ARC.BEN.LOCAL.REF)<1,L.BEN.DOC.ARCIB.POS>
RETURN

PROCESS:
    IF Y.TIPO.IDENTIFICACION EQ 'CEDULA' OR Y.TIPO.IDENTIFICACION EQ 'Cedula' THEN
        OUT.ARR = '';
        CALL L.APAP.VERIF.CEDULA.RT(Y.IDENTIFICACION,OUT.ARR)
        V1 = OUT.ARR<1>
        V2 = OUT.ARR<2>
        V3 = OUT.ARR<3>
        IF V2 EQ '-1' THEN
            MESSAGE = V3
            E = MESSAGE
            ETEXT = E
            CALL ERR
        END
    END
RETURN

END
