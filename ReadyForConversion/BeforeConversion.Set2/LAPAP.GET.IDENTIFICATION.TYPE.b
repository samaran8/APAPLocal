    SUBROUTINE LAPAP.GET.IDENTIFICATION.TYPE(R.CUSTOMER,OUT.ARR)

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER

    GOSUB INIT
    GOSUB GET.VALUES
    GOSUB GET.CUST.IDEN.TYPE
    OUT.ARR = CUST.IDEN.TYPE

    RETURN

INIT:
*****
    CUS.NATION = ''; YCUS.CIDENT = ''; YCUS.RNC = ''; YCUS.FOREIGN = ''; YCUS.LEGAL = ''
    CUST.IDEN.TYPE = ''; L.CU.PASS.NAT.POS = ''; Y.FIELD.POS = ''; L.CU.CIDENT.POS = ''; L.CU.RNC.POS = ''
    Y.APPLICATION = 'CUSTOMER'
    Y.FIELDS = 'L.CU.CIDENT':VM:'L.CU.RNC':VM:'L.CU.PASS.NAT':VM:'L.CU.NOUNICO':VM:'L.CU.ACTANAC'
    CALL MULTI.GET.LOC.REF(Y.APPLICATION,Y.FIELDS,Y.FIELD.POS)
    L.CU.CIDENT.POS = Y.FIELD.POS<1,1>
    L.CU.RNC.POS = Y.FIELD.POS<1,2>
    L.CU.PASS.NAT.POS = Y.FIELD.POS<1,3>
    L.CU.NOUNICO.POS = Y.FIELD.POS<1,4>
    L.CU.ACTANAC.POS = Y.FIELD.POS<1,5>
    RETURN

GET.VALUES:
***********
    CUS.NATION = R.CUSTOMER<EB.CUS.NATIONALITY>
    YCUS.CIDENT = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.CIDENT.POS>
    YCUS.RNC = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.RNC.POS>
    YCUS.FOREIGN = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.PASS.NAT.POS>
    YCUS.LEGAL = R.CUSTOMER<EB.CUS.LEGAL.ID,1>
    Y.L.CU.ACTANAC = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.ACTANAC.POS>
    Y.L.CU.NOUNICO = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.NOUNICO.POS>
    RETURN

GET.CUST.IDEN.TYPE:
**************
    BEGIN CASE
    CASE YCUS.CIDENT NE ''
        CUST.IDEN.TYPE = 'CED'
    CASE YCUS.RNC NE ''
        CUST.IDEN.TYPE = 'RNC'
    CASE Y.L.CU.ACTANAC NE ''
        CUST.IDEN.TYPE = 'ACT'
    CASE Y.L.CU.NOUNICO NE ''
        CUST.IDEN.TYPE = 'NOU'
    CASE YCUS.LEGAL NE ''
        CUST.IDEN.TYPE = 'PAS'
    CASE YCUS.FOREIGN NE ''
        CUST.IDEN.TYPE = 'PAS'
    END CASE
    RETURN

END