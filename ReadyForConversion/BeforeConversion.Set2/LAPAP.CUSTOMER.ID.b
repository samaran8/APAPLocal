*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.CUSTOMER.ID(ID,RS)

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER


    FN.CUS = "F.CUSTOMER"
    F.CUS = ""
    CALL OPF (FN.CUS,F.CUS)


    CALL F.READ(FN.CUS,ID,R.CUS,F.CUS,CUS.ERR)

    CALL GET.LOC.REF("CUSTOMER","L.CU.CIDENT",POS)
    CARD.ID = R.CUS<EB.CUS.LOCAL.REF,POS>

    CALL GET.LOC.REF("CUSTOMER","L.CU.NOUNICO",POS)
    NO.UNIC = R.CUS<EB.CUS.LOCAL.REF,POS>

    CALL GET.LOC.REF("CUSTOMER","L.CU.ACTANAC",POS)
    B.CERT = R.CUS<EB.CUS.LOCAL.REF,POS>

    CALL GET.LOC.REF("CUSTOMER","L.CU.PASS.NAT",POS)
    PASS.N = R.CUS<EB.CUS.LOCAL.REF,POS>

    CALL GET.LOC.REF("CUSTOMER","L.CU.RNC",POS)
    RNC.NO = R.CUS<EB.CUS.LOCAL.REF,POS>



    BEGIN CASE
    CASE CARD.ID NE ''
        RS = CARD.ID

    CASE NO.UNIC NE ''
        RS = NO.UNIC

    CASE B.CERT NE ''
        RS = B.CERT

    CASE PASS.N NE ''
        RS = PASS.N

    CASE RNC.NO NE ''
        RS = RNC.NO

    END CASE



END
