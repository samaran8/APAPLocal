*========================================================================
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE LAPAP.IDENTNTYPE.RT
*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion         ! to *

*-----------------------------------------------------------------------------

    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT ;*R22 Auto conversion - END


    GOSUB INIT
    GOSUB INITb
    GOSUB PROCESS
    GOSUB END_PROCESS


INIT:
*----

    FN.CUS = "F.CUSTOMER"
    F.CUS = ""

    customer = COMI
    CALL OPF(FN.CUS,F.CUS)

RETURN

INITb:
*----

    CALL F.READ(FN.CUS,customer,R.CUS,F.CUS,CUS.ERR)

RETURN


PROCESS:
*-------

*Using customer id to fetch the correct iidentificacion
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

RETURN


END_PROCESS:
*---------------

    BEGIN CASE
        CASE CARD.ID NE ''

            COMI = "001" : CARD.ID

        CASE NO.UNIC NE ''

            COMI = "005" : NO.UNIC

        CASE B.CERT NE ''

            COMI = "004" : B.CERT

        CASE PASS.N NE ''

            COMI = "002" : PASS.N

        CASE RNC.NO NE ''

            COMI = "003" : RNC.NO

    END CASE

RETURN

END
