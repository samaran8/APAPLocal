*========================================================================
SUBROUTINE LAPAP.CUS.PEP.AUTO
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.CUS.PEP.AUTO
* Date           : 2019-08-13
* Item ID        :
*========================================================================
* Brief description :
* -------------------
* This program validate Pep's status
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2019-08-13     Richard HC                Initial Development
*========================================================================
* Content summary :
* =================
* Table name     : CUSTOMER
* Auto Increment : N/A
* Views/versions : ALL VERSION IN CUSTOMER
* EB record      : LAPAP.CUS.PEP.AUTO
* Routine        : LAPAP.CUS.PEP.AUTO
*========================================================================

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER

    FN.CUS = "F.CUSTOMER"
    F.CUS = ""
    CALL OPF(FN.CUS,F.CUS)


    CALL GET.LOC.REF("CUSTOMER","L.CU.PEPS",POS)

    PEPS = R.NEW(EB.CUS.LOCAL.REF)<1,POS>

    IF PEPS EQ "" THEN

        R.NEW(EB.CUS.LOCAL.REF)<1,POS> = "NO"

    END



RETURN

END
