*-----------------------------------------------------------------------------
* <Rating>-40</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.MON.AUTH.RT

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT T24.BP I_F.ACCOUNT


    GOSUB INIT
    GOSUB INITb
    GOSUB PROCESS
    GOSUB END_PROCESS


INIT:
*----

    FN.ACC = "F.ACCOUNT"
    F.ACC = ""

    ACC.NUM = COMI
    CALL OPF(FN.ACC,F.ACC)

    Y.AUTHORISER = ''

    RETURN

INITb:
*----

    CALL F.READ(FN.ACC,ACC.NUM,R.ACC,F.ACC,ACC.ERR)

    RETURN


PROCESS:
*-------
    Y.AUTHORISER = R.ACC<AC.AUTHORISER>


    RETURN


    END_PROCESS:
*---------------


    COMI = FIELD(Y.AUTHORISER, "_", 2)

    RETURN


END
