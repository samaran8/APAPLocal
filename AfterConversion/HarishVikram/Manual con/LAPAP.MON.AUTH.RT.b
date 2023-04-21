SUBROUTINE LAPAP.MON.AUTH.RT

*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       No changes

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
