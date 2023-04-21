SUBROUTINE LAPAP.REMOVER.COMMA.RT
*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       BP is removed in Insert File

*-----------------------------------------------------------------------------

    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON ;*R22 Auto conversion - END

    Y.VALOR = O.DATA

    Y.VALOR = EREPLACE(Y.VALOR,',','')

    O.DATA = Y.VALOR

RETURN

END
