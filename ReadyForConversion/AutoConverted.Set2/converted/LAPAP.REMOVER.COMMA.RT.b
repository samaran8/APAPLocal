SUBROUTINE LAPAP.REMOVER.COMMA.RT

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.VALOR = O.DATA

    Y.VALOR = EREPLACE(Y.VALOR,',','')

    O.DATA = Y.VALOR

RETURN

END
