*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ENQ.B186.NOTA5.RT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.CLAIM.TYPE = O.DATA

    Y.SELECTOR.NOTA.5 = 0

*1. Lista de tipos de reclamaciones de tarjeta de crédito donde intervenga una marca internacional.
    BEGIN CASE

    CASE Y.CLAIM.TYPE = "COMPRA.DUPLICADA.T.DEBITO"
        Y.SELECTOR.NOTA.5 = 1
    CASE Y.CLAIM.TYPE = "COMPRA.DUPLICADA.T.CREDITO"
        Y.SELECTOR.NOTA.5 = 1
    CASE Y.CLAIM.TYPE = "COMPRA.DUPLICADA.T.FLOTILLA"
        Y.SELECTOR.NOTA.5 = 1
    CASE Y.CLAIM.TYPE = "COMPRA.NO.RECONOCIDA.TARJ.DEBITO"
        Y.SELECTOR.NOTA.5 = 1
    CASE Y.CLAIM.TYPE = "COMPRA.NO.RECONOCIDA.FLOTILLA"
        Y.SELECTOR.NOTA.5 = 1
    CASE Y.CLAIM.TYPE = "COMPRA.NO.RECONOCIDA.T.CREDITO"
        Y.SELECTOR.NOTA.5 = 1
    CASE Y.CLAIM.TYPE = "FRAUDE.TARJETA.DE.DEBITO"
        Y.SELECTOR.NOTA.5 = 1
    CASE Y.CLAIM.TYPE = "FRAUDE.TARJETA.DE.CREDITO"
        Y.SELECTOR.NOTA.5 = 1
    CASE Y.CLAIM.TYPE = "FRAUDE.TARJETA.FLOTILLA"
        Y.SELECTOR.NOTA.5 = 1
    CASE Y.CLAIM.TYPE = "PL-COMPRA.DUPLICADA.T.CREDITO"
        Y.SELECTOR.NOTA.5 = 1
*2. Lista de tipos de reclamaciones de buro incorrecta.
    CASE Y.CLAIM.TYPE = "PL-COMPRA.NO.RECONOCIDA"
        Y.SELECTOR.NOTA.5 = 2
    CASE Y.CLAIM.TYPE = "INFO.DE.BURO.INCORRECTA"
        Y.SELECTOR.NOTA.5 = 2

    END CASE


    O.DATA = Y.SELECTOR.NOTA.5

    RETURN

END
