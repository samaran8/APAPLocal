*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ENQ.ARCHIVO.MATRIZ
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON

    Y.CATEGORIA = O.DATA

    Y.ARCHIVO.MATRIZ = ""

    Y.ARCHIVO.MATRIZ = "PAGE=CER.FINANCIERO.FINANCIERO.xsl"

    IF Y.CATEGORIA EQ 6612 OR Y.CATEGORIA EQ 6613 THEN
        Y.ARCHIVO.MATRIZ = "PAGE=CER.FINANCIERO.LIBRE.xsl"
    END


    IF Y.CATEGORIA EQ 6614 OR Y.CATEGORIA EQ 6615 THEN
        Y.ARCHIVO.MATRIZ = "PAGE=CER.FINANCIERO.SIN.REDENCION.xsl"
    END

    O.DATA = Y.ARCHIVO.MATRIZ

    RETURN

END
