*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.STATUS.TRADUCCION

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON


    BEGIN CASE

    CASE O.DATA EQ "DECEASED"
        Y.TRADUCCION = "FALLECIDO"

    CASE O.DATA EQ "GARNISHMENT"
        Y.TRADUCCION = "EMBARGADO"


    CASE O.DATA EQ "GUARANTEE STATUS"
        Y.TRADUCCION = "ESTADO DE GARANTMA"

    END CASE

    O.DATA = Y.TRADUCCION

    RETURN

END
