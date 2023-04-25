*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.TD.OFU

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT TAM.BP I_F.LATAM.CARD.ORDER

    FN.LATAM = "F.LATAM.CARD.ORDER"
    F.LATAM = ""
    CALL OPF(FN.LATAM,F.LATAM)

    CALL F.READ(FN.LATAM,CARD.ID,R.LATAM,F.LATAM,LATAM.ERR)

    VALID = COMI

    COMI =  VALID[1,6] : 'XXXXXX' : VALID[13,4]

END
