*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------

    SUBROUTINE DR.REGN16.CLAIMS.ID.FORMAT

*routine resent for the issue PACS00305494

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE

    CLAIM.ID = COMI
    CHANGE '-' TO '' IN CLAIM.ID
    COMI = CLAIM.ID
    RETURN
END
