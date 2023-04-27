*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.REG.PASIVAS.CONCAT.FILTER(REC.ID)
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   :
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date       Author              Modification Description
*
*-----------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES
    $INCLUDE LAPAP.BP I_DR.REG.PASIVAS.CONCAT.COMMON


    IF CONTROL.LIST<1,1> EQ "GRP.16" THEN
        IF NOT(NUM(REC.ID)) THEN
            REC.ID = ""       ;* Return NULL if it is not a CUSTOMER Account
        END
    END
    RETURN

END
