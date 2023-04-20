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

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_DR.REG.PASIVAS.CONCAT.COMMON


    IF CONTROL.LIST<1,1> EQ "GRP.16" THEN
        IF NOT(NUM(REC.ID)) THEN
            REC.ID = ""       ;* Return NULL if it is not a CUSTOMER Account
        END
    END
RETURN

END
