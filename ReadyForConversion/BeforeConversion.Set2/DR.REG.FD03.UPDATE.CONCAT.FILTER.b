*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.REG.FD03.UPDATE.CONCAT.FILTER(REC.ID)
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   :
* Program Name   : DR.REG.FD03.UPDATE.CONCAT
* Date           : 10-June-2013
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the transaction made over 1000 USD by individual Customer
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date       Author              Modification Description
*
*
*-----------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES

    $INCLUDE LAPAP.BP I_DR.REG.FD03.UPDATE.CONCAT.COMMON

    BEGIN CASE

    CASE CONTROL.LIST<1,1> EQ "TRANSACTION.DETAIL"
        IF NOT(NUM(REC.ID)) THEN
            REC.ID = ""       ;* Return NULL if it is not a CUSTOMER Account
        END

    CASE 1
        NULL
    END CASE

    RETURN

*-----------------------------------------------------------------------------
END
