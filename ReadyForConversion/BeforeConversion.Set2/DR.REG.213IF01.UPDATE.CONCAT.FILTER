*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.REG.213IF01.UPDATE.CONCAT.FILTER(REC.ID)
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   :
* Program Name   : DR.REG.213IF01.UPDATE.CONCAT
* Date           : 2-May-2013
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the transaction made over 10000 USD by individual Customer
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date       Author              Modification Description
* 21-Mar-2015   Ashokkumar.V.P     PACS00309079:- Added AA overpayment details.
*-----------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES
    $INSERT T24.BP I_F.STMT.ENTRY

    $INCLUDE LAPAP.BP I_DR.REG.213IF01.UPDATE.CONCAT.COMMON

    LOCATE REC.ID IN YSUSP.ACCT.NO<1,1> SETTING POSN THEN
        RETURN
    END

    IF NOT(NUM(REC.ID)) THEN
        REC.ID = '' ;* Only customer accounts to be considered.
    END

    RETURN

*-----------------------------------------------------------------------------
END
