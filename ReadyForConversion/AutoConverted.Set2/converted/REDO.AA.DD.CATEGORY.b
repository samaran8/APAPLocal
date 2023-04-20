SUBROUTINE REDO.AA.DD.CATEGORY
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This template is used to maintain the details for a categories for direct debit
*-----------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*-----------------------------------------------------------------------------
*   Date               who              Reference            Description
* 31-Oct-2017        Edwin Charles D    PACS00625972        DD Direct Debit CR
*-----------------------------------------------------------------------------
* <region name= Inserts>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_Table
* </region>
*-----------------------------------------------------------------------------
    Table.name = 'REDO.AA.DD.CATEGORY'  ;* Full application name including product prefix
    Table.title = 'Loan Disbursement Details'     ;* Screen title
    Table.stereotype = 'H'    ;* H, U, L, W or T
    Table.product = 'EB'      ;* Must be on EB.PRODUCT
    Table.subProduct = ''     ;* Must be on EB.SUB.PRODUCT
    Table.classification = 'INT'        ;* As per FILE.CONTROL
    Table.systemClearFile = ''          ;* As per FILE.CONTROL
    Table.relatedFiles = ''   ;* As per FILE.CONTROL
    Table.isPostClosingFile = ''        ;* As per FILE.CONTROL
    Table.equatePrefix = 'DD.CT'        ;* Use to create I_F.EB.LOG.PARAMETER
*-----------------------------------------------------------------------------
RETURN
END
