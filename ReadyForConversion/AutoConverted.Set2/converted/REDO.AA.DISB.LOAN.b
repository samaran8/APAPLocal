SUBROUTINE REDO.AA.DISB.LOAN
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This template is used to update the DISBURSE details for a particular Arrangement
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
*   Date               who           Reference            Description
* 02-05-2011          Bharath G       ODR-2010-08-0017      Initial Creation
*-----------------------------------------------------------------------------
* <region name= Inserts>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_Table
* </region>
*-----------------------------------------------------------------------------
    Table.name = 'REDO.AA.DISB.LOAN'      ;* Full application name including product prefix
    Table.title = 'Loan Disbursement Details'       ;* Screen title
    Table.stereotype = 'H'      ;* H, U, L, W or T
    Table.product = 'EB'        ;* Must be on EB.PRODUCT
    Table.subProduct = ''       ;* Must be on EB.SUB.PRODUCT
    Table.classification = 'FIN'          ;* As per FILE.CONTROL
    Table.systemClearFile = 'Y' ;* As per FILE.CONTROL
    Table.relatedFiles = ''     ;* As per FILE.CONTROL
    Table.isPostClosingFile = ''          ;* As per FILE.CONTROL
    Table.equatePrefix = 'DISB.LN'        ;* Use to create I_F.EB.LOG.PARAMETER
*-----------------------------------------------------------------------------
