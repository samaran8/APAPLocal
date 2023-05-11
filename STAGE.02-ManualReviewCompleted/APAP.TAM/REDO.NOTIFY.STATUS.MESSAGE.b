$PACKAGE APAP.TAM
SUBROUTINE REDO.NOTIFY.STATUS.MESSAGE
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------
*  DATE             WHO         REFERENCE         DESCRIPTION
* 18-Apr-2011   H GANESH       PACS00033637    INITIAL CREATION
*
** 13-04-2023 R22 Auto Conversion no changes
** 13-04-2023 Skanda R22 Manual Conversion - No changes
* ----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_Table

*-----------------------------------------------------------------------------
    Table.name = 'REDO.NOTIFY.STATUS.MESSAGE'       ;* Full application name including product prefix
    Table.title = 'Notify Message'        ;* Screen title
    Table.stereotype = 'H'      ;* H, U, L, W or T
    Table.product = 'EB'        ;* Must be on EB.PRODUCT
    Table.subProduct = ''       ;* Must be on EB.SUB.PRODUCT
    Table.classification = 'INT'          ;* As per FILE.CONTROL
    Table.systemClearFile = 'Y' ;* As per FILE.CONTROL
    Table.relatedFiles = ''     ;* As per FILE.CONTROL
    Table.isPostClosingFile = ''          ;* As per FILE.CONTROL
    Table.equatePrefix = 'REDO.NOTIF'     ;* Use to create I_F.EB.LOG.PARAMETER
*-----------------------------------------------------------------------------
    Table.idPrefix = ''         ;* Used by EB.FORMAT.ID if set
    Table.blockedFunctions = '' ;* Space delimeted list of blocked functions
    Table.trigger = ''          ;* Trigger field used for OPERATION style fields
*-----------------------------------------------------------------------------

RETURN
END
