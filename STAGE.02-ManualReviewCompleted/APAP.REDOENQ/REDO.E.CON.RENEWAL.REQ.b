$PACKAGE APAP.REDOENQ
SUBROUTINE  REDO.E.CON.RENEWAL.REQ
*--------------------------------------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.E.BUILD.REGOFF
*--------------------------------------------------------------------------------------------------------
*Description  : This is conversion routine to check the agency is valid for the user accessing the data
*Linked With  : REDO.RENEWAL.CARD.REGOFF
*In Parameter : N/A
*Out Parameter: N/A
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 28 Sep 2011    Balagurunathan         PACS00131231          Initial Creation
* 11-APRIL-2023      Conversion Tool       R22 Auto Conversion - No changes
* 11-APRIL-2023      Harsha                R22 Manual Conversion - No changes  
*--------------------------------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON


    O.DATA="RENOVACION"


RETURN

END
