* @ValidationCode : MjoxNTMzOTk0OTg2OkNwMTI1MjoxNjgxMjE4OTM4MjYwOklUU1NCTkc6LTE6LTE6MDowOnRydWU6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 11 Apr 2023 18:45:38
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
SUBROUTINE REDO.CARD.NO.LOCK.FIELDS
*-----------------------------------------------------------------------------
*<doc>
*********************************************************************************************************
*Company   Name    : APAP Bank
*Developed By      : Temenos Application Management
*Program   Name    : REDO.CARD.NO.LOCK.FIELDS
*--------------------------------------------------------------------------------------------------------
*Description       : This routine is a .FIELDS routine for template REDO.CARD.NUMBERS
*
*</doc>
*-----------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 19 JUL 2010    Mohammed Anies K       ODR-2010-03-0400         Initial Creation
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*11-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*11-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
* ----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
    $INSERT I_Table
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId("@ID", T24_String)          ;* Define Table id
    ID.N = '25'    ; ID.T = 'A'
*-----------------------------------------------------------------------------
    neighbour = ''
    fieldName = 'XX-CARD.NUMBER'  ; fieldLength = '35'   ; fieldType = ''  ;  GOSUB ADD.FIELDS
    V = Table.currentFieldPosition

RETURN
*-----------------------------------------------------------------------------
***********
ADD.FIELDS:
***********
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)   ;* Add a new field
RETURN
*----------------------------------------------------------------------------
END
