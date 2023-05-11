$PACKAGE APAP.TAM
SUBROUTINE REDO.NAB.ACCOUNTING.FIELDS

*-----------------------------------------------------------------------------
* Company Name  : APAP DEV2
* Developed By  : Ravikiran
* Program Name  : REDO.NAB.ACCOUNTING.FIELDS
*-----------------------------------------------------------------------------
* Description : This application is linked to REDO.AA.NAB.ACCOUNTING
*-----------------------------------------------------------------------------
* Linked With   : -NA-
* In Parameter  : -NA-
* Out Parameter : -NA-
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------
* Reference              Date                Description
* ODR-2011-09-0029      13-Dec-2011          Initial draft
** 13-04-2023 R22 Auto Conversion no changes
** 13-04-2023 Skanda R22 Manual Conversion - No changes
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId('ID', T24_String)
*-----------------------------------------------------------------------------

    fieldName = 'NAB.AMT'
    fieldLength = '10'
    fieldType = 'IN2AMT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'LOAN.STATUS'
    fieldLength = '2'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'PRODUCT'
    fieldLength = '35'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    CALL Field.setCheckFile('AA.PRODUCT')

RETURN
