SUBROUTINE REDO.APAP.CREDIT.CARD.DET.FIELDS
*-----------------------------------------------------------------------------
*<doc>
******************************************************************************
*Company   Name    : APAP Bank
*Program   Name    : REDO.APAP.CREDIT.CARD.DET.FIELDS
*-----------------------------------------------------------------------------
*
*</doc>
*-----------------------------------------------------------------------------
*Modification Details:
*=====================
*      Date            Who                  Reference                Description
*     ------         ------               -------------             -------------
*    15/01/2019    Ashokkumar.V.P                         Initial Release
* ----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
*    CALL Table.defineId("@ID", T24_String)        ;* Define Table id
    ID.F = '@ID'
    ID.N = '16.1'
    ID.T = "A"
*------------------------------------------------------------------------------

    fieldName='CARD.LOGO'
    fieldLength='3'
    fieldType='A'; neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='CARD.ORG'
    fieldLength='3'
    fieldType='A'; neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='CARD.DESCRIPTION'
    fieldLength='35'
    fieldType='ANY'; neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    CALL Table.addReservedField('RESERVED.6')
    CALL Table.addReservedField('RESERVED.5')
    CALL Table.addReservedField('RESERVED.4')
    CALL Table.addReservedField('RESERVED.3')
    CALL Table.addReservedField('RESERVED.2')
    CALL Table.addReservedField('RESERVED.1')

    CALL Table.setAuditPosition         ;* Poputale audit information
RETURN
END
