SUBROUTINE REDO.OVERDUE.STATUS.FIELDS

*<doc>
* Template for field definitions routine REDO.AA.OVERDUE.FIELDS
* @author
* @stereotype fields template
* Reference :
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 06/04/11 -
*            New Template changes
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
*   CALL Table.defineId("TABLE.NAME.ID", T24_String)        ;* Define Table id

    ID.F = '@ID'
    ID.N = '50'
*------------------------------------------------------------------------------
    fieldName = 'DESCRIPTION'
    fieldLength = '50'
    fieldType = 'A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'OD.STATUS'
    fieldLength = '20'
    fieldType = 'A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
*------------------------------------------------------------------------------
    CALL Table.setAuditPosition

RETURN

END
