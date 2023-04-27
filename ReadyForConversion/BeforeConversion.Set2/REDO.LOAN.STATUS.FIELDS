*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.LOAN.STATUS.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine REDO.LETTER.ISSUE.FIELDS
*
* @author nareshc@temenos.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
*  DATE             WHO         REFERENCE         DESCRIPTION
* 25-10-2010      H GANESH      ODR-2010-03-0176   INITIAL CREATION
*
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
*    CALL Table.defineId("REDO.LOAN.STATUS", T24_String)        ;* Define Table id
*-----------------------------------------------------------------------------

  ID.F = '@ID'
  ID.N = '16'
  ID.T = 'A'

  fieldName = 'DESCRIPTION'
  fieldLength = '35.1'
  fieldType = 'A'
  neighbour = ''
  CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)


  CALL Table.addField("RESERVED.5",  T24_String, Field_NoInput,"");
  CALL Table.addField("RESERVED.4",  T24_String, Field_NoInput,"");
  CALL Table.addField("RESERVED.3",  T24_String, Field_NoInput,"");
  CALL Table.addField("RESERVED.2",  T24_String, Field_NoInput,"");
  CALL Table.addField("RESERVED.1",  T24_String, Field_NoInput,"");

  CALL Table.addLocalReferenceField('XX.LOCAL.REF')
  CALL Table.addOverrideField


*-----------------------------------------------------------------------------
  CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
  RETURN
*-----------------------------------------------------------------------------
END
