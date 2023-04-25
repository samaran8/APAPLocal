*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.REL.CUSTOMER.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine YOURAPPLICATION.FIELDS
*
* @author tcoleman@temenos.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
* Date             Author             Reference         Description
* 02-Mar-2011      Janani         ODR-2010-03-0150    Initial creation
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
$INSERT I_COMMON
  $INSERT I_EQUATE
  $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
  CALL Table.defineId("REDO.REL.CUSTOMER", T24_String)      ;* Define Table id
*-----------------------------------------------------------------------------
  ID.F = '@ID'
  ID.N = '10'
  ID.T = 'A'
  ID.CHECKFILE = 'CUSTOMER'


  fieldName = 'EMP.CUST.ID'
  fieldLength = '10'
  fieldType = 'A'
  neighbour = ''
  CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour) ;
  CALL Field.setCheckFile('CUSTOMER')

  RETURN
*-----------------------------------------------------------------------------
END
