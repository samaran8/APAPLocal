*-----------------------------------------------------------------------------
* <Rating>-11</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE ATM.RES.COD.TABLE.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine ATM.PROCESS.TABLE.FIELDS
*
* @author keerthysuresh@temenos.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 19/10/07 - EN_10003543
*            New Template changes
*
* 14/11/07 - BG_100015736
*            Exclude routines that are not released
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
  CALL Table.defineId("ATM.RES.CDE.ID", T24_String)         ;* Define Table id
*-----------------------------------------------------------------------------
* CALL Table.addField(fieldName, fieldType, args, neighbour) ;* Add a new fields
* CALL Field.setCheckFile(fileName)        ;* Use DEFAULT.ENRICH from SS or just field 1

  CALL Table.addFieldDefinition("XX.DESCRIPTION", 35, "A", '')        ;* Add a new field
  CALL Table.addFieldDefinition("XX<MESSAGE", 65, "A", '')
  CALL Table.addFieldDefinition("XX>RESPONSE.CODE", 5, "A", '')




* CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour) ;* Specify Lookup values
* CALL Field.setDefault(defaultValue) ;* Assign default value
*-----------------------------------------------------------------------------
  CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
  RETURN
*-----------------------------------------------------------------------------
END

