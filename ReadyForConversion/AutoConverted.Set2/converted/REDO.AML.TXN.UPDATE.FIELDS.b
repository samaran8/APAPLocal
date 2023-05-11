SUBROUTINE REDO.AML.TXN.UPDATE.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine REDO.AML.TXN.UPDATE.FIELDS
*
* @author jvalarezoulloa@temenos.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
*-----------------------
* 25/02/2017 - Creation Date
* ----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
    $INSERT I_Table
*** </region>
*-----------------------------------------------------------------------------
*    CALL Table.defineId("@ID", 'A')        ;* Define Table id
    ID.F='@ID'
    ID.N='50'
    ID.T='A'
*-----------------------------------------------------------------------------

    CALL Table.addFieldDefinition("XX<TXN.ID", 16, 'A', "") ;* Add a new field
    CALL Table.addFieldDefinition("XX-TRANS.DATE", 25, 'A', "")       ;* Add a new field
    CALL Table.addFieldDefinition("XX-TYPE.OF.ID", 15, 'A', "")       ;* Add a new field
    CALL Table.addFieldDefinition("XX-LEGAL.ID.NUM", 65, 'A', "")     ;* Add a new field
    CALL Table.addFieldDefinition("XX-CLIENT.NAME", 65, 'A', "")      ;* Add a new field
    CALL Table.addFieldDefinition("XX-BUY.SELL.TXN", 4, 'A', "")      ;* Add a new field
    CALL Table.addFieldDefinition("XX-CURRENCY", 3, 'A', "")          ;* Add a new field
    CALL Table.addFieldDefinition("XX-FCY.AMOUNT", 19, 'AMT', "")     ;* Add a new field
    CALL Table.addFieldDefinition("XX-LCY.AMOUNT", 19, 'AMT', "")     ;* Add a new field
    CALL Table.addFieldDefinition("XX-OVERRIDE.Y.N", 1, 'A', "")      ;* Add a new field
    CALL Table.addField("XX-BRANCH.CODE", 'A', "", "")      ;* Add a new fields
    CALL Field.setCheckFile("COMPANY")  ;* Use DEFAULT.ENRICH from SS or just field 1
    CALL Table.addFieldDefinition("XX-CU.AMLBUY.RT", 19, 'AMT', "")   ;* Add a new fields
    CALL Table.addFieldDefinition("XX>TXN.ACTION", 1, 'A', "")        ;* Add a new fields

    V = Table.currentFieldPosition
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
