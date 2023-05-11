$PACKAGE APAP.TAM
SUBROUTINE REDO.URB.ENS.RES.DOMI.FIELDS
*-----------------------------------------------------------------------------
*Company   Name    : ASOCIACI POPULAR DE AHORROS Y PRTAMOS
*Developed By      : TEMENOS APPLICATION MANAGEMENT
*Program   Name    : REDO.URB.ENS.RES.DOMI.FIELDS
** 18-04-2023 R22 Auto Conversion no changes
** 18-04-2023 Skanda R22 Manual Conversion - No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*-----------------------------------------------------------------------------
    CALL Table.defineId("ID", T24_String)
*-----------------------------------------------------------------------------
    fieldName="XX.DESCRIPTION"
    fieldLength="35.1"
    fieldType="A"
    neighbour=""
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
*-----------------------------------------------------------------------------
    fieldName="XX.SHORT.NAME"
    fieldLength="35.1"
    fieldType="A"
    neighbour=""
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
*------------------------------------------------------------------------------
    CALL Table.addField("XX.LOCAL.REF", T24_String,"","")
    CALL Table.addField("XX.OVERRIDE", T24_String, Field_NoInput ,"")
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
