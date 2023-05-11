* @ValidationCode : MjotMzYxNzQ1NzA6Q3AxMjUyOjE2ODEyOTA3MzQ1MjM6SVRTU0JORzotMTotMTowOjA6dHJ1ZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 12 Apr 2023 14:42:14
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
SUBROUTINE REDO.CARD.REQ.STATUS.FIELDS
*-----------------------------------------------------------------------------
*Company   Name    : ASOCIACI POPULAR DE AHORROS Y PRTAMOS
*Developed By      : TEMENOS APPLICATION MANAGEMENT
*Program   Name    : REDO.CARD.REQ.STATUS.FIELDS
*By                : Kavitha
*Initial Creation  : 3-Mar-2011
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*11-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*11-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*-----------------------------------------------------------------------------
    CALL Table.defineId("ID",T24_String)
*-----------------------------------------------------------------------------
* CALL Table.addField(fieldName, fieldType, args, neighbour)
* CALL Field.setCheckFile(fileName)

    CALL Table.addFieldDefinition("XX.LL.DESCRIPTION", "65.1", "A", "")
    CALL Table.addField("XX.LOCAL.REF", T24_String,"","")
    CALL Table.addField("XX.OVERRIDE", T24_String, Field_NoInput ,"")

* CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour)
* CALL Field.setDefault(defaultValue)
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
