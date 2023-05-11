* @ValidationCode : MjotMTIyNjEwNjQxMzpDcDEyNTI6MTY4MTE4OTk5NjIzMzpJVFNTOi0xOi0xOjA6MTp0cnVlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Apr 2023 10:43:16
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.VISA.OUT.MAP.FIELDS
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
*
*  DATE             WHO           REFERENCE          DESCRIPTION
* 07-12-2010    Akthar Rasool S  ODR-2010-08-0469   INITIAL CREATION
* 11.04.2023    Conversion Tool       R22            Auto Conversion     - No changes
* 11.04.2023    Shanmugapriya M       R22            Manual Conversion   - No changes
*
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId("REDO.VISA.STLMT.MAPPING", T24_String)          ;* Define Table id
*-----------------------------------------------------------------------------
    ID.F = '@ID' ; ID.N = '6'
    ID.T = ''   ; ID.T<2>='SYSTEM'


    fieldName='XX<FIELD.NAME'
    fieldLength='30.1'
    fieldType='A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='XX-FIELD.POS'
    fieldLength='3'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='XX-CONSTANT'
    fieldLength='35'
    fieldType='A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='XX-ATM.REV.FIELD'
    fieldLength='30'
    fieldType='A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='XX-ATM.REV.POS'
    fieldLength='3'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='XX>VALUE.RTN'
    fieldLength='35'
    fieldType='A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)



    CALL Table.addField("RESERVED.15", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.14", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.13", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.12", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.11", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.10", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.9", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.8", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.7", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.6", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.5", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.4", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.3", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.2", T24_String, Field_NoInput,"")
    CALL Table.addField("RESERVED.1", T24_String, Field_NoInput,"")




    CALL Table.addOverrideField


*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
