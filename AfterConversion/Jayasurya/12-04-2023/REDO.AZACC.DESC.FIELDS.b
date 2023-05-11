* @ValidationCode : MjotMTA5NTU4NzE4MTpDcDEyNTI6MTY4MTIwNjA0NTI0ODpJVFNTQk5HOi0xOi0xOjA6MDp0cnVlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Apr 2023 15:10:45
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
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*11-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*11-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
SUBROUTINE REDO.AZACC.DESC.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine REDO.AZACC.DESC.FIELDS
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
*-------------------------------------------------------
    CALL Table.defineId("ID", T24_String) ;* Define Table id
*-----------------------------------------------------------------------------
    neighbour = ''
*    fieldName = 'XX<DESC'
    fieldName = 'DESC'
    fieldLength = '50'
    fieldType = 'A'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)   ;* Add a new field

    neighbour = ''
*    fieldName = 'XX>XX.ASSET.TYPE'
    fieldName = 'ASSET.TYPE'
    fieldLength = '25'
    fieldType = 'A'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)   ;* Add a new field

*    CALL Field.setDefault(defaultValue) ;* Assign default value
*-----------------------------------------------------------------------------
*    CALL Table.setAuditPosition         ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
