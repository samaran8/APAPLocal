$PACKAGE APAP.TAM
SUBROUTINE REDO.T.DEP.COLLATERAL.FIELDS
*-----------------------------------------------------------------------------
* COMPANY NAME   : APAP
* DEVELOPED BY   : RAJA SAKTHIVEL K P
* PROGRAM NAME   : REDO.T.DEP.COLLATERAL.FIELDS

*-----------------------------------------------------------------------------
* Description : This is the field template definition routine to create the table
* 'REDO.T.DEP.COLLATERAL'
*-----------------------------------------------------------------------------
* Input/Output :
*-----------------
* IN : NA
* OUT : NA
*------------------

*Modification History :
*
* 19/10/07 - EN_10003543
*            New Template changes
*
* 14/11/07 - BG_100015736
*            Exclude routines that are not released
** 17-04-2023 R22 Auto Conversion no changes
** 17-04-2023 Skanda R22 Manual Conversion - No changes
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
*  CALL Table.defineId("TABLE.NAME.ID", T24_String) ;* Define Table id
    ID.F = '@ID'
    ID.N = '30'
    ID.T = 'A'

*-----------------------------------------------------------------------------

    fieldName = "ARRANGEMENT.ID"
    fieldLength = "16"
    fieldType = "A"
    neighbour = ""
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)   ;* Add a new field


*-----------------------------------------------------------------------------
*    CALL Table.setAuditPosition         ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
