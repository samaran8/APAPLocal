* @ValidationCode : MjozOTczMzA1Mjc6Q3AxMjUyOjE2ODEzNzI0NTc2MzY6SVRTU0JORzotMTotMTowOjA6dHJ1ZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 13:24:17
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
*12-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*12-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
SUBROUTINE REDO.CUS.SEC.FOL.NO.DET.FIELDS
*-----------------------------------------------------------------------------
*<doc>
*********************************************************************************************************
*Company   Name    : APAP Bank
*Developed By      : Temenos Application Management
*Program   Name    : REDO.CUS.SEC.FOL.NO.DET
*--------------------------------------------------------------------------------------------------------
*Description       : This routine ia a template routine. This template is used to store the Customer
*                    Security folder number details
*</doc>
*-----------------------------------------------------------------------------
* TODO - You MUST write a .FIELDS routine for the field definitions
*-----------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 20/05/2010    Shiva Prasad Y     ODR-2009-10-0310 B.180C      Initial Creation
* ----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId("ID", T24_String) ;* Define Table id
    ID.N = '25'    ;   ID.T = 'A'
*-----------------------------------------------------------------------------
    neighbour = ''
    fieldName = 'CUSTOMER.NO'  ; fieldLength = '35'   ; fieldType = 'A'  ;  GOSUB ADD.FIELDS
    CALL Field.setCheckFile("CUSTOMER")
    fieldName = 'SEC.FOLD.NO'  ; fieldLength = '35'   ; fieldType = ''  ;  GOSUB ADD.FIELDS
*-----------------------------------------------------------------------------
    fieldName = 'RESERVED.05'  ;  fieldType = T24_String ; args = Field_NoInput ;  GOSUB ADD.RESERVED.FIELDS
    fieldName = 'RESERVED.04'  ;  fieldType = T24_String ; args = Field_NoInput ;  GOSUB ADD.RESERVED.FIELDS
    fieldName = 'RESERVED.03'  ;  fieldType = T24_String ; args = Field_NoInput ;  GOSUB ADD.RESERVED.FIELDS
    fieldName = 'RESERVED.02'  ;  fieldType = T24_String ; args = Field_NoInput ;  GOSUB ADD.RESERVED.FIELDS
    fieldName = 'RESERVED.01'  ;  fieldType = T24_String ; args = Field_NoInput ;  GOSUB ADD.RESERVED.FIELDS
*-----------------------------------------------------------------------------
    CALL Table.addLocalReferenceField('XX.LOCAL.REF')

    CALL Table.addOverrideField

    CALL Table.setAuditPosition ;* Poputale audit information

RETURN
*-----------------------------------------------------------------------------
***********
ADD.FIELDS:
***********
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)   ;* Add a new field

RETURN
********************
ADD.RESERVED.FIELDS:
********************
    CALL Table.addField(fieldName, fieldType, args, neighbour)          ;* Add a new fields

RETURN
*----------------------------------------------------------------------------
END
