* @ValidationCode : MjotNDI2MDI2MDU5OkNwMTI1MjoxNjgxNzExMzIwMTA2OklUU1M6LTE6LTE6MDowOnRydWU6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 17 Apr 2023 11:32:00
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.BRANCH.REQ.STOCK.FIELDS
*-----------------------------------------------------------------------------
*<doc>
*********************************************************************************************************
*Company   Name    : APAP Bank
*Developed By      : Temenos Application Management
*Program   Name    : REDO.BRANCH.REQ.STOCK.FIELDS
*--------------------------------------------------------------------------------------------------------
*Description       : This routine is a .FIELDS routine for template REDO.BRANCH.REQ.STOCK
*
*</doc>
*-----------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 8 MAR 2011    SWAMINATHAN.S.R       ODR-2010-03-0400         Initial Creation
* 6 JUN 2011    KAVITHA               PACS00024249             PACS00024249 FIX
* Date                  who                   Reference              
* 17-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION - No Change
* 17-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
* ----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
    $INSERT I_Table
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId("@ID", T24_String)          ;* Define Table id
    ID.N = '25'    ; ID.T = 'A'

*-----------------------------------------------------------------------------
    neighbour = ''
*    fieldName = 'INITIAL.LOAD'  ; fieldLength = '25'   ; fieldType = ''  ;  GOSUB ADD.FIELDS
*  fieldName = 'LOAD.DATE'  ; fieldLength = '8'   ; fieldType = 'D'  ;  GOSUB ADD.FIELDS
*   fieldName = 'LAST.STOCK.REQ'  ; fieldLength = '25'   ; fieldType = 'D'  ;  GOSUB ADD.FIELDS
*    fieldName = 'LOAD.STOCK.RDM'  ; fieldLength = '25'   ; fieldType = 'D'  ;  GOSUB ADD.FIELDS

    fieldName = 'CARD.TYPE'  ; fieldLength = '25'   ; fieldType = 'A'  ;  GOSUB ADD.FIELDS
    fieldName = 'XX<INITIAL.STK'  ; fieldLength = '25'   ; fieldType = ''  ;  GOSUB ADD.FIELDS
    fieldName = 'XX-QTY.REQUEST' ; fieldLength = '25'   ; fieldType = ''  ;  GOSUB ADD.FIELDS
    fieldName = 'XX-REQUEST.ID'  ; fieldLength = '35'   ; fieldType = 'A'  ;  GOSUB ADD.FIELDS
    fieldName = 'XX-AGENCY'  ; fieldLength = '15'   ; fieldType = 'A'  ;  GOSUB ADD.FIELDS
    fieldName = 'XX-LOST'  ; fieldLength = '25'   ; fieldType = ''  ;  GOSUB ADD.FIELDS
    fieldName = 'XX-DAMAGE'  ; fieldLength = '25'   ; fieldType = ''  ;  GOSUB ADD.FIELDS
    fieldName = 'XX-RETURN'  ; fieldLength = '25'   ; fieldType = ''  ;  GOSUB ADD.FIELDS
    fieldName = 'XX-DELIVERED' ; fieldLength = '25'   ; fieldType = ''  ;  GOSUB ADD.FIELDS
    fieldName = 'XX-VIRGIN.LOAD' ; fieldLength = '25'   ; fieldType = ''  ;  GOSUB ADD.FIELDS
    fieldName = 'XX>CURRENT.QTY'   ; fieldLength = '25'   ; fieldType = ''  ;  GOSUB ADD.FIELDS
    fieldName = 'TXN.DATE'         ; fieldLength = '8'    ; fieldType = 'D' ;  GOSUB ADD.FIELDS

    V = Table.currentFieldPosition

RETURN
*-----------------------------------------------------------------------------
***********
ADD.FIELDS:
***********
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)   ;* Add a new field

RETURN
*----------------------------------------------------------------------------
END
