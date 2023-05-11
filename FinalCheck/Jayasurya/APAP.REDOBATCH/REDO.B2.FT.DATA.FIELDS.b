* @ValidationCode : Mjo1Njc2ODQyMTE6Q3AxMjUyOjE2ODE3MDg2MTA4NzY6SVRTUzotMTotMTowOjA6dHJ1ZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 10:46:50
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
SUBROUTINE REDO.B2.FT.DATA.FIELDS
*-----------------------------------------------------------------------------
*<doc>
** Template for field definitions routine REDO.B2.FT.DATA.FIELDS *
* @author ejijon@temenos.com
* @stereotype fields template
* @uses Table
** </doc>
*-----------------------------------------------------------------------------
* Modification History :
*------------------------
* Date                  who                   Reference              
* 17-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION - No Change
* 17-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
    $INSERT I_F.REDO.B2.FT.DATA
*** </region>
*-----------------------------------------------------------------------------
*
    CALL Table.defineId("TABLE.NAME.ID", T24_String)          ;* Define Table id
*    ID.F = '@ID'
*    ID.N = '35'
*    ID.T = 'A'
*------------------------------------------------------------------------------
*
*
    fieldName = 'INS.COMPANY'
    fieldLength = '35'
    fieldType = "A"
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour) ;
    CALL Field.setCheckFile('REDO.APAP.H.COMP.NAME')
*
    fieldName = 'INS.POLICY.TYPE'
    fieldLength = '35'
    fieldType='A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
    CALL Field.setCheckFile("APAP.H.INSURANCE.POLICY.TYPE")
*
    fieldName = 'SEN.POLICY.NUMBER'
    fieldLength = '35'
    fieldType = "A"
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour) ;
*
    fieldName = 'CHARGE'
    fieldLength = '35'
    fieldType='A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
*
    fieldName = 'CLOSING.DATE'
    fieldLength = '8'
    fieldType = 'D'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
*
    fieldName = 'CURRENCY'
    fieldLength = '3'
    fieldType = 'CCY'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
    CALL Field.setCheckFile('CURRENCY')
*
    fieldName = 'TOTAL.VALUE'
    fieldLength = '35'
    fieldType = 'AMT'
    fieldType<2,2> = PAY.DAT.CURRENCY
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
*
    fieldName = 'NET.VALUE'
    fieldLength = '35'
    fieldType = 'AMT'
    fieldType<2,2> = PAY.DAT.CURRENCY
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)

*
    fieldName = 'BASE.COMMISION'
    fieldLength = '35'
    fieldType = 'AMT'
    fieldType<2,2> = PAY.DAT.CURRENCY
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)

    fieldName = 'APAP.COMMISION'
    fieldLength = '35'
    fieldType = 'AMT'
    fieldType<2,2> = PAY.DAT.CURRENCY
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
*
    fieldName = 'ITBIS.VALUE'
    fieldLength = '35'
    fieldType = 'AMT'
    fieldType<2,2> = PAY.DAT.CURRENCY
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
*
    fieldName = 'BASE.AMOUNT'
    fieldLength = '35'
    fieldType = 'AMT'
    fieldType<2,2> = PAY.DAT.CURRENCY
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
*
    fieldName = 'GROSS.COMMISION'
    fieldLength = '35'
    fieldType = 'AMT'
    fieldType<2,2> = PAY.DAT.CURRENCY
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
*
    fieldName = 'NET.COMMISION'
    fieldLength = '35'
    fieldType = 'AMT'
    fieldType<2,2> = PAY.DAT.CURRENCY
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
*
    fieldName = 'ITBIS.COMMISION'
    fieldLength = '35'
    fieldType = 'AMT'
    fieldType<2,2> = PAY.DAT.CURRENCY
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
*
    fieldName = 'PAYMENT.VALUE'
    fieldLength = '35'
    fieldType = 'AMT'
    fieldType<2,2> = PAY.DAT.CURRENCY
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
*
    Y.PAYMENT.TYPE = 'REDO.B2.FT.PAYMENT'
    CALL EB.LOOKUP.LIST(Y.PAYMENT.TYPE)
    fieldName = 'PAYMENT.TYPE'
    fieldLength = '38'
    fieldType = Y.PAYMENT.TYPE
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)
*
    fieldName = 'ACC.DEBIT'
    fieldLength = '20'
    fieldType = 'ACC'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
*
    fieldName = 'ACC.CREDIT'
    fieldLength = '20'
    fieldType = 'ACC'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    CALL Field.setCheckFile("ACCOUNT")

    fieldName = 'XX.BEN.NAME'
    fieldLength = '65'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'INS.CLAS.POLICY'
    fieldLength = '35'
    fieldType='A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName,fieldLength,fieldType,neighbour)

    CALL Table.addReservedField("RESERVED.5")
    CALL Table.addReservedField("RESERVED.4")
    CALL Table.addReservedField("RESERVED.3")
    CALL Table.addReservedField("RESERVED.2")
    CALL Table.addReservedField("RESERVED.1")

    CALL Table.addLocalReferenceField(XX.LOCAL.REF)
    CALL Table.addOverrideField

*
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
