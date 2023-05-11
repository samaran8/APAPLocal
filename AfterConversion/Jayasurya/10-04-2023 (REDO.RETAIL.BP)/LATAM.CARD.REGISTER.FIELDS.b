* @ValidationCode : MjoxMjkwOTAwNDEyOkNwMTI1MjoxNjgxMTEyMjk0NTE4OklUU1NCTkc6LTE6LTE6MDowOnRydWU6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 10 Apr 2023 13:08:14
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
SUBROUTINE LATAM.CARD.REGISTER.FIELDS
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
* 19/10/07 - EN_10003543
*            New Template changes
*
* 14/11/07 - BG_100015736
*            Exclude routines that are not released
* Date             Who                 Reference           Description
* 15-OCT-2008    KarthiK             STBP20081015        Initial creation
* 22-Sep-2010    Swaminathan       ODR-2009-12-0264  Changed to R09 standards
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*10-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*10-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*--------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_METHODS.AND.PROPERTIES
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.CURRENCY
    $INSERT I_F.CATEGORY
*-----------------------------------------------------------------------------

    GOSUB DEFINE.FIELDS
RETURN

*======================================================================================================================
DEFINE.FIELDS:

    ID.F = '@ID' ; ID.N = '35' ; ID.T = 'A'

    neighbour = ''
    fieldName = 'XX<DATE'
    fieldLength = '8'
    fieldType = 'D'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-CUSTOMER'
    fieldLength = '35'
    fieldType = 'A'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-CARD.NAME'
    fieldLength = '35'
    fieldType = 'A'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-CARD.NUMBER'
    fieldLength = '15'
    fieldType = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-TYPE.OF.CARD'
    fieldLength = '35'
    fieldType = ''
    fieldType<2> = "PRINCIPAL_ADDITIONAL"
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-PRINCIPAL.CARD'
    fieldLength = '15'
    fieldType = 'A'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-PRINCIPAL.CUST'
    fieldLength = '15'
    fieldType = 'A'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-PRINC.CUST.NAME'
    fieldLength = '35'
    fieldType = 'A'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-XX.ACCOUNTS'
    fieldLength = '15'
    fieldType = 'A'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-CARD.STAGE'
    fieldLength = '18'
    fieldType = ''
    fieldType<2> = "ISSUED_SENT TO EMBOSS_RECEIVED FROM EMBOSSING"
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-EXPIRY.DATE'
    fieldLength = '11'
    fieldType = 'D'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-TYPE.OF.ISSUE'
    fieldLength = '18'
    fieldType = ''
    fieldType<2> = "ISSUE_REISSUE_RENEWAL"
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX-ISSUE.NUMBER'
    fieldLength = '15'
    fieldType = ''
    fieldType<2> = "SAME_DIFFERENT"
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'XX>REISS.REASON'
    fieldLength = '35'
    fieldType = 'A'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'ISSUE.INDICATOR'
    fieldLength = '15'
    fieldType = ''
    fieldType<2> = "ISSUE_REISSUE_RENEWAL"
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

RETURN
*-----------------------------------------------------------------------------
END
