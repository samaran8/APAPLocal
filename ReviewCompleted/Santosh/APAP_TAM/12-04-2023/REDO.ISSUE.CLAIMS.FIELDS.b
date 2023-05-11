$PACKAGE APAP.TAM
SUBROUTINE REDO.ISSUE.CLAIMS.FIELDS
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
*  DATE             WHO                DESCRIPTION         REFERENCE
*  21-07-2010       SUDHARSANAN S      INITIAL CREATION   ODR-2009-12-0283
*  01-MAR-2010      PRABHU             HD1100464          Mandatory requirment removed from STATUS since it is no input
*  10-MAR-2010      PRABHU             HD1100441          EB.LOOKUP ADDED FOR SER.AGR.PERF,STATUS AND CLOSED STATUS
*  26-MAY-2011      PRADEEP S          PACS00071941       All the fields are made as non-mandatory
*                                                         HOT.FIELD property removed for all fields
*                                                         Char type for Claim.Description & Remark fields changed to TEXT
*                                                         Vetting value changed from INTERACCION to QUESJAS for the field TYPE
*                                                         Vetting Value changed to PRODUCT.TYPE field.Linked to REDO.U.CRM.PRODUCT.TYPE
*                                                         Vetting Value changed to CLAIM.TYPE field.Linked to REDO.U.CRM.CLAIM.TYPE
*                                                         Field STATUS made as inputtable field
*                                                         Used the Reserved Field #19,#18 for OBSERVATIONS & CARD.NO
*                                                         Used the Reserved Field #17,#16 for DOC.NAME & DOC.REV
*                                                         Used the Reserved Field #15,#14 for AMT.PAID & INT.ACCT.NO
*                                                         Used the Reserved Field #13 for TXN.RATE
* 09-AUG-2011      RIYAS              PACS00104863        EB.LOOKUP ADDED DISTRIB.CHANNEL,OPENING.CHANNEL,SER.AGR.COMP FIELDS
** 12-04-2023 R22 Auto Conversion no changes
** 12-04-2023 Skanda R22 Manual Conversion - No changes
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
    C$NS.OPERATION='ALL'
    ID.F = '@ID' ; ID.N = '35'
    ID.T = 'A'   ;
*------------------------------------------------------------------------------
    fieldName='CUSTOMER.CODE'
    fieldLength='35'
    fieldType='ANY'
*fieldType<9>='HOT.FIELD'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
*CALL Field.setCheckFile("CUSTOMER")

    fieldName='ACCOUNT.ID'
    fieldLength='18'
    fieldType='A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    CALL Field.setCheckFile("ACCOUNT")

    fieldName='CURRENCY'
    fieldLength='3'
    fieldType='A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    CALL Field.setCheckFile("CURRENCY")

    fieldName='PRODUCT.TYPE'
    fieldLength='50'
    fieldType='A'
*fieldType<2>=' _PRESTAMOS_CERTIFICADOS_CUENTA.CORRIENTE_TARJETA.DE.CREDITO_OTROS_CAJA.DE.SEGURIDAD_CUENTA.DE.AHORROS'
*fieldType<9>='HOT.FIELD'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    CALL Field.setCheckFile("REDO.U.CRM.PRODUCT.TYPE")

*virtualTableName='CL.PRODUCT.TYPE'
*CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour)

    fieldName='OPENING.CHANNEL'
    fieldLength='20'
    fieldType=''
*   fieldType<2>='BRANCH_CONTACT.CENTRE_APAENLINEA'
    neighbour=''
*   CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    virtualTableName='OPENING.CHANNEL'
    CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour)

    fieldName    = 'OPENING.DATE'
    fieldLength  = '8'
    fieldType    = 'D'
    fieldType<3> = 'NOINPUT'
    neighbour    = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='RECEPTION.TIME'
    fieldLength='20'
    fieldType='A'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'PAYROLL'
    fieldLength = '35'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'COMPANY.PAYROLL'
    fieldLength = '35'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='AUTHORISATION.NO'
    fieldLength='10'
    fieldType=''
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='BUSINESS.NAME'
    fieldLength='35'
    fieldType='A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='TYPE'
    fieldLength='35'
    fieldType=''
    fieldType<2>='RECLAMACION_SOLICITUD_QUEJAS'
*fieldType<9>='HOT.FIELD'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'SLA.ID'
    fieldLength = '35'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'CLAIM.TYPE'
    fieldLength = '35'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    CALL Field.setCheckFile("REDO.U.CRM.CLAIM.TYPE")

    fieldName    = 'DISTRIB.CHANNEL'
    fieldLength  = '35'
    fieldType    = ''
*    fieldType<2> = ' _E BANKING_Cajeros Automaticos (ATM)_Interactive Voice Response(IVR)_Ventanilla_Proceso Interno Automatizado_Proceso Interno por Error Manual_Punto de Servicio Movil_ACH'
    neighbour    = ''
*    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    virtualTableName='DISTRIB.CHANNEL'
    CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour)

    fieldName = 'RISK.LEVEL'
    fieldLength = '6'
    fieldType = 'A'
    neighbour = ''
*    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    virtualTableName='RISK.LEVEL'
    CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour)

    fieldName = 'LEGAL.REP'
    fieldLength = '30'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='APPLICATION.ID'
    fieldLength='26'
    fieldType='A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='TRANSACTION.ID'
    fieldLength='50'
    fieldType='A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='TRANSACTION.AMOUNT'
    fieldLength='26'
    fieldType='AMT'
*
    fieldType<2,2> = '3'
*
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName   = 'CHARGE.KEY'
    fieldLength = '16'
    fieldType   = 'A'
    neighbour   = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    CALL Field.setCheckFile("FT.COMMISSION.TYPE")

    fieldName = 'AMOUNT.CLAIM'
    fieldLength = '26'
    fieldType = 'AMT'
*
    fieldType<2,2> = '3'
*
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName   = 'TRANSACTION.DATE'
    fieldLength = '8.1'
    fieldType   = 'D'
*fieldType<9>= 'HOT.FIELD'
    neighbour   = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'DATE.RESOLUTION'
    fieldLength = '8'
    fieldType = 'D'
*fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'SUPPORT.GROUP'
    fieldLength = '50'
    fieldType = ''
*FieldType<2> = ' _CONTACT.CENTER_PAYMENTS.ADMINISTRATION_SPECIAL.PROGRAMS_LENDING.OPERATIONS_FUNDING.MANAGEMENT_COLLECTION_INTERCHANGE_RETENTION_PRODUCT.SECURITY_LEGAL.DEPARTMENT'
    neighbour = ''
*    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
*    CALL Field.setCheckFile("PW.PARTICIPANT")
    virtualTableName='SUPPORT.GROUP'
    CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour)

    fieldName   = 'RATE.TXN.DATE'
    fieldLength = '8'
    fieldType   = 'D'
    neighbour   = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='STATUS'
    fieldLength='50'
    fieldType=''
*    fieldType<3> = 'NOINPUT'
*    fieldType<2> = ' _OPEN_IN PROCESS_RESOLVED ACCEPTED_RESOLVED REJECTED_RESOLVED NOTIFIED_PENDING INTERNAL NOTIFICATION_PENDING APPROVAL_PENDING DOCUMENTATION_CLOSED FOR LACK OF DOCUMENTATION_CLOSED'
    neighbour=''
*    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    virtualTableName='CM.STATUS'
    CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour)

    fieldName='SER.AGR.PERF'
    fieldLength='50'
    fieldType=''
*fieldType<9> = 'HOT.FIELD'
*    fieldType<2> = ' _OPEN_IN PROCESS_RESOLVED ACCEPTED_RESOLVED REJECTED_RESOLVED NOTIFIED_PENDING INTERNAL NOTIFICATION_PENDING APPROVAL_PENDING DOCUMENTATION_CLOSED FOR LACK OF DOCUMENTATION_CLOSED'
    neighbour=''
*    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    virtualTableName='CL.SER.AGR.PERF'
    CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour)

    fieldName='DATE.NOTIFICATION'
    fieldLength='8'
    fieldType='D'
    fieldType<3> = 'NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'SER.AGR.COMP'
    fieldLength = '50'
    fieldType = ''
* fieldType<2> = 'EXPIRED_ON TIME'
* fieldType<3>='NOINPUT'
    neighbour = ''
*CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    virtualTableName='SER.AGR.COMP'
    CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour)

    fieldName = 'CLAIM.DESCRIP'
    fieldLength = '50'
    fieldType = 'TEXT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='CLOSING.STATUS'
    fieldLength='50'
    fieldType=''
*    fieldType<2> = ' _ACCEPTED_REJECTED_VOID_BY PENDING DOCUMENTATION_REJECTED BY CUSTOMER'
    neighbour=''
*    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    virtualTableName='CL.CLOSING.STATUS'
    CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour)

    fieldName='CLOSING.REMARKS'
    fieldLength='50'
    fieldType='TEXT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'INTERNAL.REMARKS'
    fieldLength = '50'
    fieldType = 'TEXT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'USER.REMARKS'
    fieldLength = '50'
    fieldType = 'TEXT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='COST.CENTRE'
    fieldLength='20'
    fieldType=''
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='AUTHORISED.BY'
    fieldLength='20'
    fieldType='A'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'CLIENT.CONTACTED'
    fieldLength = '3'
    fieldType = ''
    fieldType<2> = 'YES_NO'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'XX.NOTES'
    fieldLength = '50'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='CLOSING.DATE'
    fieldLength='8'
    fieldType='D'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='CLOSE.NOTIFICATION'
    fieldLength='3'
    fieldType=''
    fieldType<2>='YES_NO'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='CUST.ID.NUMBER'
    fieldLength='20'
    fieldType='A'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'GIVEN.NAMES'
    fieldLength = '35'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'FAMILY.NAMES'
    fieldLength = '35'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'SOCIAL.NAME'
    fieldLength = '35'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'XX.RESIDENCE.TYPE'
    fieldLength = '10'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'RESIDENCE'
    fieldLength = '26'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'TOWN.COUNTRY'
    fieldLength = '26'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'COUNTRY'
    fieldLength = '20'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='L.CU.RES.SECTOR'
    fieldLength='35'
    fieldType='A'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    CALL Field.setCheckFile("REDO.SECTOR.DOMI")

    fieldName = 'L.CU.URB.ENS.REC'
    fieldLength = '35'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='STREET'
    fieldLength='35'
    fieldType='A'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='XX.ADDRESS'
    fieldLength='35'
    fieldType='A'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='XX.OFF.PHONE'
    fieldLength='35'
    fieldType='A'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='POST.CODE'
    fieldLength='10'
    fieldType='A'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)


    fieldName='XX<L.CU.TEL.TYPE'
    fieldLength='35'
    fieldType='A'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='XX-L.CU.TEL.AREA'
    fieldLength='35'
    fieldType='A'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'XX-L.CU.TEL.NO'
    fieldLength = '35'
    fieldType = ''
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'XX-L.CU.TEL.EXT'
    fieldLength = '35'
    fieldType = ''
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='XX>L.CU.TEL.P.CONT'
    fieldLength='35'
    fieldType='A'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName='XX.EMAIL'
    fieldLength='35'
    fieldType='A'
    fieldType<3>='NOINPUT'
    neighbour=''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'BRANCH'
    fieldLength = '35'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'ACCOUNT.OFFICER'
    fieldLength = '35'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName    = 'DATA.CONFIRMED'
    fieldLength  = '3'
    fieldType    = ''
    fieldType<2> = 'YES_NO'
    neighbour    = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'XX.LOCAL.REF'
    fieldLength = '35'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName    = 'CONTACT.ATTEMPT'
    fieldLength  = '1'
    fieldType    = ''
    neighbour    = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

*PACS00071941 - S
    fieldName = 'OBSERVATIONS'
    fieldLength = '50'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'CARD.NO'
    fieldLength = '20'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    fieldName = 'XX<DOC.NAME'
    fieldLength = '35'
    fieldType = 'A'
    fieldType<3>='NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    CALL Field.setCheckFile("REDO.U.CRM.DOC.TYPE")

    neighbour = ''
    fieldName = 'XX>DOC.REV'
    fieldLength = '3'
    fieldOptions = 'YES_NO'
    CALL Table.addOptionsField(fieldName,fieldOptions,'','')

    fieldName = 'AMT.PAID'
    fieldLength = '18'
    fieldType = 'AMT'
    fieldType<2,2>='3'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'INT.ACCT.NO'
    fieldLength = '22'
    fieldType = 'ANY'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

    neighbour = ''
    fieldName = 'TXN.RATE'
    fieldLength = '8'
    fieldType = 'R'
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
*PACS00071941 - E

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

    fieldName = 'XX.OVERRIDE'
    fieldLength = '35'
    fieldType<3> = 'NOINPUT'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
