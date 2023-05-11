* @ValidationCode : Mjo2MjUzNDc3OTE6Q3AxMjUyOjE2ODEyMDA5NzcwNTk6SGFyaXNodmlrcmFtQzotMTotMTowOjA6dHJ1ZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 13:46:17
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOCHNLS
SUBROUTINE REDO.CH.PREV.DBCM.FIELDS
*-----------------------------------------------------------------------------

* COMPANY NAME   : APAP
* DEVELOPED BY   : RMONDRAGON
* PROGRAM NAME   : REDO.CH.PREV.DBCM.FIELDS
*-----------------------------------------------------------------------------
* Description : This is the field template definition routine to create the table
* 'REDO.CH.PREV.DBCM'
*-----------------------------------------------------------------------------
* Input/Output :
*--------------------------------------------------
* IN : NA
* OUT : NA
*--------------------------------------------------
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 11-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 11-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
*    CALL Table.defineId("TABLE.NAME.ID", T24_String)        ;* Define Table id

    ID.F = ''
    ID.N = '6'
    ID.T = ''
    ID.T<2> = 'SYSTEM'


*-----------------------------------------------------------------------------
* CALL Table.addField(fieldName, fieldType, args, neighbour) ;* Add a new fields
* CALL Field.setCheckFile(fileName)        ;* Use DEFAULT.ENRICH from SS or just field 1

    CALL Table.addFieldDefinition("IP.ADD","15","A","")       ;* Add a new field
    CALL Table.addFieldDefinition("PORT","6","A","");
    CALL Table.addFieldDefinition("SIDSERV","35","ANY","");
    CALL Table.addFieldDefinition("DB.USER","35","A","");
    CALL Table.addFieldDefinition("DB.PWD","35","A","");
    CALL Table.addFieldDefinition("MSG.IF.DB.ER","100","A","");

* CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour)       ;* Specify Lookup values
* CALL Field.setDefault(defaultValue) ;* Assign default value
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
