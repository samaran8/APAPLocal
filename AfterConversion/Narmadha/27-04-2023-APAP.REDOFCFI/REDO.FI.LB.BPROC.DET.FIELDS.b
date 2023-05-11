* @ValidationCode : MjoxMDU4OTYzNjgzOkNwMTI1MjoxNjgwNjA3MTMyOTQ5OklUU1M6LTE6LTE6LTY6MTp0cnVlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Apr 2023 16:48:52
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : -6
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FI.LB.BPROC.DET.FIELDS
*-----------------------------------------------------------------------------
* Template for field definitions routine REDO.FI.LB.BPROC.DET.FIELDS
*
* @author avelasco@temenos.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 10/11/10 - C18 ODR-2010-03-0025
*            New Template changes
*  DATE             WHO                   REFERENCE                  
* 05-APRIL-2023      Conversion Tool       R22 Auto Conversion - No changes
* 05-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId("ID.RECORD", T24_String)    ;* Define Table id
*-----------------------------------------------------------------------------
    CALL Table.addField("ID.PROCESO.BATCH", T24_String,Field_NoInput,'')
    CALL Table.addField("ID.PRESTAMO", T24_String,Field_NoInput,'')
    CALL Table.addField("TIPO.PRESTAMO", T24_String,Field_NoInput,'')
    CALL Table.addField("CLIENTE.ID", T24_String,Field_NoInput,'')
    CALL Table.addField("EMPLEADO.ID", T24_String,Field_NoInput,'')
    CALL Table.addField("NOMBRE", T24_String,Field_NoInput,'')
    CALL Table.addField("MONTO.DESCONTAR", T24_Numeric,Field_NoInput,'')
    CALL Table.addField("CUOTAS_VENCIDAS", T24_Numeric,Field_NoInput,'')
    CALL Table.addField("BALANCE", T24_Numeric,Field_NoInput,'')
    CALL Table.addField("INTERES", T24_Numeric,Field_NoInput,'')
    CALL Table.addField("TASA", T24_Numeric,Field_NoInput,'')
    CALL Table.addField("MORA", T24_Numeric,Field_NoInput,'')
    CALL Table.addField("NUEVO.BALANCE", T24_Numeric,Field_NoInput,'')
    CALL Table.addField("MNT.APLICAR", T24_Numeric,'','')
    CALL Table.addOptionsField("EXCLUYO","SI_NO",'','')
    CALL Table.addOptionsField("ESTADO","PAGO_NO.PAGO",Field_NoInput,'')
    CALL Table.addField("ERROR.MSG", T24_String,Field_NoInput,'')
    CALL Table.addField("PAYMENT.REF", T24_String,Field_NoInput,'')
    CALL Table.addField("CAPITAL", T24_Numeric,Field_NoInput,'')
    CALL Table.addField("SEGUROS", T24_Numeric,Field_NoInput,'')
    CALL Table.addReservedField("RESERVED.1")
    CALL Table.addReservedField("RESERVED.2")
    CALL Table.addReservedField("RESERVED.3")
    CALL Table.addReservedField("RESERVED.4")
    CALL Table.addReservedField("RESERVED.5")
    CALL Table.addOverrideField
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
