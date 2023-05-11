* @ValidationCode : MjotMjI1ODExMTg2OkNwMTI1MjoxNjgwNjA3MTMzMDIyOklUU1M6LTE6LTE6LTY6MTp0cnVlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Apr 2023 16:48:53
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
SUBROUTINE REDO.FI.LB.BPROC.FIELDS
*-----------------------------------------------------------------------------
* Template for field definitions routine REDO.FI.LB.BPROC
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
    CALL Table.defineId("ID.PROCESO.BATCH", T24_String)       ;* Define Table id
*-----------------------------------------------------------------------------
    CALL Table.addField("PLANILLA.ID", T24_String,Field_NoInput,'')
    CALL Table.addField("FECHA.CREADO",T24_Date,Field_NoInput,'')
    CALL Table.addField("FECHA.APLICADO",T24_Date,Field_NoInput,'')
    CALL Table.addField("TOTAL.REGISTROS",T24_Numeric,Field_NoInput,'')
    CALL Table.addField("MONTO.TOTAL",T24_Numeric,Field_NoInput,'')
    CALL Table.addField("TOTAL.REG.PROC",T24_Numeric,Field_NoInput,'')
    CALL Table.addField("MONTO.TOTAL.PROC",T24_Numeric,Field_NoInput,'')
    CALL Table.addOptionsField("ESTADO","NO.APLICADO_APLICADO_DESESTIMADO_APROBADO_COMPLETADO",Field_Mandatory,'')
    CALL Table.addField("XX.TOTAL.PROC",T24_Numeric,Field_NoInput,'')
    CALL Table.addField("TRANSACTION.ID", T24_String,'','')
    fieldName="TRAN.AMOUNT"
    fieldLength="16"
    fieldType="AMT"
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)
    CALL Table.addField("ERROR.MSG", T24_String,Field_NoInput,'')
    CALL Table.addField("PAYMENT.REF", T24_String,Field_NoInput,'')
    CALL Table.addField("RET.TXN.REF", T24_String,Field_NoInput,'')
    CALL Table.addField("RET.TAX.REF", T24_String,Field_NoInput,'')
    CALL Table.addField("REJECT.REASON", T24_String,Field_NoInput,'')
    CALL Table.addReservedField("RESERVED.1")
    CALL Table.addReservedField("RESERVED.2")
    CALL Table.addReservedField("RESERVED.3")
    CALL Table.addReservedField("RESERVED.4")

    CALL Table.addOverrideField
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
