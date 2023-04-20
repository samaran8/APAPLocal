*-----------------------------------------------------------------------------
* @(#) REDO.B.CUS.TAX.RETENTIONS.SELECT Ported to jBASE 16:17:06  28 NOV 2017
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.B.CUS.TAX.RETENTIONS.SELECT
*---------------------------------------------------------------------------------------------
*
* Description           : This is the Routine used to select the REDO.NCF.ISSUED Application with Based on the Date Value.

* Developed By          : Amaravathi Krithika B
*
* Development Reference : RegN11
*
* Attached To           : Batch - BNK/REDO.B.CUS.TAX.RETENTIONS
*
* Attached As           : Online Batch Routine to COB
*---------------------------------------------------------------------------------------------
* Input Parameter:
*----------------*
* Argument#1 : NA
*
*-----------------*
* Output Parameter:
*-----------------*
* Argument#4 : NA
*
*---------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*---------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
*-----------------------------------------------------------------------------------------------------------------
* PACS00375393           Ashokkumar.V.P                 11/12/2014            New mapping changes - Rewritten the whole source.
* APAP-132               Ashokkumar.V.P                 03/02/2016            Spliting the file based on customer identification.
*---------------------------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES
    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.STMT.ENTRY
    $INSERT LAPAP.BP I_REDO.B.CUS.TAX.RETENTIONS.COMMON
*

    GOSUB SELECT.PROCESS
    RETURN
*
SELECT.PROCESS:
*--------------
    CALL EB.CLEAR.FILE(FN.DR.REG.REGN11.WORKFILE, F.DR.REG.REGN11.WORKFILE)
    YGP.STMT.ID.LIST = ''
    D.FIELDS = "ACCOUNT":FM:"BOOKING.DATE"
    D.LOGICAL.OPERANDS = "1":FM:"2"
    D.RANGE.AND.VALUE = YTAX.ACCT:FM:Y.DATE.FROM:SM:Y.DATE.TO
    CALL E.STMT.ENQ.BY.CONCAT(YGP.STMT.ID.LIST)
    CALL BATCH.BUILD.LIST('',YGP.STMT.ID.LIST)
    RETURN
*---------------------------------------------------------------------------------------------
END
*---------------------------------------------------------------------------------------------
*
