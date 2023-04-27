* @ValidationCode : MjoxMjM0OTUxODc4OkNwMTI1MjoxNjgyNTc1MzcyMzg0OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 27 Apr 2023 11:32:52
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.CUST.PRD.ACI.UPD.PRE.LOAD
*-------------------------------------------------------------------
* New Subroutine
* spliting REDO.B.CUST.PRD.ACI.UPD.select routine as a multi-thread rtn
*
*-------------------------------------------------------------------------------------
*Modification
* Date                  who                   Reference
* 11-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION - No Change
* 11-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION ADDING PACKAGE NAME APAP.REDOBATCH
*-------------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.ACCOUNT.CREDIT.INT
    $INSERT I_F.BASIC.INTEREST
    $INSERT I_F.DATES
    $INSERT I_F.REDO.CUST.PRD.LIST
    $INSERT I_F.REDO.ACC.CR.INT
    $INSERT I_REDO.B.CUST.PRD.ACI.UPD.COMMON
    


*CALL APAP.REDOBATCH.REDO.B.CUST.PRD.ACI.UPD.LOAD  ;*R22 MANUAL CONVERSTION ADDING PACKAGE NAME APAP.REDOBATCH
    CALL APAP.REDOBATCH.RedoBCustPrdAciUpdLoad() ;*R22 MANUAL CONVERSTION ADDING PACKAGE NAME APAP.REDOBATCH


RETURN
