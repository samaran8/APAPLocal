* @ValidationCode : MjoyOTM4OTMxMjc6Q3AxMjUyOjE2ODMxMzM0NTA0NDg6SVRTUzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMl9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 03 May 2023 22:34:10
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R22_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
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
    CALL APAP.REDOBATCH.RedoBCustPrdAciUpdLoad()


RETURN
