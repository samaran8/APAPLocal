* @ValidationCode : MjotMjU2OTEwMjIzOkNwMTI1MjoxNjgxODA0ODY5ODI2OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 18 Apr 2023 13:31:09
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.RAISE.REPAY.ACCOUNTING.SELECT

*DESCRIPTION:
*------------
* This is the COB routine for CR-41.
*
* This will select the IDs from the REDO.NAB.ACCOUNTING file.
*
* Input/Output:
*--------------
* IN : -NA-
* OUT : -NA-
*---------------
*-----------------------------------------------------------------------------------------------------------------
* Modification History :
*   Date            Who                   Reference                         Description
*   ------         ------               -------------                     -------------
* 05 Dec 2011    Ravikiran AV              CR.41                         Initial Creation
*18-04-2023      Conversion Tool        R22 Auto Code conversion          No Changes
*18-04-2023       Samaran T               R22 Manual Code Conversion       No Changes
*
*------------------------------------------------------------------------------------------------------------------
* All File INSERTS done here
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.RAISE.REPAY.ACCOUNTING.COMMON

*------------------------------------------------------------------------------------------------------------------
*Main Logic of the routine
*
MAIN.LOGIC:

    GOSUB PROCESS

RETURN
*------------------------------------------------------------------------------------------------------------------
* Load the Arrangement ids for Multi-Threaded Processing
*
PROCESS:

    SELECT.CMD = "SELECT ":FN.REDO.REPAID.INT
    CALL EB.READLIST(SELECT.CMD,SEL.LIST,'',NO.REC,PGM.ERR)

    CALL BATCH.BUILD.LIST('', SEL.LIST)

RETURN
*------------------------------------------------------------------------------------------------------------------
END
