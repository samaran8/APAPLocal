* @ValidationCode : Mjo5NDE5MzM1NzQ6Q3AxMjUyOjE2ODI2OTAxMDU5NDA6c2FtYXI6LTE6LTE6MDoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 28 Apr 2023 19:25:05
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.TFS.TRANSACTION.DUP
*--------------------------------------------------
* Description: This is the Validation routine for the TFS to avoid the duplicate
*               of TRANSACTION.
*--------------------------------------------------
* Date          Who              Reference                      Comments
* 14 Apr 2013  H Ganesh         PACS00255601 - TFS ISSUE       Initial Draft
*--------------------------------------------------
*Modification History
*DATE                WHO                         REFERENCE                DESCRIPTION
*19-04-2023       Conversion Tool        R22 Auto Code conversion          No Changes
*19-04-2023       Samaran T               R22 Manual Code Conversion       No Changes
*-----------------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.T24.FUND.SERVICES

    GOSUB PROCESS

RETURN
*--------------------------------------------------
PROCESS:
*--------------------------------------------------


    IF FIELD(OFS$HOT.FIELD,'.',1) EQ 'TRANSACTION' THEN
        R.NEW(TFS.WAIVE.CHARGE)<1,AV> = 'NO'
    END

    LOCATE COMI IN R.NEW(TFS.TRANSACTION)<1,1> SETTING POS THEN
        IF POS NE AV THEN
            ETEXT = 'SC-DUPLICATES.NOT.ALLOW'
            CALL STORE.END.ERROR
        END

    END


RETURN
END
