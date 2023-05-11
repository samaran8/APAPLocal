* @ValidationCode : MjotNDc5MDk0NzUyOkNwMTI1MjoxNjgwNjc3MDE3NjM0OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 12:13:37
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.CHQ.UPD.STATUS.SELECT
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.B.CHQ.UPD.STATUS.SELECT
*--------------------------------------------------------------------------------------------------------
*Description       : Multi threaded Select routine used to select the records

*In  Parameter     :
*Out Parameter     : NA
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date               Who                    Reference                 Description
*   ------             -----                 -------------              -------------
* 17 OCT 2011        MARIMUTHU S              PACS00146454             Initial Creation
* 04-APR-2023        Conversion tool         R22 Auto conversion       No changes
* 04-APR-2023      Harishvikram C           Manual R22 conversion      No changes
*********************************************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.B.CHQ.UPD.STATUS.COMMON

MAIN:

    SEL.CMD = 'SELECT ':FN.REDO.LOAN.CHQ.RETURN
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,SEL.ERR)
    CALL BATCH.BUILD.LIST('',SEL.LIST)

RETURN

END
