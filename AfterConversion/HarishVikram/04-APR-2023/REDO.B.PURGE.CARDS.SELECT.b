* @ValidationCode : MjoxNzkwMDMyNTQ6Q3AxMjUyOjE2ODA2MTEzNzQ4NDI6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 04 Apr 2023 17:59:34
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
SUBROUTINE REDO.B.PURGE.CARDS.SELECT
*********************************************************************************************************
*Company   Name    : APAP Bank
*Developed By      : Temenos Application Management
*Program   Name    : REDO.B.PURGE.CARDS.SELECT
*--------------------------------------------------------------------------------------------------------
*Description       :  This is a Multi threaded Select Routine Which is used to select LATAM.CARD.ORDER table
*                     with CARD.STATUS equal to '93'
*In Parameter      :
*Out Parameter     :
*Files  Used       :
*
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
*  30/07/2010       REKHA S            ODR-2010-03-0400 B166      Initial Creation
* 04-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.LATAM.CARD.ORDER
    $INSERT I_REDO.B.PURGE.CARDS.COMMON

    GOSUB PROCESS
RETURN

********
PROCESS:
********
    SEL.CMD = 'SELECT ':FN.LATAM.CARD.ORDER:' WITH CARD.STATUS EQ 93'
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,Y.RET.CODE)

    CALL BATCH.BUILD.LIST('',SEL.LIST)

RETURN
END
