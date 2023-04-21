* @ValidationCode : Mjo1MTA0NzIyMjQ6Q3AxMjUyOjE2ODA2MDIwNjA0Njc6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 04 Apr 2023 15:24:20
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
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 04-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.AUDIT.LOG.BULK.SELECT

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.AUDIT.TRAIL.LOG
    $INSERT I_REDO.B.AUDIT.LOG.BULD.COMMON

    SEL.CMD = 'SELECT ':FN.REDO.AUDIT.TRAIL.LOG
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,REC.ERR)
    CALL BATCH.BUILD.LIST('',SEL.LIST)

RETURN

END
