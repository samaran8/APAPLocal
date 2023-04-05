* @ValidationCode : MjoxNDA4MjM3Mzc1OkNwMTI1MjoxNjgwNjEwODI5MTgxOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Apr 2023 17:50:29
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
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 04-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
SUBROUTINE REDO.B.NAB.ACCT.HOL.SELECT

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.B.NAB.ACCT.HOL.COMMON
    $INSERT I_F.DATES


    Y.NEX.DATE = R.DATES(EB.DAT.NEXT.WORKING.DAY)
    SEL.CMD = 'SELECT ':FN.REDO.AA.NAB.HISTORY:' WITH NAB.CHANGE.DATE GT ':TODAY:' AND NAB.CHANGE.DATE LT ':Y.NEX.DATE
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,SEL.ERR)

    CALL BATCH.BUILD.LIST('',SEL.LIST)

RETURN

END
