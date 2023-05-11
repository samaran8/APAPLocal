* @ValidationCode : Mjo2MTAyNjQ5MjE6Q3AxMjUyOjE2ODExMjM5NjIzMDk6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 10 Apr 2023 16:22:42
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
$PACKAGE APAP.REDOCHNLS
SUBROUTINE REDO.ATH.VAL.DELAY
**************************************************************************
******************************************************************************
*  Company   Name    :Asociacion Popular de Ahorros y Prestamos
*  Developed By      :DHAMU.S
*  Program   Name    :REDO.ATH.VAL.DELAY
*********************************************************
*Description:This routine is to identify delay in submission of liquidation on
*             based on PURCHASE.DATE and configured days in REDO.H.ATM.PARAMETER
***********************************************************************
*LINKED WITH: NA
*IN PARAMETER: NA
*OUT PARAMETER: REDO.ATH.STLMT.FILE.PROCESS
******************************************************************************
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*03.12.2010   S DHAMU       ODR-2010-08-0469  INITIAL CREATION
* 10-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 10-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*----------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.ATH.STLMT.FILE.PROCESS.COMMON
    $INSERT I_F.REDO.APAP.H.PARAMETER
    $INSERT I_F.ATM.REVERSAL


    GOSUB PROCESS

RETURN


*******
PROCESS:
********


    Y.PARAM.DAYS=R.REDO.APAP.H.PARAMETER<PARAM.LOCK.DAYS>
    ATM.REVERSAL.ID=CARD.NUMBER:'.':Y.FIELD.VALUE
    CALL F.READU(FN.ATM.REVERSAL,ATM.REVERSAL.ID,R.ATM.REVERSAL,F.ATM.REVERSAL,ATM.REV.ERR,'')
    IF R.ATM.REVERSAL EQ '' THEN
*        ERROR.MESSAGE='NOT.VALID.TRANSACTION'
        ERROR.MESSAGE='NO.MATCH.TRANSACTION'
    END


    Y.ATM.REV.LOC.DATE=R.ATM.REVERSAL<AT.REV.T24.DATE>
    YREGION=''
    Y.ADD.DAYS='+':Y.PARAM.DAYS:'C'

*    CALL CDT(YREGION,Y.ATM.REV.LOC.DATE,Y.ADD.DAYS)

*    IF Y.ATM.REV.LOC.DATE LT TODAY THEN
*        ERROR.MESSAGE='DELAY.SUBMISSION'
*    END

RETURN

END
