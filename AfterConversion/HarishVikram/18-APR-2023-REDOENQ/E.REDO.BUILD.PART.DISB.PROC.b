* @ValidationCode : MjotMTg0NjkwMTgzMDpDcDEyNTI6MTY4MTgwMDgxMDU3MTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 18 Apr 2023 12:23:30
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
$PACKAGE APAP.REDOENQ
SUBROUTINE E.REDO.BUILD.PART.DISB.PROC(ENQ.DATA)
*
* ====================================================================================
*
*
* ====================================================================================
*
* Subroutine Type : BUILD.ROUTINE
* Attached to     : REDO.E.PART.DESEMBOLSO
* Attached as     : BUILD.ROUTINE
* Primary Purpose :
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : APAP
* Development by  : Sivakumar K
* Date            : 2013-03-19
*
* 18-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 18-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*=======================================================================

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_GTS.COMMON
    $INSERT I_F.AA.ARRANGEMENT
*************************************************************************

    GOSUB INITIALISE
    GOSUB PROCESS

RETURN

*==========
INITIALISE:
*==========

    FN.AA.ARR = 'F.AA.ARRANGEMENT'
    F.AA.ARR = ''
    CALL OPF(FN.AA.ARR,F.AA.ARR)

RETURN

*=======
PROCESS:
*=======

    LOCATE 'ID.ARRANGEMENT' IN ENQ.DATA<2,1> SETTING AA.POS THEN
        CALL F.READ(FN.AA.ARR,ENQ.DATA<4,1>,R.ARRANGEMENT,F.AA.ARR,AA.ARR.ERR)
        IF R.ARRANGEMENT THEN
            ENQ.DATA<2,1> = 'LOAN.AC'
            ENQ.DATA<4,1> = R.ARRANGEMENT<AA.ARR.LINKED.APPL.ID>
        END
    END

RETURN

END
