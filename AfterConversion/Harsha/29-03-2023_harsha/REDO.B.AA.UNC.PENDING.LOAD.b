* @ValidationCode : MjotOTM1NTU0NjQ4OkNwMTI1MjoxNjgwMTUzODYxMzM0OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 30 Mar 2023 10:54:21
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
$PACKAGE APAP.AA
SUBROUTINE REDO.B.AA.UNC.PENDING.LOAD
*-------------------------------------------------
*Description:
*-------------------------------------------------
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE          
* 29-MAR-2023      Conversion Tool       R22 Auto Conversion - No changes        
* 29-MAR-2023      Harsha                R22 Manual Conversion - No changes
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.AA.UNC.PENDING
    $INSERT I_REDO.B.AA.UNC.PENDING.COMMON

    GOSUB PROCESS

RETURN


PROCESS:
*-------

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT = ''
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    FN.REDO.AA.UNC.PENDING = 'F.REDO.AA.UNC.PENDING'
    F.REDO.AA.UNC.PENDING = ''

    CALL OPF(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)
    CALL OPF(FN.REDO.AA.UNC.PENDING,F.REDO.AA.UNC.PENDING)

RETURN
END
