* @ValidationCode : MjoxMDY0ODU4NzI1OkNwMTI1MjoxNjgwMTUzODYzNTQyOklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 30 Mar 2023 10:54:23
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
SUBROUTINE REDO.CONV.AA.DISB.PROCESS.LOAD
*-------------------------------------------------
*Description:
*-------------------------------------------------
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 29-MAR-2023      Conversion Tool       R22 Auto Conversion - No changes
* 29-MAR-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.CONV.AA.DISB.PROCESS.COMMON

******
INIT:
******
*Initialise all the variable

    FN.REDO.DISB.CHAIN = 'F.REDO.DISB.CHAIN'
    F.REDO.DISB.CHAIN = ''
    CALL OPF(FN.REDO.DISB.CHAIN, F.REDO.DISB.CHAIN)

    FN.TEMP.FILE.PATH = '../EXTRACT/MASSIVE.BP/temp1'
    OPEN FN.TEMP.FILE.PATH TO F.TEMP.FILE.PATH ELSE
        Y.MK.CMD = "mkdir ../EXTRACT/MASSIVE.BP/temp1"
        EXECUTE Y.MK.CMD
        OPEN FN.TEMP.FILE.PATH TO F.TEMP.FILE.PATH ELSE
        END
    END

    FN.EXP.FILE.PATH = '../EXTRACT/MASSIVE.BP/CONVERSE.BP'
    OPEN FN.EXP.FILE.PATH TO F.EXP.FILE.PATH ELSE
        Y.MK.CMD = "mkdir ../EXTRACT/MASSIVE.BP/CONVERSE.BP"
        EXECUTE Y.MK.CMD
        OPEN FN.EXP.FILE.PATH TO F.EXP.FILE.PATH ELSE
        END
    END

    Y.DATE = TODAY
    Y.FILE.NAME = 'AA.CONV.':Y.DATE:'.csv'

RETURN
END
