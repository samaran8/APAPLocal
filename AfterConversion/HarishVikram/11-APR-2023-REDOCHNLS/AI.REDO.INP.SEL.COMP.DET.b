* @ValidationCode : MjotMjk1OTI3MTg2OkNwMTI1MjoxNjgxMTk2NTYwMjc3OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Apr 2023 12:32:40
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
SUBROUTINE AI.REDO.INP.SEL.COMP.DET

*-----------------------------------------------------------------------------
* @author riyasbasha@temenos.com
*-----------------------------------------------------------------------------
* Modification History :
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 11-APR-2023      Conversion tool    R22 Auto conversion       No changes
* 11-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.THIRDPRTY.PARAMETER
    $INSERT I_F.REDO.ADD.THIRDPARTY


    FN.REDO.THIRDPRTY.PARAMETER = 'F.REDO.THIRDPRTY.PARAMETER'
    F.REDO.THIRDPRTY.PARAMETER  = ''
    CALL OPF(FN.REDO.THIRDPRTY.PARAMETER,F.REDO.THIRDPRTY.PARAMETER)
    GOSUB PROCESS

RETURN
*********
PROCESS:
*********

    Y.COMP.NAME  = R.NEW(ARC.TP.COMP.SERV.NAME)
    SEL.CMD.RCO = "SELECT ":FN.REDO.THIRDPRTY.PARAMETER:" WITH COMP.NAME EQ ":Y.COMP.NAME
    CALL EB.READLIST(SEL.CMD.RCO,SEL.LIST,'',NO.OF.REC,SEL.ERR)
    R.NEW(ARC.TP.TYPE.OF.SERVICE) = SEL.LIST<1>
RETURN
*-----------------------------------------------------------------------------
END
