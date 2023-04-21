* @ValidationCode : Mjo3Mjk0NTI5OTQ6Q3AxMjUyOjE2ODIwNzc4MjY1MTM6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 17:20:26
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
$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.ESTATUS.PR.MONITOR
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 21-APR-2023     Conversion tool    R22 Auto conversion       BP Removed in insert file
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.AA.OVERDUE
    $INSERT I_ENQUIRY.COMMON ;*R22 Auto conversion - END

    Y.AA.ARR.ID = COMI
    GOSUB AA.OVERDUE.READ
*---------------
AA.OVERDUE.READ:
**--------------
    Y.ESTADO = ''; Y.CONDICION = ''
    ARRANGEMENT.ID = Y.AA.ARR.ID
    R.AA.OVERDUE   = ''
    PROP.CLASS     = ''
    PROP.NAME      = 'APAP.OVERDUE'
    RET.ERR        = ''
    returnConditions = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.ID,PROP.CLASS,PROP.NAME,'','',returnConditions,RET.ERR)
    R.AA.OVERDUE = RAISE(returnConditions)
    OVERDUE      = R.AA.OVERDUE
    CALL GET.LOC.REF("AA.PRD.DES.AA.OVERDUE","L.LOAN.STATUS.1",Y.POS)
    Y.ESTADO = R.AA.OVERDUE<AA.OD.LOCAL.REF,Y.POS>
    FINDSTR "Normal" IN Y.ESTADO SETTING Ap, Vp THEN
        Y.ESTADO = "Normal"
    END
    FINDSTR "JudicialCollection" IN Y.ESTADO SETTING Ap, Vp THEN
        Y.ESTADO = "JudicialCollection"
    END
    FINDSTR "Restructured" IN Y.ESTADO SETTING Ap, Vp THEN
        Y.ESTADO = "Restructured"
    END
    FINDSTR "Write-off" IN Y.ESTADO SETTING Ap, Vp THEN
        Y.ESTADO = "Write-off"
    END
    COMI = Y.ESTADO
RETURN
END
