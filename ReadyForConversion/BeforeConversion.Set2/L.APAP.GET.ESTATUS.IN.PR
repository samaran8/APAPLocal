*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.GET.ESTATUS.IN.PR
    $INSERT T24.BP  I_COMMON
    $INSERT T24.BP  I_EQUATE
    $INSERT T24.BP  I_F.AA.OVERDUE
    $INSERT T24.BP  I_ENQUIRY.COMMON

    Y.AA.ARR.ID = O.DATA
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
    O.DATA = Y.ESTADO
    RETURN
END
