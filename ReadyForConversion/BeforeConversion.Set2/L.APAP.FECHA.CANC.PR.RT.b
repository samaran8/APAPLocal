*-----------------------------------------------------------------------------
* <Rating>-50</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.FECHA.CANC.PR.RT(ARRANGEMENT, OUT.ARR)
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AA.ARRANGEMENT
    $INSERT T24.BP I_F.AA.ARRANGEMENT.ACTIVITY

*********************************************************************************
* Version: 1.0 A.
* Date:    2018/04/10
* By:      Asociacion Popular Ahorros & Prestamos
* Dept:    Requerimientos T.I. Clientes & Captaciones
* Func:    This routine accept the ARRANGEMENT ID, and return its pay off date.
*********************************************************************************
    GOSUB INITIAL
    GOSUB GET.LIST
    GOSUB LOOP.LIST
    GOSUB SET.FINAL.VALUE
    RETURN

INITIAL:
    FN.AA.A = "FBNK.AA.ARRANGEMENT.ACTIVITY"
    F.AA.A = ""
    R.AA.A = ""
    AA.A.ERR = ""
    CALL OPF(FN.AA.A,F.AA.A)

    Y.CONT.FLAG = "Y"
    RETURN

GET.LIST:
    SEL.CMD.0 = "SELECT " : FN.AA.A : " WITH ARRANGEMENT EQ " : ARRANGEMENT
    CALL EB.READLIST(SEL.CMD.0,SEL.LIST.0,"",NO.OF.RECS.0,SEL.0.ERR)
    RETURN

LOOP.LIST:
    LOOP REMOVE Y.ARRANGEMENT.ACTIVITY.ID FROM SEL.LIST.0 SETTING POS.0
    WHILE Y.ARRANGEMENT.ACTIVITY.ID DO
        GOSUB GET.ARR.ACTIVITY
        IF Y.CONT.FLAG EQ "N" THEN
            BREAK
        END

    REPEAT
    RETURN

GET.ARR.ACTIVITY:
    Y.AA.ARR.ACT.EFFECTIVE.DATE = ''; Y.AA.ARR.ACT.ACTIVITY = ''
    CALL F.READ(FN.AA.A,Y.ARRANGEMENT.ACTIVITY.ID,R.AA.A,F.AA.A,AA.A.ERR)
    Y.AA.ARR.ACT.EFFECTIVE.DATE = R.AA.A<AA.ARR.ACT.EFFECTIVE.DATE>
    Y.AA.ARR.ACT.ACTIVITY = R.AA.A<AA.ARR.ACT.ACTIVITY>
    IF Y.AA.ARR.ACT.ACTIVITY EQ 'LENDING-APPLYPAYMENT-RP.PAYOFF.CHQ' OR Y.AA.ARR.ACT.ACTIVITY EQ 'LENDING-APPLYPAYMENT-RP.PAYOFF' THEN
        Y.CONT.FLAG = "N"
    END
    RETURN

SET.FINAL.VALUE:
    OUT.ARR = Y.AA.ARR.ACT.EFFECTIVE.DATE : FM : Y.AA.ARR.ACT.ACTIVITY
    RETURN
END
