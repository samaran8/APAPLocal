*-----------------------------------------------------------------------------
* <Rating>-62</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.GUARDAR.PRIN.COV
*-----------------------------------------------------------------------------
* Bank name: APAP
* Decription: Rutina atachada a las nuevas versiones de abono de interes covid19 en efectivo y cheque
* la cual no permite realizar el pago mayor al monto de prelacion covid19 pendiente en la tabla
* tabla FBNK.ST.L.APAP.COVI.PRELACIONIII
* Developed By: APAP
* Date:  22/12/2020
*-----------------------------------------------------------------------------
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $INSERT BP I_F.ST.L.APAP.COVI.PRELACIONIII
    $INSERT BP I_F.ST.L.APAP.PRELACION.COVI19.DET
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.VERSION
    $INSERT T24.BP I_F.USER
    GOSUB MAIN.PROCESS
    RETURN
MAIN.PROCESS:
    GOSUB OPEN.TABLA
    GOSUB READ.ACCOUNT
    RETURN

OPEN.TABLA:
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    CALL OPF (FN.ACC,F.ACC)
    FN.PRELACIONIII = 'F.ST.L.APAP.COVI.PRELACIONIII'
    F.PRELACIONIII = ''
    CALL OPF (FN.PRELACIONIII,F.PRELACIONIII)
    FN.L.APAP.PRELACION.COVI19.DET = 'F.ST.L.APAP.PRELACION.COVI19.DET'
    FV.L.APAP.PRELACION.COVI19.DET = ''
    CALL OPF (FN.L.APAP.PRELACION.COVI19.DET,FV.L.APAP.PRELACION.COVI19.DET)

    RETURN

READ.ACCOUNT:
    R.ACC = ''; ERR.ACC = ''
    CALL F.READ(FN.ACC,R.NEW(FT.CREDIT.ACCT.NO),R.ACC,F.ACC,ERR.ACC)
    IF (R.ACC) THEN
        GOSUB READ.PRELACIONIII
    END
    RETURN

READ.PRELACIONIII:
    Y.MONTO.COVID19 = 0; Y.VALOR.CUOTA.PAY = 0
    CALL F.READ(FN.PRELACIONIII,R.ACC<AC.ARRANGEMENT.ID>,R.PRELACIONIII,F.PRELACIONIII,ERROR.PRELACIONIII)
    Y.MONTO.COVID19 = R.PRELACIONIII<ST.L.A76.MONTO.COVI19>
    IF V$FUNCTION EQ 'I' OR V$FUNCTION EQ 'A'  THEN
        GOSUB WRITE.L.APAP.COVID.PRELACIONIII
    END
    RETURN

WRITE.L.APAP.COVID.PRELACIONIII:
********************************
    Y.RESULTADO = 0 ; Y.MONTO.PRINCIPAL.PR = R.NEW(FT.CREDIT.AMOUNT)
    IF  Y.MONTO.COVID19 GT 0 THEN
        Y.MONTO.COVID19.ACTUAL = R.PRELACIONIII<ST.L.A76.MONTO.COVI19>
        Y.RESULTADO = Y.MONTO.COVID19.ACTUAL - Y.MONTO.PRINCIPAL.PR
*R.PRELACIONIII<ST.L.A76.FECHA> = TODAY
*R.PRELACIONIII<ST.L.A76.HORA> = OCONV(TIME(), "MTS")
        R.PRELACIONIII<ST.L.A76.MONTO.COVI19> = Y.RESULTADO
        CALL F.LIVE.WRITE(FN.PRELACIONIII,R.ACC<AC.ARRANGEMENT.ID>,R.PRELACIONIII)
        GOSUB WRITE.L.APAP.PRELACION.COVI19.DET
    END
    RETURN

WRITE.L.APAP.PRELACION.COVI19.DET:
*********************************
    Y.ARRANGEMENT.ID = R.ACC<AC.ARRANGEMENT.ID> ; Y.PRESTAMO = R.NEW(FT.CREDIT.ACCT.NO)
    Y.FT.NEW  = ID.NEW
    R.L.APAP.PRELACION.COVI19.DET = ''
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.ARRANGEMENT> = Y.ARRANGEMENT.ID
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.MONTO.COVI19> = Y.MONTO.COVID19.ACTUAL
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.MONTO.CUOTA> = Y.MONTO.PRINCIPAL.PR
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.CONTRATO> = Y.PRESTAMO
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.MONTO.COVI19.DESPU> = Y.RESULTADO
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.FECHA> = TODAY
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.HORA> = OCONV(TIME(), "MTS")
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.ESTADO> = 'CURR'
*Auditoria campos
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.INPUTTER> = OPERATOR
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.AUTHORISER> = OPERATOR
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.DATE.TIME> = R.PRELACIONIII<ST.L.A76.DATE.TIME>
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.CO.CODE> = ID.COMPANY
    R.L.APAP.PRELACION.COVI19.DET<ST.L.A64.DEPT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>
    CALL F.WRITE(FN.L.APAP.PRELACION.COVI19.DET,Y.FT.NEW,R.L.APAP.PRELACION.COVI19.DET)
    RETURN

END