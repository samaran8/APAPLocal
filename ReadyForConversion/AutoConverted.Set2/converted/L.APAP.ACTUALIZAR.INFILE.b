*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>57</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ACTUALIZAR.INFILE
*-----------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $INSERT T24.BP I_F.AA.PAYMENT.SCHEDULE
    $INSERT  BP I_F.ST.L.APAP.PRELACION.COVI19.DET
    $INSERT BP I_F.ST.L.APAP.COVI.PRELACIONIII
    $INSERT LAPAP.BP I_L.APAP.VALI.PAY.COVID.COMMON

    GOSUB TABLAS
    GOSUB PROCESS
    RETURN

TABLAS:
    FN.L.APAP.COVI.PRELACIONIII = 'F.ST.L.APAP.COVI.PRELACIONIII'
    FV.L.APAP.COVI.PRELACIONIII = ''
    CALL OPF (FN.L.APAP.COVI.PRELACIONIII,FV.L.APAP.COVI.PRELACIONIII)

    FN.FT = 'F.FUNDS.TRANSFER'
    FV.FT = ''
    CALL OPF (FN.FT,FV.FT)

    FN.AAA = 'F.AA.ARRANGEMENT.ACTIVITY'
    FV.AAA = ''
    CALL OPF (FN.AAA,FV.AAA)

    FN.L.APAP.PRELACION.COVI19.DET = 'F.ST.L.APAP.PRELACION.COVI19.DET'
    FV.L.APAP.PRELACION.COVI19.DET = ''
    CALL OPF (FN.L.APAP.PRELACION.COVI19.DET,FV.L.APAP.PRELACION.COVI19.DET)
    RETURN

PROCESS:
    Y.FECHA.DESMONTE = '20210329'
    GOSUB SET.ACTIVIDAD
    RETURN

SET.ACTIVIDAD:
    SEL.CMD.AAA = "SELECT ":FN.AAA: " WITH ACTIVITY EQ 'LENDING-APPLYPAYMENT-RP.DIR.DEBIT' 'LENDING-APPLYPAYMENT-RP.PAYOFF.CHQ' 'LENDING-APPLYPAYMENT-RP.PAYOFF' AND EFFECTIVE.DATE GT ":Y.FECHA.DESMONTE:" BY ARRANGEMENT"
    CALL EB.READLIST(SEL.CMD.AAA,SEL.LIST.AAA,"",NO.OF.RECS.AAA,ERROR.DETAILS.AAA)
    LOOP
        REMOVE Y.AAA.ID FROM SEL.LIST.AAA SETTING AAA.POS
    WHILE Y.AAA.ID  : AAA.POS
        CALL F.READ(FN.AAA,Y.AAA.ID,R.AAA,FV.AAA,ERROR.AAA)
        CALL F.READ (FN.L.APAP.COVI.PRELACIONIII,R.AAA<AA.ARR.ACT.ARRANGEMENT>,R.L.APAP.COVI.PRELACIONIII,FV.L.APAP.COVI.PRELACIONIII,ERROR.PRELACIONIII)
        IF NOT(R.L.APAP.COVI.PRELACIONIII) THEN
            CONTINUE
        END
        IF R.L.APAP.COVI.PRELACIONIII<ST.L.A76.MONTO.COVI19> EQ 0 THEN
            CONTINUE
        END
        Y.TXN.CONTRACT.ID = ''; Y.TXN.CONTRACT.ID = R.AAA<AA.ARR.ACT.TXN.CONTRACT.ID>

        CALL F.READ(FN.L.APAP.PRELACION.COVI19.DET,Y.TXN.CONTRACT.ID,R.L.APAP.PRELACION.COVI19.DET,FV.L.APAP.PRELACION.COVI19.DET,ERROR.L.APAP.PRELACION.COVI19.DET)
        IF (R.L.APAP.PRELACION.COVI19.DET) THEN
            CONTINUE
        END

        CRT "AGRANDO EL ARRANGEMENT ":R.AAA<AA.ARR.ACT.ARRANGEMENT>
        Y.ACTUALIZADOS<-1> = Y.AAA.ID:"*":R.AAA<AA.ARR.ACT.ARRANGEMENT>
    REPEAT
*Verificar las cancelaciones del mismo dia
    GOSUB GET.CANCELACIONE.FECHA
    OPEN "&SAVEDLISTS&" TO VV.SAVELISTS ELSE STOP "Unable to open SaveLists File"
    WRITE Y.ACTUALIZADOS TO VV.SAVELISTS, "INFILE.PRELACION.txt"
    CALL JOURNAL.UPDATE('')
    CRT "LA CREACION DEL ARCHIVO:INFILE.PRELACION.txt A TERMINADO "
    RETURN
GET.CANCELACIONE.FECHA:
    SEL.CMD.AAA.1 = "SELECT ":FN.AAA: " WITH ACTIVITY EQ 'LENDING-APPLYPAYMENT-RP.PAYOFF.CHQ' 'LENDING-APPLYPAYMENT-RP.PAYOFF' AND EFFECTIVE.DATE EQ ":Y.FECHA.DESMONTE:" BY ARRANGEMENT"
    CALL EB.READLIST(SEL.CMD.AAA.1,SEL.LIST.AAA.1,"",NO.OF.RECS.AAA.1,ERROR.DETAILS.AAA.1)

    LOOP
        REMOVE Y.AAA.ID.1 FROM SEL.LIST.AAA.1 SETTING AAA.POS.1
    WHILE Y.AAA.ID.1  : AAA.POS.1
        CALL F.READ(FN.AAA,Y.AAA.ID.1,R.AAA,FV.AAA,ERROR.AAA)
        CALL F.READ (FN.L.APAP.COVI.PRELACIONIII,R.AAA<AA.ARR.ACT.ARRANGEMENT>,R.L.APAP.COVI.PRELACIONIII,FV.L.APAP.COVI.PRELACIONIII,ERROR.PRELACIONIII)
        IF NOT(R.L.APAP.COVI.PRELACIONIII) THEN
            CONTINUE
        END
        IF R.L.APAP.COVI.PRELACIONIII<ST.L.A76.MONTO.COVI19> EQ 0 THEN
            CONTINUE
        END
        Y.TXN.CONTRACT.ID = ''; Y.TXN.CONTRACT.ID = R.AAA<AA.ARR.ACT.TXN.CONTRACT.ID>

        CALL F.READ(FN.L.APAP.PRELACION.COVI19.DET,Y.TXN.CONTRACT.ID,R.L.APAP.PRELACION.COVI19.DET,FV.L.APAP.PRELACION.COVI19.DET,ERROR.L.APAP.PRELACION.COVI19.DET)
        IF (R.L.APAP.PRELACION.COVI19.DET) THEN
            CALL F.DELETE (FN.L.APAP.PRELACION.COVI19.DET,Y.TXN.CONTRACT.ID)
        END
        CRT "AGRANDO EL ARRANGEMENT ":R.AAA<AA.ARR.ACT.ARRANGEMENT>
        Y.ACTUALIZADOS<-1> = Y.AAA.ID.1:"*":R.AAA<AA.ARR.ACT.ARRANGEMENT>
    REPEAT

    RETURN

END