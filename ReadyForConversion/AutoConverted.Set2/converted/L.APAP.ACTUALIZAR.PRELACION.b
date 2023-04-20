*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>18</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ACTUALIZAR.PRELACION(Y.ARREGLO.VALUES)
*-----------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES
    $INSERT T24.BP I_TSA.COMMON
    $INSERT T24.BP I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $INSERT T24.BP I_F.AA.PAYMENT.SCHEDULE
    $INSERT  BP I_F.ST.L.APAP.PRELACION.COVI19.DET
    $INSERT BP I_F.ST.L.APAP.COVI.PRELACIONIII
    $INSERT LAPAP.BP I_L.APAP.ACTUALIZAR.PRELACION.COMMON


    GOSUB PROCESS
    RETURN


PROCESS:
    Y.ARREGLO = Y.ARREGLO.VALUES
    Y.ARREGLO  = CHANGE(Y.ARREGLO,"*",FM)
    Y.ARR.ID = Y.ARREGLO<2>
    Y.AAA.ID = Y.ARREGLO<1>
    CALL F.READ(FN.L.APAP.COVI.PRELACIONIII,Y.ARR.ID,R.L.APAP.COVI.PRELACIONIII,FV.L.APAP.COVI.PRELACIONIII,ERROR.COVID)
    GOSUB SET.ACTIVIDAD
    RETURN

SET.ACTIVIDAD:
    Y.INTERES = '';
    CALL L.APAP.MONTO.SOLO.COVID.PR(Y.AAA.ID,Y.INTERES)
    IF Y.INTERES EQ 0 THEN
        RETURN
    END
    GOSUB DELETE.DETAILS.PRELACION
    CALL L.APAP.SET.PAY.COVID.IN(Y.AAA.ID)

    RETURN

DELETE.DETAILS.PRELACION:
    Y.CONTRATO = R.L.APAP.COVI.PRELACIONIII<ST.L.A76.CONTRATO>
    SEL.CMD.DET = "SELECT ":FN.L.APAP.PRELACION.COVI19.DET:" WITH CONTRATO EQ ":Y.CONTRATO
    CALL EB.READLIST(SEL.CMD.DET,SEL.LIST.DET,"",NO.OF.RECS.DET,ERROR.DET)
    LOOP
        REMOVE Y.ID FROM SEL.LIST.DET SETTING AAA.DET.POS
    WHILE Y.ID : AAA.DET.POS
        CALL F.READ(FN.L.APAP.PRELACION.COVI19.DET,Y.ID,R.L.APAP.PRELACION.COVI19.DET,FV.L.APAP.PRELACION.COVI19.DET,ERRO.DET)
        IF NOT (R.L.APAP.PRELACION.COVI19.DET) THEN
            CONTINUE
        END
        IF Y.ID[1,2] EQ 'FT' THEN
            SEL.CMD.AAA = ''; SEL.LIST.AAA = ''; NO.OF.RECS.AAA = ''; ERROR.AAA = '';
            SEL.CMD.AAA = "SELECT ":FN.AAA: " WITH ARRANGEMENT EQ ":Y.ARR.ID: " AND TXN.CONTRACT.ID EQ ":Y.ID
            CALL EB.READLIST(SEL.CMD.AAA,SEL.LIST.AAA,"",NO.OF.RECS.AAA,ERROR.AAA)
            LOOP
                REMOVE Y.RECORD.ID FROM SEL.LIST.AAA SETTING AAA.POST
            WHILE Y.RECORD.ID : AAA.POST
                CALL F.READ (FN.AAA,Y.RECORD.ID,R.AAA,FV.AAA,ERROR.AAA.2)
                Y.VALOR = R.AAA<AA.ARR.ACT.TXN.AMOUNT>
                IF R.AAA<AA.ARR.ACT.TXN.AMOUNT> EQ 0 THEN
                    CALL F.DELETE(FN.L.APAP.PRELACION.COVI19.DET, Y.ID)
                END

            REPEAT
        END

    REPEAT

    RETURN


END
