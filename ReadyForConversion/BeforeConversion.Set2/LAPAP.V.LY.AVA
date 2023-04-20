*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.V.LY.AVA
* @(#) LAPAP.V.LY.AVA Ported to jBASE 16:17:00  28 NOV 2017

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT TAM.BP I_F.REDO.LY.POINTS.US
    $INSERT TAM.BP I_F.REDO.LY.POINTS.TOT
    $INSERT TAM.BP I_F.REDO.LY.PROGRAM


    FN.LY = "F.REDO.LY.POINTS.TOT"
    FV.LY= ""
    R.LY = ""
    LY.ERR = ""

    CALL OPF(FN.LY,FV.LY)

    FN.LP = "F.REDO.LY.PROGRAM"
    FV.LP= ""
    R.LP = ""
    LP.ERR = ""

    CALL OPF(FN.LP,FV.LP)


    Y.LY.AMOUNT = COMI

    Y.LY.NO = R.NEW(REDO.PT.US.CUSTOMER.NO) :"C"
    Y.LY.N2 = R.NEW(REDO.PT.US.CUSTOMER.NO)

    Y.ID.PR = R.NEW(REDO.PT.US.PROGRAM)

    Y.TP = R.NEW(REDO.PT.US.QTYORVAL)

    IF Y.LY.NO NE "" THEN

        CALL F.READ(FN.LY,Y.LY.NO,R.LY,FV.LY,LY.ERR)

        CALL F.READ(FN.LP,Y.ID.PR,R.LP,FV.LP,LP.ERR)

        Y.AVA = R.LY<REDO.PT.T.TOT.AVAIL.POINTS>

        Y.TASA = R.LP<REDO.PROG.POINT.VALUE>


        SEL.CMD = "SELECT F.REDO.LY.POINTS.TOT TOT.AVAIL.POINTS WITH @ID LIKE ":Y.LY.N2:"... ":"AND @ID LIKE ...ALL... AND @ID LIKE ...":Y.ID.PR:"..."
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.RECS,SEL.ERR)

        TT = SUM(SEL.LIST)



        IF Y.TP = "Puntos" THEN

            R.NEW(REDO.PT.US.QTY.VALUE) = FMT((Y.LY.AMOUNT * Y.TASA),0)
            R.NEW(REDO.PT.US.QUANTITY) = INT(Y.LY.AMOUNT)

*           DEBUG


        END

        IF Y.TP = "Monto" THEN

            R.NEW(REDO.PT.US.QTY.VALUE) =  INT(Y.LY.AMOUNT)
            R.NEW(REDO.PT.US.QUANTITY) = INT(Y.LY.AMOUNT/Y.TASA)

            Y.LY.AMOUNT = INT(Y.LY.AMOUNT/Y.TASA)

        END


        IF TT < Y.LY.AMOUNT THEN

            ETEXT = "NO PUEDE INGRESAR UNA CANTIDAD SUPERIOR A LO DISPONIBLE "

            CALL STORE.END.ERROR
        END

    END



END
