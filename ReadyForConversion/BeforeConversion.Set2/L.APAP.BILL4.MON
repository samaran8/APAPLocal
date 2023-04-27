*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.BILL4.MON

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE

    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $INSERT T24.BP I_F.AA.PAYMENT.SCHEDULE


    FN.ACC = "F.ACCOUNT"
    FV.ACC = ""

    Y.ACC.ID = COMI

    CALL OPF(FN.ACC,FV.ACC)
    CALL F.READ(FN.ACC,Y.ACC.ID,R.ACC,FV.ACC,ACC.ERROR)

    ARRANGEMENT.ID = R.ACC<AC.ARRANGEMENT.ID>

    PROP.CLASS     = 'PAYMENT.SCHEDULE'
    PROP.NAME      = ''
    RET.ERR        = ''
    R.AA = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.ID,PROP.CLASS,PROP.NAME,'','',R.AA,RET.ERR)

    R.AA = RAISE(R.AA)

    Y.CAN.MULT = R.AA<AA.PS.PAYMENT.TYPE>

    Y.CAN.NUM = DCOUNT(Y.CAN.MULT,@VM)

    FOR I = 1 TO Y.CAN.NUM

        CUOTA.CALC = R.AA<AA.PS.CALC.AMOUNT,I>

        CUOTA.ACT = R.AA<AA.PS.ACTUAL.AMT,I>

        IF CUOTA.ACT NE '' THEN

            CUOTA =  CUOTA.ACT

        END ELSE

            CUOTA = CUOTA.CALC

        END

     Y.CUOTA.TOT = CUOTA + Y.CUOTA.TOT 

    NEXT I


    COMI = Y.CUOTA.TOT

END
