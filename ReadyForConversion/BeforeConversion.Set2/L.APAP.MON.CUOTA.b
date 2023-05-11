*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.MON.CUOTA

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AA.PAYMENT.SCHEDULE

    ARRANGEMENT.ID = O.DATA
    GOSUB PROCESS

    RETURN

PROCESS:
********
    PROP.CLASS     = 'PAYMENT.SCHEDULE'
    PROP.NAME      = ''
    RET.ERR        = ''
    R.AA           = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.ID,PROP.CLASS,PROP.NAME,'','',R.AA,RET.ERR)

    R.AA = RAISE(R.AA)

    Y.CAN.MULT = R.AA<AA.PS.PAYMENT.TYPE>
    Y.CAN.NUM  = DCOUNT(Y.CAN.MULT,@VM)

    FOR I = 1 TO Y.CAN.NUM
**----------------------------------------------------------
        IF R.AA<AA.PS.PAYMENT.TYPE,I> EQ 'CAPPROG' THEN
            CONTINUE
        END
**---------------------------------------------------------
        CUOTA.CALC = R.AA<AA.PS.CALC.AMOUNT,I>
        CUOTA.ACT  = R.AA<AA.PS.ACTUAL.AMT,I>

        IF CUOTA.ACT NE '' THEN
            CUOTA =  CUOTA.ACT
        END ELSE
            CUOTA = CUOTA.CALC
        END

        Y.CUOTA.TOT = CUOTA + Y.CUOTA.TOT
    NEXT I

    O.DATA = Y.CUOTA.TOT

    RETURN

END
