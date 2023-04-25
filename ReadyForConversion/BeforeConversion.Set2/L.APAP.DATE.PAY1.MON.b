*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.DATE.PAY1.MON

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

    FRECUENCIA = R.AA<AA.PS.PAYMENT.FREQ,1>

    FRECUENCIA = CHANGE(FRECUENCIA,"e0Y e1M e0W o","")

    FRECUENCIA = CHANGE(FRECUENCIA,"D e0F","")	

    COMI = FRECUENCIA

END
