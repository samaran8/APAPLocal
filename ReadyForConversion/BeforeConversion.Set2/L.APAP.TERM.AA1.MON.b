*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.TERM.AA1.MON

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE

    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $INSERT T24.BP I_F.AA.TERM.AMOUNT


    FN.ACC = "F.ACCOUNT"
    FV.ACC = ""

    Y.ACC.ID = COMI

    CALL OPF(FN.ACC,FV.ACC)
    CALL F.READ(FN.ACC,Y.ACC.ID,R.ACC,FV.ACC,ACC.ERROR)

    ARRANGEMENT.ID = R.ACC<AC.ARRANGEMENT.ID>

    PROP.CLASS     = 'TERM.AMOUNT'
    PROP.NAME      = 'COMMITMENT'
    RET.ERR        = ''
    R.AA = ''
    EFECTIVE.DATE = TODAY
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.ID,PROP.CLASS,PROP.NAME,'','',R.AA,RET.ERR)

    R.AA = RAISE(R.AA)

    PLAZO = R.AA<AA.AMT.TERM>

    PLAZO =  CHANGE(PLAZO,"M"," MESES")
    PLAZO =  CHANGE(PLAZO,"D"," DIAS")
    PLAZO =  CHANGE(PLAZO,"Y"," AÑOS")

    COMI = PLAZO
