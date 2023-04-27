*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.AMT.ENQ

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE

    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $INSERT T24.BP I_F.AA.TERM.AMOUNT

    Y.ACC.ID = O.DATA

    ARRANGEMENT.ID = Y.ACC.ID

    PROP.CLASS     = 'TERM.AMOUNT'
    PROP.NAME      = ''
    RET.ERR        = ''
    R.AA = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.ID,PROP.CLASS,PROP.NAME,'','',R.AA,RET.ERR)

    R.AA = RAISE(R.AA)

    MONTO = R.AA<AA.AMT.AMOUNT>

     O.DATA  = MONTO

    RETURN

END
