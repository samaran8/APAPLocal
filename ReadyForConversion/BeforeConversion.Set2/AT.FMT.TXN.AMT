*-----------------------------------------------------------------------------
* <Rating>99</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE AT.FMT.TXN.AMT

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CURRENCY
    $INSERT I_F.FUNDS.TRANSFER

    IF MESSAGE NE '' THEN RETURN
    FN.CURRENCY = 'F.CURRENCY'
    CALL OPF(FN.CURRENCY,F.CURRENCY)
    CCY.CODE = R.NEW(FT.DEBIT.CURRENCY)
    CALL F.READ(FN.CURRENCY,CCY.CODE,R.CURRENCY,F.CURRENCY,ERR.CURR)

* Condition added to avoid the error in forming Transaction amount

    IF NOT(R.CURRENCY) THEN
        CCY.CODE = R.NEW(FT.CREDIT.CURRENCY)
        CALL F.READ(FN.CURRENCY,CCY.CODE,R.CURRENCY,F.CURRENCY,ERR.CURR)
    END

    CURR.DEC = R.CURRENCY<EB.CUR.NO.OF.DECIMALS>
    COMI = FIELD(COMI,'.',1)
    DIV.VAL = PWR(10,CURR.DEC)
    COMI = COMI/DIV.VAL
    RETURN
END
