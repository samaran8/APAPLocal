*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ENQ.ARCHIVO.CHQ
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.TELLER
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    Y.CTA.COD = O.DATA
    FN.TT = "F.TELLER"
    FV.TT = ""
    CALL OPF(FN.TT, FV.TT)
    R.TT = ""
    TT.ERR = ""
    FN.FT = "F.FUNDS.TRANSFER"
    FV.FT = ""
    CALL OPF(FN.FT, FV.FT)
    R.FT = ""
    FT.ERR = ""
    CTA.CONTABLE = " "
    CTA.MONEDA = " "
    *DEBUG
    IF (Y.CTA.COD[1,2] EQ "TT") THEN
        CALL F.READ(FN.TT,Y.CTA.COD,R.TT, FV.TT, TT.ERR)
        CTA.CONTABLE = R.TT<TT.TE.ACCOUNT.1>
        CTA.MONEDA = R.FT<TT.TE.CURRENCY.1>
    END
    IF (Y.CTA.COD[1,2] EQ "FT") THEN
        CALL F.READ(FN.FT,Y.CTA.COD,R.FT, FV.FT, FT.ERR)
        CTA.CONTABLE = R.FT<FT.CREDIT.ACCT.NO>
        CTA.MONEDA = R.FT<FT.DEBIT.CURRENCY>

    END

    Y.ARCHIVO.CHEQUE = ""

    Y.ARCHIVO.CHEQUE = "PAGE=PRINT.CHQ.SINGLE.xsl"
    *DEBUG

    IF CTA.MONEDA EQ "USD" THEN
        Y.ARCHIVO.CHEQUE = "PAGE=PRINT.CHQ.SINGLE.USD.xsl"
    END

    IF CTA.CONTABLE EQ "DOP1500500010017" THEN
        Y.ARCHIVO.CHEQUE = "PAGE=PRINT.CHQ.SINGLE.xsl"
    END


    IF CTA.CONTABLE EQ "DOP1500600010017" THEN
        Y.ARCHIVO.CHEQUE = "PAGE=PRINT.CHQ.SINGLE.GOB.xsl"
    END
    *DEBUG 
    O.DATA = Y.ARCHIVO.CHEQUE

    RETURN
END
