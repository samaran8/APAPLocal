*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.GET.TXN.COMMENT
$INSERT T24.BP I_COMMON
$INSERT T24.BP I_EQUATE
$INSERT T24.BP I_ENQUIRY.COMMON
$INSERT T24.BP I_F.TELLER
$INSERT T24.BP I_F.FUNDS.TRANSFER
$INSERT USPLATFORM.BP I_F.T24.FUND.SERVICES

Y.TXN.ID = FIELD(O.DATA, "\", 1) 
O.DATA = ""

FINDSTR "FT" IN Y.TXN.ID SETTING Ap, Vp THEN
        
        *PARA ABRIR EL ACHIVO DE FUNDS.TRANSFER
        FN.FT = "F.FUNDS.TRANSFER"
        FV.FT = ""
        RS.FT = ""
        FT.ERR = ""

        CALL OPF(FN.FT, FV.FT)
        CALL F.READ(FN.FT, Y.TXN.ID, RS.FT, FV.FT, FT.ERR)
        O.DATA = RS.FT<FT.PAYMENT.DETAILS>

        IF FT.ERR NE "" THEN

                FN.FT.HIS = 'F.FUNDS.TRANSFER$HIS'
        FV.FT.HIS = ""
        RS.FT.HIS = ""
        FT.ERR.HIS = ""

        CALL OPF(FN.FT.HIS, FV.FT.HIS)
        CALL EB.READ.HISTORY.REC(FV.FT.HIS, Y.TXN.ID, RS.FT.HIS, FT.ERR.HIS)

        O.DATA = RS.FT.HIS<FT.PAYMENT.DETAILS>

    END

END

RETURN

END
