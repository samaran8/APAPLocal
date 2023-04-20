SUBROUTINE OBTIENE.COMPANY.NAME.TX.ENQ
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.COMPANY
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER

*DEBUG

    Y.COMPANY.NAME = ""
    Y.COMPANY.ID = ""

    Y.TX.ID = O.DATA
    Y.TX.ID = TRIM(Y.TX.ID)
    Y.TX.ID.HIS = Y.TX.ID : ";1"

    IF Y.TX.ID NE "" THEN

        Y.TIPO.TX = Y.TX.ID[1,2]

        IF Y.TIPO.TX EQ "FT" THEN
            FN.FT = "F.FUNDS.TRANSFER$HIS"
            FV.FT = ""
            CALL OPF(FN.FT, FV.FT)

            R.FT = ""; FT.ERROR = ""
            CALL F.READ(FN.FT, Y.TX.ID.HIS, R.FT, FV.FT, FT.ERROR)
            Y.COMPANY.ID = R.FT<FT.CO.CODE>

            IF Y.COMPANY.ID EQ "" THEN
                FN.FT = "F.FUNDS.TRANSFER"
                FV.FT = ""
                CALL OPF(FN.FT, FV.FT)

                R.FT = ""; FT.ERROR = ""
                CALL F.READ(FN.FT, Y.TX.ID, R.FT, FV.FT, FT.ERROR)
                Y.COMPANY.ID = R.FT<FT.CO.CODE>
            END
        END

        IF Y.TIPO.TX EQ "TT" THEN
            FN.TT = "F.TELLER$HIS"
            FV.TT = ""
            CALL OPF(FN.TT, FV.TT)

            R.TT = ""; TT.ERROR = ""
            CALL F.READ(FN.TT, Y.TX.ID.HIS, R.TT, FV.TT, TT.ERROR)
            Y.COMPANY.ID = R.TT<TT.TE.CO.CODE>

            IF Y.COMPANY.ID EQ "" THEN
                FN.TT = "F.TELLER"
                FV.TT = ""
                CALL OPF(FN.TT, FV.TT)

                R.TT = ""; TT.ERROR = ""
                CALL F.READ(FN.TT, Y.TX.ID, R.TT, FV.TT, TT.ERROR)
                Y.COMPANY.ID = R.TT<TT.TE.CO.CODE>
            END
        END


        IF Y.COMPANY.ID NE "" THEN
            IF Y.COMPANY.ID EQ "DO0010001" THEN
                Y.COMPANY.NAME = "OFICINA PRINCIPAL"
            END
            ELSE
                FN.CMP = "F.COMPANY"
                FV.CMP = ""
                CALL OPF(FN.CMP,FV.CMP)

                R.CMP = ""; CMP.ERROR = ""
                CALL CACHE.READ(FN.CMP, Y.COMPANY.ID, R.CMP, CMP.ERROR)
                Y.COMPANY.NAME = R.CMP<EB.COM.COMPANY.NAME>
            END
        END

    END
*DEBUG
    O.DATA = Y.COMPANY.ID : "*" : Y.COMPANY.NAME
RETURN
END
