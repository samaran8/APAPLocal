*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.GET.MOTIVO.RECHAZO.ACH.RT

    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.FUNDS.TRANSFER
    $INCLUDE TAM.BP I_F.REDO.ACH.PROCESS.DET

    Y.RESULT = ''
    Y.NUM.TXN = COMI

    IF SUBSTRINGS(Y.NUM.TXN, 1, 2) EQ 'FT' THEN
        GOSUB INITIALISE
        GOSUB GET.MOTIVO.ACH
    END

    COMI = Y.RESULT

    RETURN

*----------
INITIALISE:
*----------
    FN.REDO.ACH.PROCESS.DET = "F.REDO.ACH.PROCESS.DET"; F.REDO.ACH.PROCESS.DET = ""
    CALL OPF(FN.REDO.ACH.PROCESS.DET, F.REDO.ACH.PROCESS.DET)

    RETURN
*---------------
GET.MOTIVO.ACH:
*---------------

    SEL.LIST = ""; NO.REC = ""; ERR = ""
    SEL.CMD = "SELECT ":FN.REDO.ACH.PROCESS.DET : " WITH T24.TXN.ID EQ " : Y.NUM.TXN
    CALL EB.READLIST(SEL.CMD, SEL.LIST, "", NO.REC, ERR)

    FOR I = 1 TO NO.REC
        R.ACH.DET = ""
        READ R.ACH.DET FROM F.REDO.ACH.PROCESS.DET, SEL.LIST<I> THEN
            Y.MOTIVO = FIELD(R.ACH.DET,FM,8)

            IF Y.MOTIVO THEN
                Y.RESULT = SUBSTRINGS(Y.MOTIVO, 34, 200)
            END

            RETURN
        END
    NEXT I

    RETURN

END
