*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.REV.PAGO.FT.PRELAC.LOAD
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $INSERT  BP I_F.ST.L.APAP.PRELACION.COVI19.DET
    $INSERT BP I_F.ST.L.APAP.COVI.PRELACIONIII
    $INSERT LAPAP.BP I_L.APAP.REV.PAGO.FT.PRELAC.COMMON
    GOSUB TABLAS

    RETURN

TABLAS:

    FN.FUNDS.TRANSFER$HIS = 'F.FUNDS.TRANSFER$HIS'
    F.FUNDS.TRANSFER$HIS = ''
    CALL OPF (FN.FUNDS.TRANSFER$HIS,F.FUNDS.TRANSFER$HIS)

    FN.ST.L.APAP.PRELACION.COVID19.DET = 'F.ST.L.APAP.PRELACION.COVI19.DET'
    FV.ST.L.APAP.PRELACION.COVID19.DET =  ''
    CALL OPF (FN.ST.L.APAP.PRELACION.COVID19.DET,FV.ST.L.APAP.PRELACION.COVID19.DET)

    FN.ST.L.APAP.COVID.PRELACIONIII = 'F.ST.L.APAP.COVI.PRELACIONIII'
    FV.ST.L.APAP.COVID.PRELACIONIII = ''
    CALL OPF (FN.ST.L.APAP.COVID.PRELACIONIII,FV.ST.L.APAP.COVID.PRELACIONIII)

    RETURN

END


