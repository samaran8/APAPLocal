*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.DESM.RUEDA.SELECT

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES
    $INSERT T24.BP I_TSA.COMMON
    $INSERT T24.BP I_F.AA.BILL.DETAILS
    $INSERT T24.BP I_F.AA.PAYMENT.SCHEDULE
    $INSERT T24.BP I_F.AA.ARRANGEMENT
    $INSERT T24.BP I_F.AA.TERM.AMOUNT
    $INSERT TAM.BP I_F.APAP.H.INSURANCE.DETAILS
    $INSERT T24.BP I_F.DATES
    $INSERT LAPAP.BP I_LAPAP.DESM.RUEDA.COMO

*Limpiando tabla temporal
    CALL EB.CLEAR.FILE(FN.LAPAP.DESM.RUEDA,F.LAPAP.DESM.RUEDA)

    R.CHK.DIR = '' ; CHK.DIR.ERROR = ''
    CALL F.READ(FN.CHK.DIR,Y.ARCHIVO.CARGA,R.CHK.DIR,F.CHK.DIR,CHK.DIR.ERROR)
    IF NOT(R.CHK.DIR) THEN
        RETURN
    END

    SEL.LIST = R.CHK.DIR;
    CALL BATCH.BUILD.LIST('',SEL.LIST)

    RETURN

END