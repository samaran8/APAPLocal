SUBROUTINE L.APAP.EXONERA.COM.RETIRO.RT
$INSERT T24.BP I_COMMON
$INSERT T24.BP I_EQUATE
$INSERT T24.BP I_F.TELLER

R.NEW(TT.TE.WAIVE.CHARGES) = 'YES'
R.NEW(TT.TE.CHARGE.ACCOUNT) = ''
R.NEW(TT.TE.CHRG.AMT.LOCAL) = ''

RETURN

END
