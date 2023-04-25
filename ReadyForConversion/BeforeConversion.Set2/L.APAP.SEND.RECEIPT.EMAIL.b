*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.SEND.RECEIPT.EMAIL
$INSERT T24.BP I_COMMON
$INSERT T24.BP I_EQUATE
$INSERT T24.BP I_F.TELLER

TEXT = 'L.APAP.SEND.RECEIPT.EMAIL'
CURR.NO = 1
CALL STORE.OVERRIDE(CURR.NO)

END
