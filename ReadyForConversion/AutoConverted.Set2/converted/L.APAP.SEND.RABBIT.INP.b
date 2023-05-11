* Version 2 02/06/00 GLOBUS Release No. G11.0.00 29/06/00
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.SEND.RABBIT.INP
*-----------------------------------------------------------------------------
* Modification History

*-----------------------------------------------------------------------------
* Creation: ARCADIO RUIZ
* Creation Date: 2020/05/14
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_TSS.COMMON

    GOSUB PROCESS

RETURN

*-----------------------------------------------------------------------------

PROCESS:


    MAP = "CUST.STATUS.RESPONSE"

    CALL L.APAP.SEND.RABBIT.MASIVE(APPLICATION,PGM.VERSION,ID.NEW,MAP)


RETURN

END
