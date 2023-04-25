* Version 2 02/06/00 GLOBUS Release No. G11.0.00 29/06/00
*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.SEND.RABBIT.INP
*-----------------------------------------------------------------------------
* Modification History

*-----------------------------------------------------------------------------
* Creation: ARCADIO RUIZ
* Creation Date: 2020/05/14
*-----------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_TSS.COMMON

    GOSUB PROCESS

    RETURN

*-----------------------------------------------------------------------------

PROCESS:


    MAP = "CUST.STATUS.RESPONSE"

    CALL L.APAP.SEND.RABBIT.MASIVE(APPLICATION,PGM.VERSION,ID.NEW,MAP)


    RETURN

END
