*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.S.CUSTOMER.INDUSTRY(Y.OUT)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :APAP
*Program   Name    :LAPAP.S.CUSTOMER.SECTOR1
*---------------------------------------------------------------------------------
*DESCRIPTION       : Basa en la logica de la rutina local REDO.S.CUSTOMER.SECTOR
* para obtener la actividad econimica campo L.APAP.INDUSTRY
* ----------------------------------------------------------------------------------
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT TAM.BP I_REDO.DEAL.SLIP.COMMON
    GOSUB PROCESS
    RETURN
*********
PROCESS:
*********
    Y.OUT = VAR.INDUSTRY
    RETURN
END
