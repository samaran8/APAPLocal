SUBROUTINE LAPAP.S.CUSTOMER.PJ.NAME(Y.OUT)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :Melvy Martinez
*Program   Name    :LAPAP.S.CUSTOMER.PJ.NAME
*---------------------------------------------------------------------------------
*DESCRIPTION       : Este programa es utilizado para obtener la razon social de los clientes juridicos
* ----------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.DEAL.SLIP.COMMON
    GOSUB PROCESS
RETURN
*********
PROCESS:
*********
    Y.OUT = VAR.SOCIAL[1,30]
RETURN
END
