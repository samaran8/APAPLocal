*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.S.SYS.TIME(Y.OUT)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :SUDHARSANAN S
*Program   Name    :REDO.S.CUSTOMER.ADD3
*---------------------------------------------------------------------------------
*DESCRIPTION       : This program is used to get the current system time
* ----------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
  GOSUB PROCESS
  RETURN
*********
PROCESS:
*********
  CUR.TIME = OCONV(TIME(), "MT")
  Y.OUT = CUR.TIME
  RETURN
END
