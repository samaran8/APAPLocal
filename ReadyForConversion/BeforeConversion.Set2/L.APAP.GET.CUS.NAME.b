*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.GET.CUS.NAME(Y.INP.DEAL)
$INCLUDE T24.BP I_COMMON 
$INCLUDE T24.BP I_EQUATE 
$INCLUDE T24.BP I_System 

*--PARA ABRIR EL ACHIVO FBNK.CUSTOMER
CALL REDO.CUST.IDENTITY.REF(Y.INP.DEAL, Y.ALT.ID, Y.CUS.NAME) 

Y.NAME.1         = Y.CUS.NAME[1,35] 
Y.NAME.2         = Y.CUS.NAME[36,LEN(Y.CUS.NAME)]

Y.INP.DEAL = Y.NAME.1 : " " : Y.NAME.2  

RETURN

END
