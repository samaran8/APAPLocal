*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.GET.CUS.IDENT(Y.INP.DEAL)
$INCLUDE T24.BP I_COMMON 
$INCLUDE T24.BP I_EQUATE 
$INCLUDE T24.BP I_System 
$INCLUDE T24.BP I_F.COMPANY
$INCLUDE T24.BP I_F.CUSTOMER
$INCLUDE TAM.BP I_F.REDO.L.NCF.STOCK

OUT.ARR = ''

*--PARA ABRIR EL ACHIVO REDO.L.NCF.STOCK
FN.CUS = "FBNK.CUSTOMER"
FV.CUS = ""
RS.CUS = ""
CUS.ERR = ""

CALL OPF(FN.CUS, FV.CUS)
CALL F.READ(FN.CUS, Y.INP.DEAL, RS.CUS, FV.CUS, CUS.ERR)

CALL DR.REG.GET.CUST.TYPE(RS.CUS, OUT.ARR)

Y.INP.DEAL = EREPLACE(OUT.ARR<2>, "-", "")

RETURN

END
