*========================================================================
*-----------------------------------------------------------------------------
* <Rating>-24</Rating>
*-----------------------------------------------------------------------------
	SUBROUTINE LAPAP.GET.CUS.BY.IDENT(R.CUSTOMER, CUS.IDENT, IDENT.TYPE, CUS.ID)
	$INSERT T24.BP I_COMMON
	$INSERT T24.BP I_EQUATE
	$INSERT T24.BP I_F.CUSTOMER
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.GET.CUS.BY.IDENT
* Date           : 2018-06-19
* Item ID        : CN008702
*========================================================================
* Brief description :
* -------------------
* Routine that receives the identification of a client and returns the object R.CUSTOMER
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-06-19     Anthony Martinez  Initial Development
*========================================================================

GOSUB PROCESS

PROCESS:
*-------
*--PARA ABRIR EL ACHIVO CUSTOMER
FN.CUS    = "FBNK.CUSTOMER"
F.CUS     = ""
RS.CUS    = ""
CUS.ERR   = ""
SEL.LIST  = ""
SEL.ERR   = ""
NO.OF.REC = ""
Y.FIELD   = ""

BEGIN CASE
CASE IDENT.TYPE EQ 'CEDULA'
    Y.FIELD = 'L.CU.CIDENT'
CASE IDENT.TYPE EQ 'RNC'
    Y.FIELD = 'L.CU.RNC'
CASE IDENT.TYPE EQ 'PASAPORTE'
    Y.FIELD = 'L.CU.PASS.NAT'
END CASE

SEL.CMD = "SELECT ":FN.CUS:" WITH ":Y.FIELD:" EQ ":CUS.IDENT
CALL OPF(FN.CUS, F.CUS)

*--EJECUTAMOS LA CONSULTA A LA TABLA DE CUSTOMER
CALL EB.READLIST(SEL.CMD,SEL.LIST,"",NO.OF.REC,SEL.ERR)
LOOP
    REMOVE Y.CUS.ID FROM SEL.LIST SETTING RTE.POS
WHILE Y.CUS.ID DO
    CALL F.READ(FN.CUS, Y.CUS.ID, RS.CUS, F.CUS, CUS.ERR)
    R.CUSTOMER = RS.CUS
    CUS.ID     = Y.CUS.ID
REPEAT

RETURN
*-------
