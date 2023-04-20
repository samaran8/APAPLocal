* COMPANY NAME    : APAP
*
*----------------------------------------------------------
*
* DESCRIPTION     : BEFORE AUTHORISATION routine to be used in FT versions.
*                   Check to see whether account gets overdrawn.
*------------------------------------------------------------
*
* Modification History :
*-----------------------
*  DATE             WHO                 REFERENCE       DESCRIPTION
*  29.10.2020       APAP                MDA-50153       No permitir que se sobregiren las  cuenta de categoria
*                                                       ahorro
*----------------------------------------------------------------------
*
SUBROUTINE L.APAP.V.OVER.FT.SOBR

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_System
*
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.OVERRIDE
*

    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB PROCESS
*
RETURN
*
* ===
INIT:
* ===
*

    Y.DR.ACCT.NO      = ''
    Y.TXN.CCY         = ''
*
RETURN          ;* Return INIT
*
* =========
OPEN.FILES:
* =========

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.OVERRIDE = 'F.OVERRIDE'
    FV.OVERRIDE = ''
    CALL OPF (FN.OVERRIDE,FV.OVERRIDE)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    FV.FUNDS.TRANSFER = ''
    CALL OPF (FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    CALL GET.LOC.REF("ACCOUNT", "L.AC.AV.BAL",L.AC.AV.BAL.POS)

    CALL GET.LOC.REF("FUNDS.TRANSFER", "L.TT.TAX.CODE",L.TT.TAX.CODE.POS)
    CALL GET.LOC.REF("FUNDS.TRANSFER", "L.TT.TAX.AMT",L.TT.TAX.AMT.POS)

RETURN          ;* Return OPEN.FILES

* ======
PROCESS:
* ======
*
    GOSUB GET.TRANSACTION.INFO

    GOSUB GET.OVERRIDE.MSG

RETURN          ;* Return PROCESS
*

* ===================
GET.TRANSACTION.INFO:
* ===================
*

    Y.FT.ID         = ID.NEW
    Y.DR.ACCT.NO    = R.NEW(FT.DEBIT.ACCT.NO)

RETURN
*
* ===============
GET.OVERRIDE.MSG:
*================
    CALL CACHE.READ(FN.OVERRIDE, 'ACCT.UNAUTH.OD', R.OVERRIDE, ERRO.OVERRIDE)
    Y.EB.OR.MESSAGE = R.OVERRIDE<EB.OR.MESSAGE>
    Y.EB.OR.MESSAGE = Y.EB.OR.MESSAGE<1,3,2>
    CALL F.READ(FN.ACCOUNT,Y.DR.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACC)
    YACCNO = Y.DR.ACCT.NO
    Y.L.AC.AV.BAL = R.ACCOUNT<AC.LOCAL.REF,L.AC.AV.BAL.POS>

    Y.L.TT.TAX.CODE = R.NEW(FT.LOCAL.REF)<1,L.TT.TAX.CODE.POS>
    Y.L.TT.TAX.AMT = R.NEW(FT.LOCAL.REF)<1,L.TT.TAX.AMT.POS>

    IF R.ACCOUNT<AC.CATEGORY> GE 6000 AND R.ACCOUNT<AC.CATEGORY> LE 6599 THEN
        IF Y.L.TT.TAX.CODE EQ 'IMP015%' AND Y.L.AC.AV.BAL LT 0 OR Y.L.AC.AV.BAL LT Y.L.TT.TAX.AMT THEN
            AF = FT.DEBIT.ACCT.NO

            MESSAGE = Y.EB.OR.MESSAGE
            E = MESSAGE
            ETEXT = E
            CALL ERR

        END
    END
RETURN

END
