*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.V.AZ.ACC.VALID
*--------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Program Name : REDO.V.AZ.ACC.VALID
*--------------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*
*  DATE             WHO               DESCRIPTION
* 07-09-2015      V.P.ASHOKKUMAR     INITIAL CREATION
*----------------------------------------------------------------------------

    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.FUNDS.TRANSFER
    $INCLUDE T24.BP I_F.ACCOUNT


    GOSUB INIT
    GOSUB GET.LOC.VALUES
    GOSUB PROCESS
    RETURN

INIT:
*****
    FN.ACCOUNT='F.ACCOUNT';    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    FN.ACCOUNT.H='F.ACCOUNT$HIS';    F.ACCOUNT.H=''
    CALL OPF(FN.ACCOUNT.H,F.ACCOUNT.H)
    Y.FT.ACC.REF = ''
    RETURN

GET.LOC.VALUES:
***************
    LOC.REF.APPL='FUNDS.TRANSFER'
    LOC.REF.FIELDS='L.FT.AZ.TXN.REF'
    LOC.REF.POS=""; L.FT.AZ.TXN.REF.POS = ""
    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,LOC.REF.POS)
    L.FT.AZ.TXN.REF.POS = LOC.REF.POS<1,1>
    RETURN

PROCESS:
********
    Y.FT.ACC.REF = COMI
    IF COMI EQ '' THEN
        RETURN
    END
    YTEMP.VAL = R.NEW(FT.LOCAL.REF)<1,L.FT.AZ.TXN.REF.POS>
    ERR.ACCOUNT = ''; R.ACCOUNT = ''; ERRH.ACCOUNT = ''; YCATEG = ''
    CALL F.READ(FN.ACCOUNT,COMI,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    IF NOT(R.ACCOUNT) THEN
        FT.HIST.ID = COMI
        CALL EB.READ.HISTORY.REC(F.ACCOUNT.H,FT.HIST.ID,R.ACCOUNT,ERRH.ACCOUNT)
    END
    YCATEG = R.ACCOUNT<AC.CATEGORY>
    IF LEN(YCATEG) EQ 4 AND YCATEG[1,1] EQ '3' THEN
        ETEXT = 'EB-REDO.AA.NOT.ALLOWED'
        CALL STORE.END.ERROR
	RETURN
    END
    IF NOT(R.ACCOUNT) THEN
        ETEXT = 'EB-REDO.ACCT.MISS'
        CALL STORE.END.ERROR
        RETURN
    END
    RETURN
END
