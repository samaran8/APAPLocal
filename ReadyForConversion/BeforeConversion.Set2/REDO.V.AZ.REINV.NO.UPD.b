*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.V.AZ.REINV.NO.UPD
*
* Description: This INPUT routine is attached to the version- 'AZ.ACCOUNT,NOR.PRECLOSURE.AUTH',
*              'ACCOUNT.CLOSURE,REDO.NAO.AUTH' and 'ACCOUNT.CLOSURE,REDO.NAO.TELLER.AUTH'
*              to update the deposit number in the closure record.
* Dev By     : V.P.Ashokkumar
******************************************************************************
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.ACCOUNT
    $INCLUDE T24.BP I_F.ACCOUNT.CLOSURE

    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
*****
    FN.ACCOUNT = 'F.ACCOUNT'; F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    LOC.REF.APP = 'ACCOUNT.CLOSURE':FM:'ACCOUNT'
    LOC.REF.FIELD = 'L.AC.AZ.ACC.REF':FM:'L.AC.AZ.ACC.REF'
    YLOC.REF = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELD,YLOC.REF)
    POS.AZ.ACC.REF.CLOSE    = YLOC.REF<1,1>
    POS.AZ.ACC.REF          = YLOC.REF<2,1>
    RETURN

PROCESS:
********
    ACC.ERR = ''; R.ACCOUNT = ''
    CALL F.READ(FN.ACCOUNT,ID.NEW,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    IF NOT(R.ACCOUNT) THEN
        RETURN
    END

    R.NEW(AC.ACL.LOCAL.REF)<1,POS.AZ.ACC.REF.CLOSE> = R.ACCOUNT<AC.LOCAL.REF,POS.AZ.ACC.REF>
    RETURN
END
