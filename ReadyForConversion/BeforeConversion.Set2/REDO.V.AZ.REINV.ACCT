*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.V.AZ.REINV.ACCT
*
* Description: The Input routine to check the Re-Invested Account
* Dev by: V.P.Ashokkumar
*
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.AZ.ACCOUNT
    $INCLUDE TAM.BP I_F.REDO.CHEQUE.PROCESS


    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
*****
    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'; F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    L.TYPE.INT.PAY.POS = ''
    CALL GET.LOC.REF('AZ.ACCOUNT','L.TYPE.INT.PAY',L.TYPE.INT.PAY.POS)
    RETURN

PROCESS:
********
    YAZ.ID = R.NEW(CHQ.PRO.AZ.ACCOUNT)
    ERR.AZ.ACCOUNT = ''; R.AZ.ACCOUNT = ''; YACCT.TYPE = ''
    CALL F.READ(FN.AZ.ACCOUNT,YAZ.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,ERR.AZ.ACCOUNT)
    YACCT.TYPE = R.AZ.ACCOUNT<AZ.LOCAL.REF,L.TYPE.INT.PAY.POS>
    IF YACCT.TYPE NE 'Reinvested' THEN
        AF = CHQ.PRO.AZ.ACCOUNT
        ETEXT = 'AZ-REINV.DEPOSIT.ACCT'
        CALL STORE.END.ERROR
        RETURN
    END
    RETURN
END
