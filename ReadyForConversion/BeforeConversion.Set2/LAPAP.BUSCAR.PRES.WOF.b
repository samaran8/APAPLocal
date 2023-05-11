*-----------------------------------------------------------------------------
* <Rating>-31</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.BUSCAR.PRES.WOF
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.AA.ARRANGEMENT

    GOSUB OPEN.FILES
    GOSUB PROCESS


OPEN.FILES:

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    FV.AA.ARRANGEMENT = ''
    CALL OPF (FN.AA.ARRANGEMENT,FV.AA.ARRANGEMENT)
    FN.ACCOUNT = 'F.ACCOUNT'
    FV.ACCOUNT = ''
    CALL OPF (FN.ACCOUNT,FV.ACCOUNT)
    FN.ACCOUNT$HIS = 'F.ACCOUNT$HIS'
    F.ACCOUNT$HIS = ''
    CALL OPF(FN.ACCOUNT$HIS,F.ACCOUNT$HIS)

    FN.REDO.CONCAT.ACC.WOF = 'F.REDO.CONCAT.ACC.WOF'
    F.REDO.CONCAT.ACC.WOF = ''
    CALL OPF(FN.REDO.CONCAT.ACC.WOF,F.REDO.CONCAT.ACC.WOF)

    FN.REDO.CONCAT.ACC.NAB = 'F.REDO.CONCAT.ACC.NAB'
    F.REDO.CONCAT.ACC.NAB  = ''
    CALL OPF(FN.REDO.CONCAT.ACC.NAB,F.REDO.CONCAT.ACC.NAB)


    FN.TEMP.FILE.PATH = '../EXTRACT/MASSIVE.BP/MIG.CLS'
    OPEN FN.TEMP.FILE.PATH TO F.TEMP.FILE.PATH ELSE
        Y.MK.CMD = "mkdir ../EXTRACT/MASSIVE.BP/MIG.CLS"
        EXECUTE Y.MK.CMD
        OPEN FN.TEMP.FILE.PATH TO F.TEMP.FILE.PATH ELSE
        END
    END

    RETURN


PROCESS:
    YTODAY = TODAY
    YFINA.ARRY<-1> = "CLIENTE,AA VIEJO,CONTRATO VIEJO,CUENTA HIJA NAB, CUENTA HIJA DE CASTIGO 1, CUENTA HIJA DE CASTIGO 2,AA NUEVO,CONTRATO NUEVO,ID ALTENO1, ID ALTERNO4, ID ALTERNO5"
    SEL.CMD = " SELECT " : FN.AA.ARRANGEMENT : " WITH PRODUCT.GROUP EQ PRODUCTOS.WOF ":" AND START.DATE EQ " : YTODAY
    CALL EB.READLIST(SEL.CMD, SEL.LIST, '',NO.OF.RECS,SEL.ERR)
    LOOP
        REMOVE Y.IN.ID FROM SEL.LIST SETTING AA.POS
    WHILE Y.IN.ID DO
        CALL F.READ(FN.AA.ARRANGEMENT,Y.IN.ID,R.AA.ARRANGEMENT,FV.AA.ARRANGEMENT,ERROR.AA.ARRANGEMENT)
        Y.PRESTAMO.NUEVO = R.AA.ARRANGEMENT<AA.ARR.LINKED.APPL.ID>
        Y.AA.PRESTAMO.NUEVO = Y.IN.ID
        Y.CODIGO.CLIENTE = R.AA.ARRANGEMENT<AA.ARR.CUSTOMER>
        CALL F.READ(FN.ACCOUNT,Y.PRESTAMO.NUEVO,R.ACCOUNT,FV.ACCOUNT,ERR.ACCOUNT)
        Y.ALT.ACCT.TYPE = R.ACCOUNT<AC.ALT.ACCT.TYPE>

        LOCATE "ALTERNO1" IN Y.ALT.ACCT.TYPE<1,1> SETTING POS.ACC1  THEN
            Y.ALTENO1 = R.ACCOUNT<AC.ALT.ACCT.ID,POS.ACC1>
        END


        LOCATE "ALTERNO2" IN Y.ALT.ACCT.TYPE<1,1> SETTING POS.ACC  THEN
            Y.ALTENO2 = R.ACCOUNT<AC.ALT.ACCT.ID,POS.ACC>
            Y.ALTENO2 = Y.ALTENO2[3,LEN(Y.ALTENO2)]
        END

        LOCATE "ALTERNO3" IN Y.ALT.ACCT.TYPE<1,1> SETTING POS.ACC  THEN
            Y.ALTENO3 = R.ACCOUNT<AC.ALT.ACCT.ID,POS.ACC>
            Y.ALTENO3  = Y.ALTENO3 [3,LEN(Y.ALTENO3)]
            CALL F.READ(FN.ACCOUNT,Y.ALTENO3,R.ACCOUNT,FV.ACCOUNT,ERR.ACCOUNT)
            IF NOT (R.ACCOUNT) THEN
                CALL EB.READ.HISTORY.REC(FN.ACCOUNT$HIS,Y.ALTENO3,R.ACCOUNT$HIS,ERR.TELLERHIS)
                R.ACCOUNT = R.ACCOUNT$HIS
            END
            Y.PRESTAMO.VIEJO = Y.ALTENO3
            Y.AA.PRESTMO.VIEJO = R.ACCOUNT<AC.ARRANGEMENT.ID>
            GOSUB READ.CUENTAS.HIJA
        END
        YFINA.ARRY<-1> = Y.CODIGO.CLIENTE:",":Y.AA.PRESTMO.VIEJO:",":Y.PRESTAMO.VIEJO:",":Y.CUENTA.HIJA.NAB:",":Y.CUENTA.WOF1:",":Y.CUENTA.WOF2:",":Y.AA.PRESTAMO.NUEVO:",":Y.PRESTAMO.NUEVO:",":Y.ALTENO1:",":Y.ALTENO2:",":Y.ALTENO3
    REPEAT

    YTMP.FILE = "MIG.CONTRATOS.WOF.":YTODAY:".csv"
    WRITE YFINA.ARRY TO F.TEMP.FILE.PATH,YTMP.FILE

    RETURN

READ.CUENTAS.HIJA:
    CNCT.ERR = ''; R.REDO.CONCAT.ACC.NAB = ''; Y.NAB.AMT = ''; Y.CUENTA.HIJA.NAB = ''
    CALL F.READ(FN.REDO.CONCAT.ACC.NAB,Y.PRESTAMO.VIEJO,R.REDO.CONCAT.ACC.NAB,F.REDO.CONCAT.ACC.NAB,CNCT.ERR)
    IF R.REDO.CONCAT.ACC.NAB THEN
        Y.CUENTA.HIJA.NAB = R.REDO.CONCAT.ACC.NAB<1>
    END
    R.REDO.CONCAT.ACC.WOF = ''; WOF.ERR = '';  Y.CUENTA.WOF1 = ''; Y.CUENTA.WOF2 = '';
    CALL F.READ(FN.REDO.CONCAT.ACC.WOF,Y.PRESTAMO.VIEJO,R.REDO.CONCAT.ACC.WOF,F.REDO.CONCAT.ACC.WOF,WOF.ERR)
    IF R.REDO.CONCAT.ACC.WOF THEN
        Y.CUENTA.WOF1 = R.REDO.CONCAT.ACC.WOF<1>
        Y.CUENTA.WOF2 = R.REDO.CONCAT.ACC.WOF<2>
    END

    RETURN

END
