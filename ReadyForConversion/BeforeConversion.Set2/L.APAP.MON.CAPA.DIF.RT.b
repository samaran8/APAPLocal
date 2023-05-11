*-----------------------------------------------------------------------------
* <Rating>599</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.MON.CAPA.DIF.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
*---------------------------------------------------------------------------------------------------
*DESCRIPCION: ESTA RUTINA A PARTIR DEL NUMERO DE CUENTA EN CUAL VIENE EN LA VARIABLE COMI,
*LEERA FBNK.ACCOUNT & FBNK.ACCOUNT$HIS BUSCANDO EL DIFERENCIAL DE CAMPOS ENTRE EL VIVO Y EL HIS.
*EL RESULTADO SI APLICA SERA DEVUELTO EN LA VARIABLE COMI.
*---------------------------------------------------------------------------------------------------
    GOSUB PRIMERO
    GOSUB SEGUNDO
    GOSUB TERCERO

    RETURN

PRIMERO:
    Y.ACCOUNT = COMI          ;*COMI DEBERIA TENER EL NUMERO DE CUENTA COMO VALOR...
    FN.ACC  = "F.ACCOUNT"
    F.ACC  = ""
    R.ACC  = ""
    ACC.ERR = ""
    FN.ACC.H  = "F.ACCOUNT$HIS"
    F.ACC.H  = ""
    R.ACC.H  = ""
    ACC.ERR.H = ""

    RETURN

SEGUNDO:
    CALL OPF(FN.ACC,F.ACC)
    CALL OPF(FN.ACC.H,F.ACC.H)
    Y.APPL = 'ACCOUNT'
    Y.FLD = 'L.AC.SRCINITDEP' : VM : 'L.AC.TOT.DEP' : VM : 'L.AC.CASH.DEP' : VM : 'L.AC.CHQ.DEP' : VM : 'L.AC.TRANS.DEP' : VM : 'L.AC.TOT.WITHDR' : VM
    Y.FLD := 'L.AC.CASHWITHDR' : VM : 'L.AC.CHQWITHDR' : VM : 'L.AC.TRANS.WDR' : VM : 'L.AC.QTY.DEPOS' : VM : 'L.AC.QTY.WITHDR' : VM : 'L.AC.PROP.USE': VM
    Y.FLD := 'L.AC.OTHER.DETS' : VM : 'L.AC.FUNDORIGIN' : VM : 'L.AC.ALPH.AC.NO' : VM : 'L.AC.STD.ACC.NO' : VM : 'L.AC.CHEK.DIGIT' : VM : 'L.AC.STATUS1' : VM
    Y.FLD := 'L.AC.STATUS2' : VM : 'L.AC.NOTIFY.1' : VM : 'L.AC.NOTIFY.2' : VM : 'L.DATE.INT.UPD' : VM : 'L.STAT.INT.RATE' : VM : 'L.AC.MAN.UPD'
    Y.POS = ''
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FLD,Y.POS)
    L.AC.SRCINITDEP.POS = Y.POS<1,1>
    L.AC.TOT.DEP.POS = Y.POS<1,2>
    L.AC.CASH.DEP.POS = Y.POS<1,3>
    L.AC.CHQ.DEP.POS = Y.POS<1,4>
    L.AC.TRANS.DEP.POS = Y.POS<1,5>
    L.AC.TOT.WITHDR.POS = Y.POS<1,6>
    L.AC.CASHWITHDR.POS = Y.POS<1,7>
    L.AC.CHQWITHDR.POS = Y.POS<1,8>
    L.AC.TRANS.WDR.POS = Y.POS<1,9>
    L.AC.QTY.DEPOS.POS = Y.POS<1,10>
    L.AC.QTY.WITHDR.POS = Y.POS<1,11>
    L.AC.PROP.USE.POS = Y.POS<1,12>
    L.AC.OTHER.DETS.POS = Y.POS<1,13>
    L.AC.FUNDORIGIN = Y.POS<1,14>
    L.AC.ALPH.AC.NO.POS = Y.POS<1,15>
    L.AC.STD.ACC.NO.POS = Y.POS<1,16>
    L.AC.CHEK.DIGIT.POS = Y.POS<1,17>
    L.AC.STATUS1.POS = Y.POS<1,18>
    L.AC.STATUS2.POS = Y.POS<1,19>
    L.AC.NOTIFY.1.POS = Y.POS<1,20>
    L.AC.NOTIFY.2.POS = Y.POS<1,21>
    L.DATE.INT.UPD.POS = Y.POS<1,22>
    L.STAT.INT.RATE.POS = Y.POS<1,23>
    L.AC.MAN.UPD.POS = Y.POS<1,24>

    RETURN

TERCERO:
    Y.CAMBIOS = ''
    Y.HIS.ID = Y.ACCOUNT
    CALL F.READ(FN.ACC,Y.ACCOUNT,R.ACC,F.ACC,ACC.ERR)
    CALL F.READ.HISTORY(FN.ACC.H, Y.HIS.ID, R.ACC.H, F.ACC.H, ACC.ERR.H)

    IF R.ACC THEN

        IF R.ACC<AC.CATEGORY> NE R.ACC.H<AC.CATEGORY> THEN
            Y.CAMBIOS<-1> = "CATEGORY:":R.ACC.H<AC.CATEGORY> : '#' :R.ACC<AC.CATEGORY>
        END
        IF R.ACC<AC.ACCOUNT.TITLE.1> NE R.ACC.H<AC.ACCOUNT.TITLE.1> THEN
            Y.CAMBIOS<-1> = "ACCOUNT TITLE1:":R.ACC.H<AC.ACCOUNT.TITLE.1> : '#' :R.ACC<AC.ACCOUNT.TITLE.1>
        END
        IF R.ACC<AC.ACCOUNT.TITLE.2> NE R.ACC.H<AC.ACCOUNT.TITLE.2> THEN
            Y.CAMBIOS<-1> = "ACCOUNT TITLE2:":R.ACC.H<AC.ACCOUNT.TITLE.2> : '#' :R.ACC<AC.ACCOUNT.TITLE.2>
        END
        IF R.ACC<AC.SHORT.TITLE> NE R.ACC.H<AC.SHORT.TITLE> THEN
            Y.CAMBIOS<-1> = "SHORT TITLE:":R.ACC.H<AC.SHORT.TITLE> : '#' :R.ACC<AC.SHORT.TITLE>
        END
        IF R.ACC<AC.ACCOUNT.OFFICER> NE R.ACC.H<AC.ACCOUNT.OFFICER> THEN
            Y.CAMBIOS<-1> = "ACCOUNT OFFICER:":R.ACC.H<AC.ACCOUNT.OFFICER> : '#' :R.ACC<AC.ACCOUNT.OFFICER>
        END
        IF R.ACC<AC.OTHER.OFFICER> NE R.ACC.H<AC.OTHER.OFFICER> THEN
            Y.CAMBIOS<-1> = "OTHER OFFICER:":R.ACC.H<AC.OTHER.OFFICER> : '#' :R.ACC<AC.OTHER.OFFICER>
        END
        IF R.ACC<AC.JOINT.HOLDER> NE R.ACC.H<AC.JOINT.HOLDER> THEN
            Y.CAMBIOS<-1> = "JOINT HOLDER:":R.ACC.H<AC.JOINT.HOLDER> : '#' :R.ACC<AC.JOINT.HOLDER>
        END
        IF R.ACC<AC.RELATION.CODE> NE R.ACC.H<AC.RELATION.CODE> THEN
            Y.CAMBIOS<-1> = "RELATION CODE:":R.ACC.H<AC.RELATION.CODE> : '#' :R.ACC<AC.RELATION.CODE>
        END
        IF R.ACC<AC.JOINT.NOTES> NE R.ACC.H<AC.JOINT.NOTES> THEN
            Y.CAMBIOS<-1> = "JOINT NOTES:":R.ACC.H<AC.JOINT.NOTES> : '#' :R.ACC<AC.JOINT.NOTES>
        END
        IF R.ACC<AC.ALT.ACCT.ID> NE R.ACC.H<AC.ALT.ACCT.ID> THEN
            Y.ALT.ACCTS = R.ACC<AC.ALT.ACCT.ID>
            Y.ALT.ACCTS = CHANGE(Y.ALT.ACCTS,FM,' ')
            Y.ALT.ACCTS = CHANGE(Y.ALT.ACCTS,VM,' ')
            Y.ALT.ACCTS = CHANGE(Y.ALT.ACCTS,SM,' ')
            Y.ALT.ACCTS.H = R.ACC.H<AC.ALT.ACCT.ID>
            Y.ALT.ACCTS.H = CHANGE(Y.ALT.ACCTS.H,FM,' ')
            Y.ALT.ACCTS.H = CHANGE(Y.ALT.ACCTS.H,VM,' ')
            Y.ALT.ACCTS.H = CHANGE(Y.ALT.ACCTS.H,SM,' ')

            Y.CAMBIOS<-1> = "ALT ACCT ID:":Y.ALT.ACCTS.H : '#' :Y.ALT.ACCTS
        END
        IF R.ACC<AC.INTEREST.LIQU.ACCT> NE R.ACC.H<AC.INTEREST.LIQU.ACCT> THEN
            Y.CAMBIOS<-1> = "INTEREST LIQU ACCT:":R.ACC.H<AC.INTEREST.LIQU.ACCT> : '#' :R.ACC<AC.INTEREST.LIQU.ACCT>
        END
*KYC
        IF R.ACC<AC.LOCAL.REF,L.AC.SRCINITDEP.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.SRCINITDEP.POS> THEN
            Y.CAMBIOS<-1> = "SRCINITDEP:":R.ACC.H<AC.LOCAL.REF,L.AC.SRCINITDEP.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.SRCINITDEP.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.TOT.DEP.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.TOT.DEP.POS> THEN
            Y.CAMBIOS<-1> = "TOT DEP:":R.ACC.H<AC.LOCAL.REF,L.AC.TOT.DEP.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.TOT.DEP.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.CASH.DEP.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.CASH.DEP.POS> THEN
            Y.CAMBIOS<-1> = "CASH DEP:":R.ACC.H<AC.LOCAL.REF,L.AC.CASH.DEP.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.CASH.DEP.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.CHQ.DEP.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.CHQ.DEP.POS> THEN
            Y.CAMBIOS<-1> = "CHQ DEP:":R.ACC.H<AC.LOCAL.REF,L.AC.CHQ.DEP.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.CHQ.DEP.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.TRANS.DEP.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.TRANS.DEP.POS> THEN
            Y.CAMBIOS<-1> = "TRANS DEP:":R.ACC.H<AC.LOCAL.REF,L.AC.TRANS.DEP.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.TRANS.DEP.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.TOT.WITHDR.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.TOT.WITHDR.POS> THEN
            Y.CAMBIOS<-1> = "TOT WITHDR:":R.ACC.H<AC.LOCAL.REF,L.AC.TOT.WITHDR.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.TOT.WITHDR.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.CASHWITHDR.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.CASHWITHDR.POS> THEN
            Y.CAMBIOS<-1> = "CASHWITHDR:":R.ACC.H<AC.LOCAL.REF,L.AC.CASHWITHDR.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.CASHWITHDR.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.CHQWITHDR.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.CHQWITHDR.POS> THEN
            Y.CAMBIOS<-1> = "CHQWITHDR:":R.ACC.H<AC.LOCAL.REF,L.AC.CHQWITHDR.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.CHQWITHDR.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.TRANS.WDR.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.TRANS.WDR.POS> THEN
            Y.CAMBIOS<-1> = "TRANS WDR:":R.ACC.H<AC.LOCAL.REF,L.AC.TRANS.WDR.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.TRANS.WDR.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.QTY.DEPOS.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.QTY.DEPOS.POS> THEN
            Y.CAMBIOS<-1> = "QTY DEPOS:":R.ACC.H<AC.LOCAL.REF,L.AC.QTY.DEPOS.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.QTY.DEPOS.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.QTY.WITHDR.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.QTY.WITHDR.POS> THEN
            Y.CAMBIOS<-1> = "QTY WITHDR:":R.ACC.H<AC.LOCAL.REF,L.AC.QTY.WITHDR.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.QTY.WITHDR.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.PROP.USE.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.PROP.USE.POS> THEN
            Y.CAMBIOS<-1> = "PROP USE:":R.ACC.H<AC.LOCAL.REF,L.AC.PROP.USE.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.PROP.USE.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.OTHER.DETS.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.OTHER.DETS.POS> THEN
            Y.CAMBIOS<-1> = "OTHER DETS:":R.ACC.H<AC.LOCAL.REF,L.AC.OTHER.DETS.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.OTHER.DETS.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.FUNDORIGIN> NE R.ACC.H<AC.LOCAL.REF,L.AC.FUNDORIGIN> THEN
            Y.CAMBIOS<-1> = "FUNDORIGIN:":R.ACC.H<AC.LOCAL.REF,L.AC.FUNDORIGIN> : '#' :R.ACC<AC.LOCAL.REF,L.AC.FUNDORIGIN>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.FUNDORIGIN> NE R.ACC.H<AC.LOCAL.REF,L.AC.FUNDORIGIN> THEN
            Y.CAMBIOS<-1> = "FUNDORIGIN:":R.ACC.H<AC.LOCAL.REF,L.AC.FUNDORIGIN> : '#' :R.ACC<AC.LOCAL.REF,L.AC.FUNDORIGIN>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.ALPH.AC.NO.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.ALPH.AC.NO.POS> THEN
            Y.CAMBIOS<-1> = "ALPH AC NO:":R.ACC.H<AC.LOCAL.REF,L.AC.ALPH.AC.NO.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.ALPH.AC.NO.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.STD.ACC.NO.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.STD.ACC.NO.POS> THEN
            Y.CAMBIOS<-1> = "STD ACC NO:":R.ACC.H<AC.LOCAL.REF,L.AC.STD.ACC.NO.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.STD.ACC.NO.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.CHEK.DIGIT.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.CHEK.DIGIT.POS> THEN
            Y.CAMBIOS<-1> = "CHEK DIGIT:":R.ACC.H<AC.LOCAL.REF,L.AC.CHEK.DIGIT.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.CHEK.DIGIT.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.STATUS1.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.STATUS1.POS> THEN
            Y.CAMBIOS<-1> = "STATUS1:":R.ACC.H<AC.LOCAL.REF,L.AC.STATUS1.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.STATUS1.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.STATUS2.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.STATUS2.POS> THEN
            Y.CAMBIOS<-1> = "STATUS2:":R.ACC.H<AC.LOCAL.REF,L.AC.STATUS2.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.STATUS2.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.NOTIFY.1.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.NOTIFY.1.POS> THEN
            Y.CAMBIOS<-1> = "NOTIFY 1:":R.ACC.H<AC.LOCAL.REF,L.AC.NOTIFY.1.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.NOTIFY.1.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.NOTIFY.2.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.NOTIFY.2.POS> THEN
            Y.CAMBIOS<-1> = "NOTIFY 2:":R.ACC.H<AC.LOCAL.REF,L.AC.NOTIFY.2.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.NOTIFY.2.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.DATE.INT.UPD.POS> NE R.ACC.H<AC.LOCAL.REF,L.DATE.INT.UPD.POS> THEN
            Y.CAMBIOS<-1> = "INT UPD:":R.ACC.H<AC.LOCAL.REF,L.DATE.INT.UPD.POS> : '#' :R.ACC<AC.LOCAL.REF,L.DATE.INT.UPD.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.STAT.INT.RATE.POS> NE R.ACC.H<AC.LOCAL.REF,L.STAT.INT.RATE.POS> THEN
            Y.CAMBIOS<-1> = "INT UPD:":R.ACC.H<AC.LOCAL.REF,L.STAT.INT.RATE.POS> : '#' :R.ACC<AC.LOCAL.REF,L.STAT.INT.RATE.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.STAT.INT.RATE.POS> NE R.ACC.H<AC.LOCAL.REF,L.STAT.INT.RATE.POS> THEN
            Y.CAMBIOS<-1> = "INT RATE:":R.ACC.H<AC.LOCAL.REF,L.STAT.INT.RATE.POS> : '#' :R.ACC<AC.LOCAL.REF,L.STAT.INT.RATE.POS>
        END
        IF R.ACC<AC.LOCAL.REF,L.AC.MAN.UPD.POS> NE R.ACC.H<AC.LOCAL.REF,L.AC.MAN.UPD.POS> THEN
            Y.CAMBIOS<-1> = "MAN UPD:":R.ACC.H<AC.LOCAL.REF,L.AC.MAN.UPD.POS> : '#' :R.ACC<AC.LOCAL.REF,L.AC.MAN.UPD.POS>
        END


*FIN KYC

        Y.FINAL = CHANGE(Y.CAMBIOS, FM, '!')
        Y.FINAL = CHANGE(Y.FINAL, '.', '$')
        Y.FINAL = CHANGE(Y.FINAL, VM, ' ')
        Y.FINAL = CHANGE(Y.FINAL, @VM, ' ')
        Y.FINAL = CHANGE(Y.FINAL, CHAR(253), ' ')
        COMI = Y.FINAL
    END ELSE
        COMI = 'REGISTRO-CERRADO'
    END


    RETURN

END
