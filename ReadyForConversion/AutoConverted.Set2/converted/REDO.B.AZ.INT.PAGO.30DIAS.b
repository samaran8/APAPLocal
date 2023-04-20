SUBROUTINE REDO.B.AZ.INT.PAGO.30DIAS
*
* Description: The report to show the accred interest greater than 30 days.
* Dev By : V.P.Ashokkumar
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCR.ACCT.CR
    $INSERT I_F.REDO.H.REPORTS.PARAM


    GOSUB INIT
    GOSUB READ.PARAM
    GOSUB PROCESS
RETURN

INIT:
*****
    YFIN.ARRY = ''
    YPARAM.ID = 'REDO.OPER.ENQ'
    FN.ACCR.ACCT.CR = 'F.ACCR.ACCT.CR'; F.ACCR.ACCT.CR = ''
    CALL OPF(FN.ACCR.ACCT.CR,F.ACCR.ACCT.CR)
    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'; F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)
    FN.REDO.H.REPORTS.PARAM = 'F.REDO.H.REPORTS.PARAM'; F.REDO.H.REPORTS.PARAM = ''
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)
RETURN

READ.PARAM:
***********
    ERR.REDO.H.REPORTS.PARAM = ''; R.REDO.H.REPORTS.PARAM = ''
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,YPARAM.ID,R.REDO.H.REPORTS.PARAM,ERR.REDO.H.REPORTS.PARAM)
    IF R.REDO.H.REPORTS.PARAM THEN
        FN.CHK.DIR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.DIR>
    END

    F.CHK.DIR = ''
    CALL OPF(FN.CHK.DIR,F.CHK.DIR)
    Y.FINAL.OUT.FILE.NAME = "AZ.TASAS.PAGO.MIA30.DIAS.txt"
    R.FIL = ''; READ.FIL.ERR = ''
    CALL F.READ(FN.CHK.DIR,Y.FINAL.OUT.FILE.NAME,R.FIL,F.CHK.DIR,READ.FIL.ERR)
    IF R.FIL THEN
        DELETE F.CHK.DIR,Y.FINAL.OUT.FILE.NAME
    END
RETURN

PROCESS:
********
    YFIN.ARRY = "No. Certificado,Fecha Corte,Cant. Dias,Monto Ints. Acumulados,Sucursal"
    SEL.AZ = ''; SEL.REC = ''; SEL.LIST = ''; SEL.ERR = ''
    SEL.AZ = "SELECT ":FN.AZ.ACCOUNT
    CALL EB.READLIST(SEL.AZ,SEL.REC,'',SEL.LIST,SEL.ERR)
    LOOP
        REMOVE ACCR.ID FROM SEL.REC SETTING SEL.POSN
    WHILE ACCR.ID:SEL.POSN

        AZ.ACCT.ERR = ''; R.AZ.ACCOUNT = ''; INT.DATE.LST = ''; YFLD4 = ''; YNO.OF.DAY = ''
        ERR.ACCR.ACCT.CR = ''; R.ACCR.ACCT.CR = ''; YINT.CNT = 0; YCNT = 0
        CALL F.READ(FN.ACCR.ACCT.CR,ACCR.ID,R.ACCR.ACCT.CR,F.ACCR.ACCT.CR,ERR.ACCR.ACCT.CR)

        YNO.OF.DAY = SUM(R.ACCR.ACCT.CR<IC.ACRCR.CR.NO.OF.DAYS>)
        IF (YNO.OF.DAY EQ '' OR YNO.OF.DAY LE 30) THEN
            CONTINUE
        END

        CALL F.READ(FN.AZ.ACCOUNT,ACCR.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,AZ.ACCT.ERR)
        YFLD4 = R.AZ.ACCOUNT<AZ.CO.CODE>
        INT.DATE.LST = R.ACCR.ACCT.CR<IC.ACRCR.CR.INT.DATE>
        YINT.CNT = DCOUNT(INT.DATE.LST,@VM)

        LOOP
        UNTIL YINT.CNT EQ 0
            YCNT += 1
            YFLD1 = ''; YFLD2 = ''; YFLD3 = ''
            YFLD1 = R.ACCR.ACCT.CR<IC.ACRCR.CR.INT.DATE,YCNT>
            YFLD2 = R.ACCR.ACCT.CR<IC.ACRCR.CR.NO.OF.DAYS,YCNT>
            YFLD3 = R.ACCR.ACCT.CR<IC.ACRCR.CR.INT.AMT,YCNT>

            YFIN.ARRY<-1> = ACCR.ID:',':YFLD1:',':YFLD2:',':YFLD3:',':YFLD4
            YINT.CNT -= 1
        REPEAT
    REPEAT

    WRITE YFIN.ARRY ON F.CHK.DIR, Y.FINAL.OUT.FILE.NAME ON ERROR
        Y.ERR.MSG = "Unable to Write ":Y.FINAL.OUT.FILE.NAME
        RETURN
    END
RETURN

END
