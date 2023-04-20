SUBROUTINE REDO.B.AA.LOAN.UNC.BAL.POST
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BATCH
    $INSERT I_F.DATES
    $INSERT I_F.REDO.H.REPORTS.PARAM


    GOSUB OPEN.FILES
    GOSUB READ.PARAM
    GOSUB PROCESS.PARA
RETURN

OPEN.FILES:
***********
    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM"; F.REDO.H.REPORTS.PARAM  = ""
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)
    FN.DR.REG.UNC.BAL.WORKFILE = 'F.DR.REG.UNC.BAL.WORKFILE'; F.DR.REG.UNC.BAL.WORKFILE = ''
    CALL OPF(FN.DR.REG.UNC.BAL.WORKFILE,F.DR.REG.UNC.BAL.WORKFILE)
    R.REDO.H.REPORTS.PARAM = ''; PARAM.ERR = ''; F.CHK.DIR = ''; R.FIL = ''
    SEL.CMD = ''; ID.LIST = ""; ID.CNT = ''; ERR.SEL = ''; READ.FIL.ERR = ''
    LAST.WRK.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
RETURN

READ.PARAM:
***********
    Y.REPORT.PARAM.ID = "REDO.OPER.UNC"
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,Y.REPORT.PARAM.ID,R.REDO.H.REPORTS.PARAM,PARAM.ERR)
    IF R.REDO.H.REPORTS.PARAM THEN
        FN.CHK.DIR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.DIR>
        Y.OUT.FILE.NAME = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.FILE.NAME>
    END
    CALL OPF(FN.CHK.DIR,F.CHK.DIR)
    Y.FINAL.OUT.FILE.NAME = Y.OUT.FILE.NAME:".":LAST.WRK.DATE:".csv"
    CALL F.READ(FN.CHK.DIR,Y.FINAL.OUT.FILE.NAME,R.FIL,F.CHK.DIR,READ.FIL.ERR)
    IF R.FIL THEN
        DELETE F.CHK.DIR,Y.FINAL.OUT.FILE.NAME
    END
RETURN

PROCESS.PARA:
************
    YFLD2.LST = 0; YFLD3.TP = ''; R.REC.TP = ''; YFLD2.LSTTP = 0; YSKP.ID = ''
    R.FILE.DATA = "Suc. Pago,Canal del Pago,No. Prestamo,Codigo Cliente,Fecha del Pago,Refencia Pago,Monto Pago,Monto Saldo Pasivo,Dia Pago Prestamo,Monto Actividad,Monto Actividad id"
    SEL.CMD = "SELECT ":FN.DR.REG.UNC.BAL.WORKFILE:" BY-DSND @ID"
    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)
    ID.CTR = 1
    LOOP
        REMOVE REC.ID FROM ID.LIST SETTING SL.POSN
    WHILE REC.ID:SL.POSN
        R.REC = ''; RD.ERR = ''; YFLD1 = ''; YFLD2 = ''; YFLD3 = ''; FIN.V = 1
        CALL F.READ(FN.DR.REG.UNC.BAL.WORKFILE, REC.ID, R.REC, F.DR.REG.UNC.BAL.WORKFILE, RD.ERR)
        IF R.REC THEN
            YFLD1 = R.REC<2>
            YFLD2 = R.REC<3>
            YFLD3 = FIELD(REC.ID,'-',1)
            IF ID.CTR EQ 1 THEN
                YFLD3.TP = YFLD3
            END
            IF YFLD3 EQ YSKP.ID THEN
                R.REC.TP = ''
                ID.CTR += 1
                CONTINUE
            END
            IF YFLD3.TP EQ YFLD3 THEN
                IF YFLD2.LST LT YFLD1 THEN
                    YFLD2.LST += YFLD2
                    GOSUB ARRAY.VAL
                    YFLD2.LSTTP = YFLD2.LST; FIN.V = 0
                END ELSE
                    IF YFLD1 NE YFLD2.LSTTP AND YFLD2.LST LE YFLD1 THEN
                        YFLD2.LST += YFLD2
                        GOSUB ARRAY.VAL
                        YSKP.ID = YFLD3
                        YFLD2.LSTTP = YFLD1; FIN.V = 0
                    END ELSE
                        R.REC<1> = R.REC.TP
                    END
                END
            END ELSE
                GOSUB ARRAY.VAL
                YFLD2.LST = YFLD2
                YFLD2.LSTTP = YFLD1; FIN.V = 0
* fixed to get the last processing record
                IF ID.CNT EQ ID.CTR AND YFLD3.TP NE YFLD3 THEN
                    R.FILE.DATA<-1> = R.REC<1>
                END
*
            END
            YFLD3.TP = YFLD3
            R.REC.TP = R.REC<1>
        END
        ID.CTR += 1
    REPEAT
    WRITE R.FILE.DATA ON F.CHK.DIR, Y.FINAL.OUT.FILE.NAME ON ERROR
        CALL OCOMO("Unable to write to the file":F.CHK.DIR)
    END
RETURN

ARRAY.VAL:
**********
    IF R.REC.TP AND ID.CTR NE 1 THEN
        R.FILE.DATA<-1> = R.REC.TP
    END
RETURN

END
