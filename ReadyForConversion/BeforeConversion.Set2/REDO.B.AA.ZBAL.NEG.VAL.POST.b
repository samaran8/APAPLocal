*--------------------------------------------------------------------------------------------------------------------------------
* <Rating>-103</Rating>
*--------------------------------------------------------------------------------------------------------------------------------
    SUBROUTINE REDO.B.AA.ZBAL.NEG.VAL.POST
*----------------------------------------------------------------------------------------------------------------------------------
*
* Description  : This routine will get the details from work file and writes into text file.
*
*-------------------------------------------------------------------------
* Date              Author                    Description
* ==========        ====================      ============
* 03-Oct-2014      Ashokkumar                 :- Displaying Credit lines details
*-------------------------------------------------------------------------

    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.BATCH
    $INCLUDE T24.BP I_F.DATES
    $INCLUDE TAM.BP I_F.REDO.H.REPORTS.PARAM
    $INCLUDE LAPAP.BP I_REDO.B.AA.ZBAL.NEG.VAL.COMMON
*
    GOSUB OPEN.FILES
    GOSUB PROCESS.PARA
    GOSUB PROCESS.PARA.2
    GOSUB PROCESS.PARA.3
*    GOSUB PROCESS.PARA.4
    RETURN

OPEN.FILES:
***********
*    LINE.TYP.C = "Cero_Cantidad"; LINE.TYP.P = "Saldo_Positivo"; LINE.TYP.N = "Saldo_Negativo"
*    LINE.TYP.A = "Auth_Estado"; LINE.TYP.IR = "Cantidad_Devengo"; LINE.TYP.DDI = "Instrucciones_debito_directo"

    FN.DR.REG.AA.PROB.WORKFILE = 'F.DR.REG.AA.PROB.WORKFILE'
    F.DR.REG.AA.PROB.WORKFILE = ''
    CALL OPF(FN.DR.REG.AA.PROB.WORKFILE,F.DR.REG.AA.PROB.WORKFILE)

    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM"
    F.REDO.H.REPORTS.PARAM  = ""
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)

    Y.REPORT.PARAM.ID = "REDO.DE08"
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,Y.REPORT.PARAM.ID,R.REDO.H.REPORTS.PARAM,PARAM.ERR)
    IF R.REDO.H.REPORTS.PARAM THEN
        FN.CHK.DIR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.DIR>
    END
    ENQ.DATA = "Loan Number*Account number*AA status*Principal Amt*Interest Amt*Penalty Amt*Charges*Line Type"
    F.CHK.DIR = ''
    CALL OPF(FN.CHK.DIR,F.CHK.DIR)
    TIME.STAMP.VAL = TIMEDATE()
    Y.TIME = FIELD(TIME.STAMP.VAL,' ',1)
    CHANGE ":" TO '' IN Y.TIME
    RETURN

READ.FILE:
**********
    R.FIL = '';    FIL.ERR = ''; R.FILE.DATA = ''
    CALL F.READ(FN.CHK.DIR,EXTRACT.FILE.ID,R.FIL,F.CHK.DIR,READ.FIL.ERR)
    IF R.FIL THEN
        DELETE F.CHK.DIR,EXTRACT.FILE.ID
    END
    RETURN
*-------------------------------------------------------------------
PROCESS.PARA:
***************
    Y.OUT.FILE.NAME = "Prestamo.Problema.Detalles."
    EXTRACT.FILE.ID = Y.OUT.FILE.NAME:TODAY:'.':Y.TIME:'.txt'         ;* Parameterise
    GOSUB READ.FILE
    R.FILE.DATA = "ID Prestamo*ID Cuenta*Estado*Estatus Manual*Estatus Envejecimiento*Capital Pendiente*Interes Corriente*Int Cap Vencido*Mora*Cargos*Cuenta Saldo*Blce Capital Pendiente*Detalles"
    SEL.CMD = ''; ID.LIST = ""; ID.CNT = ''; ERR.SEL = ''
    SEL.CMD = "SELECT ":FN.DR.REG.AA.PROB.WORKFILE:" LIKE ..._1"
    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)
    ID.CTR = 1
    LOOP
    WHILE ID.CTR LE ID.CNT
        R.REC = ''; RD.ERR = ''; REC.ID = ''
        REC.ID = ID.LIST<ID.CTR>
        CALL F.READ(FN.DR.REG.AA.PROB.WORKFILE, REC.ID, R.REC, F.DR.REG.AA.PROB.WORKFILE, RD.ERR)
        IF R.REC THEN
            R.FILE.DATA<-1> = R.REC
        END
        ID.CTR += 1
    REPEAT
    WRITE R.FILE.DATA ON F.CHK.DIR, EXTRACT.FILE.ID ON ERROR
        CALL OCOMO("Unable to write to the file":F.CHK.DIR)
    END
    RETURN

PROCESS.PARA.4:
***************
    Y.OUT.FILE.NAME = "Prestamo.Cantidad.Detalles."
    EXTRACT.FILE.ID = Y.OUT.FILE.NAME:TODAY:'.':Y.TIME:'.txt'         ;* Parameterise
    GOSUB READ.FILE
    R.FILE.DATA = "ID Prestamo*ID Cuenta*Estado*Estatus Manual*Estatus Envejecimiento*Capital Pendiente*Interes Corriente*Int Cap Vencido*Mora*Cargos*Cuenta Saldo*Blce Capital Pendiente*Detalles"
    SEL.CMD = ''; ID.LIST = ""; ID.CNT = ''; ERR.SEL = ''
    SEL.CMD = "SELECT ":FN.DR.REG.AA.PROB.WORKFILE:" LIKE ..._4"
    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)
    ID.CTR = 1
    LOOP
    WHILE ID.CTR LE ID.CNT
        R.REC = ''; RD.ERR = ''; REC.ID = ''
        REC.ID = ID.LIST<ID.CTR>
        CALL F.READ(FN.DR.REG.AA.PROB.WORKFILE, REC.ID, R.REC, F.DR.REG.AA.PROB.WORKFILE, RD.ERR)
        IF R.REC THEN
            R.FILE.DATA<-1> = R.REC
        END
        ID.CTR += 1
    REPEAT
    WRITE R.FILE.DATA ON F.CHK.DIR, EXTRACT.FILE.ID ON ERROR
        CALL OCOMO("Unable to write to the file":F.CHK.DIR)
    END
    RETURN

PROCESS.PARA.2:
***************
    Y.OUT.FILE.NAME = "Prestamo.Accr.Detalles."
    EXTRACT.FILE.ID = Y.OUT.FILE.NAME:TODAY:'.':Y.TIME:'.txt'         ;* Parameterise
    GOSUB READ.FILE
    R.FILE.DATA = "ID Prestamo*ID Cuenta*Estado*Estatus Manual*Ult Fecha Accr*Tipo de Producto*Monto Original Prestamo*Blce. Cap Pendiente"
    SEL.CMD = ''; ID.LIST = ""; ID.CNT = ''; ERR.SEL = ''
    SEL.CMD = "SELECT ":FN.DR.REG.AA.PROB.WORKFILE:" LIKE ..._2"
    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)
    ID.CTR = 1
    LOOP
    WHILE ID.CTR LE ID.CNT
        R.REC = ''; RD.ERR = ''; REC.ID = ''
        REC.ID = ID.LIST<ID.CTR>
        CALL F.READ(FN.DR.REG.AA.PROB.WORKFILE, REC.ID, R.REC, F.DR.REG.AA.PROB.WORKFILE, RD.ERR)
        IF R.REC THEN
            R.FILE.DATA<-1> = R.REC
        END
        ID.CTR += 1
    REPEAT
    WRITE R.FILE.DATA ON F.CHK.DIR, EXTRACT.FILE.ID ON ERROR
        CALL OCOMO("Unable to write to the file":F.CHK.DIR)
    END
    RETURN

PROCESS.PARA.3:
***************
    Y.OUT.FILE.NAME = "Prestamo.Direct.Debit.Detalles."
    EXTRACT.FILE.ID = Y.OUT.FILE.NAME:TODAY:'.':Y.TIME:'.txt'         ;* Parameterise
    GOSUB READ.FILE
    R.FILE.DATA = "ID Prestamo*ID Cuenta*Estado No*Estatus Manual*Condici�n Pr�stamo*Cliente Pr�stamo*Vlor Cuota*Pendiente Pago*Impuesto*Fecha Cuota*No. Cuenta Debito*Cliente Cta Debito*Saldo Disp. Cta Debito"
    SEL.CMD = ''; ID.LIST = ""; ID.CNT = ''; ERR.SEL = ''
    SEL.CMD = "SELECT ":FN.DR.REG.AA.PROB.WORKFILE:" LIKE ..._3"
    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)
    ID.CTR = 1
    LOOP
    WHILE ID.CTR LE ID.CNT
        R.REC = ''; RD.ERR = ''; REC.ID = ''
        REC.ID = ID.LIST<ID.CTR>
        CALL F.READ(FN.DR.REG.AA.PROB.WORKFILE, REC.ID, R.REC, F.DR.REG.AA.PROB.WORKFILE, RD.ERR)
        IF R.REC THEN
            R.FILE.DATA<-1> = R.REC
        END
        ID.CTR += 1
    REPEAT
    WRITE R.FILE.DATA ON F.CHK.DIR, EXTRACT.FILE.ID ON ERROR
        CALL OCOMO("Unable to write to the file":F.CHK.DIR)
    END
    RETURN
*-------------------------------------------------------------------
END