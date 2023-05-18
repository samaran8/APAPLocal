* @ValidationCode : MjoxOTU4NTUyOTA3OkNwMTI1MjoxNjg0NDA1MzM1MDI4OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 18 May 2023 15:52:15
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE DR.REG.REGN8.EXTRACT.POST
*----------------------------------------------------------------------------------------------------------------------------------
*
* Description  : This routine will get the details from work file and writes into text file.
*
*-------------------------------------------------------------------------
* Date            Author                Description
* ==========      =================     ============
*07-09-2014        Ashokkumar.V.P       PACS00309080:- modified all the amount retrieving fields
*10-03-2015        Ashokkumar.V.P       PACS00309080:- modified the formatting
* Date                  who                   Reference
* 24-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION -$INSERT T24.BP TO $INSERT AND $INCLUDE REGREP.BP TO $INSERT AND $INCLUDE TAM.BP TO $INSERT
* 24-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BATCH
    $INSERT I_F.DATES
    $INSERT I_F.REDO.H.REPORTS.PARAM
    $INSERT I_F.DR.REG.REGN8.CONCAT
*
    GOSUB OPEN.FILES
    GOSUB PROCESS.PARA
RETURN

OPEN.FILES:
***********
    FN.DR.REG.REGN8.WORKFILE = "F.DR.REG.REGN8.CONCAT"; F.DR.REG.REGN8.WORKFILE = ""
    CALL OPF(FN.DR.REG.REGN8.WORKFILE, F.DR.REG.REGN8.WORKFILE)

    FN.REDO.H.REPORTS.PARAM = 'F.REDO.H.REPORTS.PARAM'; F.REDO.H.REPORTS.PARAM  = ''
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)

    Y.PARAM.ID = 'REDO.REGN8'; R.REDO.H.REPORTS.PARAM = ''; Y.PARAM.ERR = ''
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,Y.PARAM.ID,R.REDO.H.REPORTS.PARAM,Y.PARAM.ERR)
    IF R.REDO.H.REPORTS.PARAM THEN
        Y.FILE.NAME = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.FILE.NAME>
        Y.FIELD.NAME = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.NAME>
        Y.FIELD.VAL  = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE>
        Y.DISP.TXT  = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.DISPLAY.TEXT>
        Y.OUT.FILE.ID  = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.FILE.NAME>
        FN.CHK.DIR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.DIR>
    END

    F.CHK.DIR = ''; LAST.WORK.VAL = ''
    CALL OPF(FN.CHK.DIR,F.CHK.DIR)
    LAST.WORK.VAL = R.DATES(EB.DAT.LAST.WORKING.DAY)
    EXTRACT.FILE.ID = Y.OUT.FILE.ID:'_':LAST.WORK.VAL:'.csv'
    LOCATE 'TAX.ACCT.NO' IN Y.FIELD.NAME<1,1> SETTING Y.TAC.POS THEN
        TAX.ACC = Y.FIELD.VAL<1,Y.TAC.POS>
    END
    LOCATE 'FROM.DATE' IN Y.FIELD.NAME<1,1> SETTING Y.FRM.POS THEN
        STR.DATE = Y.FIELD.VAL<1,Y.FRM.POS>
    END
    LOCATE 'TO.DATE' IN Y.FIELD.NAME<1,1> SETTING Y.TO.POS THEN
        END.DATE = Y.FIELD.VAL<1,Y.TO.POS>
    END
    LOCATE 'LAST.RUN.DATE' IN Y.FIELD.NAME<1,1> SETTING Y.LRD.POS THEN
        LRUN.WDATE = Y.FIELD.VAL<1,Y.LRD.POS>
    END
    IF LRUN.WDATE THEN
        CALL CDT('',LRUN.WDATE,'+1C')
    END
    LAST.WDAY = TODAY
    CALL CDT('',LAST.WDAY,'-1C')
    IF NOT(STR.DATE) AND NOT(END.DATE) THEN
        IF LAST.WDAY LT LRUN.WDATE THEN
            LRUN.WDATE = LAST.WORK.VAL
        END
        STR.DATE = LRUN.WDATE
        END.DATE = LAST.WDAY
    END

    LOCATE 'HEADER.VAL.1' IN Y.FIELD.NAME<1,1> SETTING Y.HAD1.POS THEN
        DISPL.VAL = Y.DISP.TXT<1,Y.HAD1.POS>
    END
RETURN

INIT.FLDS:
**********
*
    FIELD8 = ''; FIELD9 = ''; FIELD10 = ''; FIELD11 = ''
    FIELD12 = ''; FIELD13 = ''; FIELD14 = ''; FIELD15 = ''
    FIELD16 = ''; FIELD17 = ''; FIELD18 = ''; FIELD19 = ''
    FIELD20 = ''; FIELD21 = ''; FIELD22 = ''; FIELD23 = ''
    FIELD24 = ''; FIELD25 = ''; FIELD26 = ''; FIELD27 = ''
    FIELD28 = ''; FIELD29 = ''

    FIELD8.1 = ''; FIELD9.1 = ''; FIELD10.1 = ''; FIELD11.1 = ''
    FIELD12.1 = ''; FIELD13.1 = ''; FIELD14.1 = ''; FIELD15.1 = ''
    FIELD16.1 = ''; FIELD17.1 = ''; FIELD18.1 = ''; FIELD19.1 = ''
    FIELD20.1 = ''; FIELD21.1 = ''; FIELD22.1 = ''; FIELD23.1 = ''
    FIELD24.1 = ''; FIELD25.1 = ''; FIELD26.1 = ''; FIELD27.1 = ''
    FIELD28.1 = ''; FIELD29.1 = ''

    FIELD8.2 = ''; FIELD9.2 = ''; FIELD10.2 = ''; FIELD11.2 = ''
    FIELD12.2 = ''; FIELD13.2 = ''; FIELD14.2 = ''; FIELD15.2 = ''
    FIELD16.2 = ''; FIELD17.2 = ''; FIELD18.2 = ''; FIELD19.2 = ''
    FIELD20.2 = ''; FIELD21.2 = ''; FIELD22.2 = ''; FIELD23.2 = ''
    FIELD24.2 = ''; FIELD25.2 = ''; FIELD26.2 = ''; FIELD27.2 = ''
    FIELD28.2 = ''; FIELD29.2 = ''
RETURN
*------------------------------------------------------------------
APPEND.FIELDS:
**************
*
    FIELD8 = FIELD8.1:'|':FIELD8.2
    FIELD9 = FIELD9.1:'|':FIELD9.2
    FIELD10 = FIELD10.1:'|':FIELD10.2
    FIELD11 = FIELD11.1:'|':FIELD11.2
    FIELD12 = FIELD12.1:'|':FIELD12.2
    FIELD13 = FIELD13.1:'|':FIELD13.2
    FIELD14 = FIELD14.1:'|':FIELD14.2
    FIELD15 = FIELD15.1:'|':FIELD15.2
    FIELD16 = FIELD16.1:'|':FIELD16.2
    FIELD17 = FIELD17.1:'|':FIELD17.2
    FIELD18 = FIELD18.1:'|':FIELD18.2
    FIELD19 = FIELD19.1:'|':FIELD19.2
    FIELD20 = FIELD20.1:'|':FIELD20.2
    FIELD21 = FIELD21.1:'|':FIELD21.2
    FIELD22 = FIELD22.1:'|':FIELD22.2
    FIELD23 = FIELD23.1:'|':FIELD23.2
    FIELD24 = FIELD24.1:'|':FIELD24.2
    FIELD25 = FIELD25.1:'|':FIELD25.2
    FIELD26 = FIELD26.1:'|':FIELD26.2
    FIELD27 = FIELD27.1:'|':FIELD27.2
    FIELD28 = FIELD28.1:'|':FIELD28.2
    FIELD29 = FIELD29.1:'|':FIELD29.2
*
RETURN
*------------------------------------------------------------------
PROCESS.PARA:
*************
*
    GOSUB INIT.FLDS
    GOSUB WRITE.HEADER
*
*    SEL.CMD = "SELECT ":FN.DR.REG.REGN8.WORKFILE
*    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)
*    ID.CTR = 1
*    LOOP
*        REMOVE REC.ID FROM ID.LIST SETTING ID.POS
*    WHILE REC.ID:ID.POS
    REGN.ID = TAX.ACC
    GOSUB READ.WORKFILE
    IF R.REC THEN
        GOSUB CALC.FIELD1
        GOSUB CALC.FIELD2
    END
*        ID.CTR += 1
*    REPEAT
*
    GOSUB APPEND.FIELDS
    GOSUB PROCESS.1

    SUM.CANTIDAD = FIELD(FIELD8,'|',1) + FIELD(FIELD9,'|',1) + FIELD(FIELD10,'|',1) + FIELD(FIELD11,'|',1) + FIELD(FIELD12,'|',1) + FIELD(FIELD13,'|',1) + FIELD(FIELD14,'|',1) + FIELD(FIELD15,'|',1) + FIELD(FIELD16,'|',1) + FIELD(FIELD17,'|',1) + FIELD(FIELD18,'|',1) + FIELD(FIELD19,'|',1) + FIELD(FIELD20,'|',1) + FIELD(FIELD21,'|',1) + FIELD(FIELD22,'|',1)
    SUM.DECLARADO = FIELD(FIELD8,'|',2) + FIELD(FIELD9,'|',2) + FIELD(FIELD10,'|',2) + FIELD(FIELD11,'|',2) + FIELD(FIELD12,'|',2) + FIELD(FIELD13,'|',2) + FIELD(FIELD14,'|',2) + FIELD(FIELD15,'|',2) + FIELD(FIELD16,'|',2) + FIELD(FIELD17,'|',2) + FIELD(FIELD18,'|',2) + FIELD(FIELD19,'|',2) + FIELD(FIELD20,'|',2) + FIELD(FIELD21,'|',2) + FIELD(FIELD22,'|',2)
    REP.CONTENT<-1> = DISPL.VAL<1,1,27>:'|':SUM.CANTIDAD:'|':SUM.DECLARADO
    REP.CONTENT<-1> = DISPL.VAL<1,1,28>
*    TAX.TO.PAY = SUM.DECLARADO * 0.0015
    TAX.TO.PAY = FIELD(R.REC<DR.REGN8.CONCAT.FIELD29>,'|',3)
    TAX.TO.PAY = FMT(TAX.TO.PAY,"R2")
    GOSUB PROCESS.2
    CALL F.WRITE(FN.CHK.DIR,EXTRACT.FILE.ID,REP.CONTENT)

    R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE,Y.FRM.POS> = ''
    R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE,Y.TO.POS> = ''
    R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE,Y.LRD.POS> = END.DATE
    CALL F.WRITE(FN.REDO.H.REPORTS.PARAM,Y.PARAM.ID,R.REDO.H.REPORTS.PARAM)
RETURN

READ.WORKFILE:
**************
    R.REC = ''; RD.ERR = ''
    CALL F.READ(FN.DR.REG.REGN8.WORKFILE, REGN.ID, R.REC, F.DR.REG.REGN8.WORKFILE, RD.ERR)
RETURN

CALC.FIELD2:
************
*
    FIELD19.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD19>,'|',1)
    FIELD19.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD19>,'|',2)
    FIELD20.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD20>,'|',1)
    FIELD20.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD20>,'|',2)
    FIELD21.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD21>,'|',1)
    FIELD21.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD21>,'|',2)
    FIELD22.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD22>,'|',1)
    FIELD22.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD22>,'|',2)
    FIELD23.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD23>,'|',1)
    FIELD23.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD23>,'|',2)
    FIELD24.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD24>,'|',1)
    FIELD24.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD24>,'|',2)
    FIELD25.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD25>,'|',1)
    FIELD25.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD25>,'|',2)
    FIELD26.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD26>,'|',1)
    FIELD26.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD26>,'|',2)
    FIELD27.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD27>,'|',1)
    FIELD27.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD27>,'|',2)
    FIELD28.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD28>,'|',1)
    FIELD28.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD28>,'|',2)
    FIELD29.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD29>,'|',1)
    FIELD29.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD29>,'|',2)
*
RETURN
*----------------------------------------------------------------------
CALC.FIELD1:
************
*
    FIELD8.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD8>,'|',1)
    FIELD8.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD8>,'|',2)
    FIELD9.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD9>,'|',1)
    FIELD9.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD9>,'|',2)
    FIELD10.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD10>,'|',1)
    FIELD10.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD10>,'|',2)
    FIELD11.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD11>,'|',1)
    FIELD11.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD11>,'|',2)
    FIELD12.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD12>,'|',1)
    FIELD12.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD12>,'|',2)
    FIELD13.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD13>,'|',1)
    FIELD13.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD13>,'|',2)
    FIELD14.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD14>,'|',1)
    FIELD14.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD14>,'|',2)
    FIELD15.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD15>,'|',1)
    FIELD15.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD15>,'|',2)
    FIELD16.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD16>,'|',1)
    FIELD16.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD16>,'|',2)
    FIELD17.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD17>,'|',1)
    FIELD17.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD17>,'|',2)
    FIELD18.1 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD18>,'|',1)
    FIELD18.2 += FIELD(R.REC<DR.REGN8.CONCAT.FIELD18>,'|',2)
*
RETURN
*-----------------------------------------------------------------------------------
PROCESS.1:
*********
*
    REP.CONTENT<-1> = DISPL.VAL<1,1,12>:'|':FIELD8
    REP.CONTENT<-1> = DISPL.VAL<1,1,13>:'|':FIELD9
    REP.CONTENT<-1> = DISPL.VAL<1,1,14>:'|':FIELD10
    REP.CONTENT<-1> = DISPL.VAL<1,1,15>:'|':FIELD11
    REP.CONTENT<-1> = DISPL.VAL<1,1,16>:'|':FIELD12
    REP.CONTENT<-1> = DISPL.VAL<1,1,17>:'|':FIELD13
    REP.CONTENT<-1> = DISPL.VAL<1,1,18>:'|':FIELD14
    REP.CONTENT<-1> = DISPL.VAL<1,1,19>:'|':FIELD15
    REP.CONTENT<-1> = DISPL.VAL<1,1,20>:'|':FIELD16
    REP.CONTENT<-1> = DISPL.VAL<1,1,21>:'|':FIELD17
    REP.CONTENT<-1> = DISPL.VAL<1,1,22>:'|':FIELD18
    REP.CONTENT<-1> = DISPL.VAL<1,1,23>:'|':FIELD19
    REP.CONTENT<-1> = DISPL.VAL<1,1,24>:'|':FIELD20
    REP.CONTENT<-1> = DISPL.VAL<1,1,25>:'|':FIELD21
    REP.CONTENT<-1> = DISPL.VAL<1,1,26>:'|':FIELD22
*
RETURN
*-------------------------------------------------------------------
PROCESS.2:
**********
    REP.CONTENT<-1> = DISPL.VAL<1,1,29>:'||':TAX.TO.PAY
    REP.CONTENT<-1> = DISPL.VAL<1,1,30>:'||'
    REP.CONTENT<-1> = DISPL.VAL<1,1,31>:'||':TAX.TO.PAY - 0
    REP.CONTENT<-1> = DISPL.VAL<1,1,32>:'||'
    REP.CONTENT<-1> = DISPL.VAL<1,1,33>
    REP.CONTENT<-1> = DISPL.VAL<1,1,34>
    REP.CONTENT<-1> = DISPL.VAL<1,1,35>
    REP.CONTENT<-1> = DISPL.VAL<1,1,36>
    REP.CONTENT<-1> = DISPL.VAL<1,1,37>:'||':TAX.TO.PAY
    REP.CONTENT<-1> = DISPL.VAL<1,1,38>
    REP.CONTENT<-1> = DISPL.VAL<1,1,39>:'|':FIELD27
    REP.CONTENT<-1> = DISPL.VAL<1,1,40>:'|':FIELD28
    REP.CONTENT<-1> = DISPL.VAL<1,1,41>:'|':FIELD29
*
    CALL F.WRITE(FN.CHK.DIR,EXTRACT.FILE.ID,REP.CONTENT)
RETURN
*-------------------------------------------------------------------
WRITE.HEADER:
*************
*
    REP.CONTENT = ''
    REP.CONTENT<-1> = DISPL.VAL<1,1,1>
    REP.CONTENT<-1> = DISPL.VAL<1,1,2>:' ':STR.DATE:' ':DISPL.VAL<1,1,3>:' ':END.DATE
    REP.CONTENT<-1> = DISPL.VAL<1,1,4>
    REP.CONTENT<-1> = DISPL.VAL<1,1,5>
    REP.CONTENT<-1> = DISPL.VAL<1,1,6>:' ':DISPL.VAL<1,1,7>
    REP.CONTENT<-1> = DISPL.VAL<1,1,8>
    REP.CONTENT<-1> = DISPL.VAL<1,1,9>:'|':DISPL.VAL<1,1,10>:'|':DISPL.VAL<1,1,11>
RETURN
*-------------------------------------------------------------------
END
