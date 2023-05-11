SUBROUTINE DR.REG.ACTIVAS.CONCAT.POST
*----------------------------------------------------------------------------------------------------------------------------------
*
* Description  : This routine will get the details from work file and writes into text file.
*
*-------------------------------------------------------------------------
* Date         Author                Description
* ==========   =================     ============
* 25-Jul-2014  Ashokkumar.V.P         PACS00305233:- Fixed to display the value with proper format &
*                                    added SubTotal, Total in all the section.
* 09-Oct-2014  Ashokkumar.V.P         PACS00305233:- Changed the report format to new layout
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BATCH
    $INSERT I_F.DATES
    $INSERT I_F.DR.REG.ACTIVAS.PARAM
    $INSERT I_F.DR.REG.ACTIVAS.GROUP

    GOSUB OPEN.FILES
    GOSUB PROCESS.PARA
RETURN

OPEN.FILES:
***********
    FN.DR.REG.ACTIVAS.PARAM = 'F.DR.REG.ACTIVAS.PARAM'; F.DR.REG.ACTIVAS.PARAM = ''
    CALL OPF(FN.DR.REG.ACTIVAS.PARAM,F.DR.REG.ACTIVAS.PARAM)
    FN.DR.REG.ACTIVAS.WORKFILE = "F.DR.REG.ACTIVAS.GROUP" ; F.DR.REG.ACTIVAS.WORKFILE = ""
    CALL OPF(FN.DR.REG.ACTIVAS.WORKFILE, F.DR.REG.ACTIVAS.WORKFILE)
    R.DR.REG.ACTIVAS.PARAM = ''; DR.REG.ACTIVAS.PARAM.ERR = ''; F.CHK.DIR = ''

    CALL CACHE.READ(FN.DR.REG.ACTIVAS.PARAM,'SYSTEM',R.DR.REG.ACTIVAS.PARAM,DR.REG.ACTIVAS.PARAM.ERR)
    FN.CHK.DIR = R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.OUT.PATH>
    CALL OPF(FN.CHK.DIR,F.CHK.DIR)

    EXTRACT.FILE.ID = R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.FILE.NAME,4>:'.txt'
    R.FIL = ''; FIL.ERR = ''
    CALL F.READ(FN.CHK.DIR,EXTRACT.FILE.ID,R.FIL,F.CHK.DIR,READ.FIL.ERR)
    IF R.FIL THEN
        CALL F.DELETE(FN.CHK.DIR,EXTRACT.FILE.ID)
    END
    AAEXTRACT.FILE.ID = R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.FILE.NAME,5>:'.csv'
    R.FIL = ''; FIL.ERR = ''
    CALL F.READ(FN.CHK.DIR,AAEXTRACT.FILE.ID,R.FIL,F.CHK.DIR,READ.FIL.ERR)
    IF R.FIL THEN
        CALL F.DELETE(FN.CHK.DIR,ACEXTRACT.FILE.ID)
    END
RETURN

PROCESS.PARA:
*************
    GOSUB WRITE.HEADER
    SEL.CMD1 = "SELECT ":FN.DR.REG.ACTIVAS.WORKFILE:" WITH @ID LIKE GROUP1-..."
    CALL EB.READLIST(SEL.CMD1, ID.LIST1, "", ID.CNT1, ERR.SEL1)
    IF ID.LIST1 THEN
        GOSUB WRITE.BODY.PARA1
    END   ;* Byron - PACS00305233 S/E

    SEL.CMD2 = "SELECT ":FN.DR.REG.ACTIVAS.WORKFILE:" WITH @ID LIKE GROUP2-..."
    CALL EB.READLIST(SEL.CMD2, ID.LIST2, "", ID.CNT2, ERR.SEL2)
    IF ID.LIST2 THEN
        GOSUB WRITE.BODY.PARA2
    END   ;* Byron - PACS00305233 S/E

    SEL.CMD3 = "SELECT ":FN.DR.REG.ACTIVAS.WORKFILE:" WITH @ID LIKE GROUP3-..."
    CALL EB.READLIST(SEL.CMD3, ID.LIST3, "", ID.CNT3, ERR.SEL3)
    IF ID.LIST3 THEN
        GOSUB WRITE.BODY.PARA3
    END   ;* Byron - PACS00305233 S/E

    SEL.CMD4 = "SELECT ":FN.DR.REG.ACTIVAS.WORKFILE:" WITH @ID LIKE GROUP4-..."
    CALL EB.READLIST(SEL.CMD4, ID.LIST4, "", ID.CNT4, ERR.SEL4)
    IF ID.LIST4 THEN
        GOSUB WRITE.BODY.PARA4
    END   ;* Byron - PACS00305233 S/E

    SEL.CMD5 = "SELECT ":FN.DR.REG.ACTIVAS.WORKFILE:" WITH @ID LIKE GROUP5-..."
    CALL EB.READLIST(SEL.CMD5, ID.LIST5, "", ID.CNT5, ERR.SEL5)
    IF ID.LIST5 THEN
        GOSUB WRITE.BODY.PARA5
    END   ;* Byron - PACS00305233 S/E

    SEL.CMD6 = "SELECT ":FN.DR.REG.ACTIVAS.WORKFILE:" WITH @ID LIKE GROUP6-..."
    CALL EB.READLIST(SEL.CMD6, ID.LIST6, "", ID.CNT6, ERR.SEL6)
    IF ID.LIST6 THEN
        GOSUB WRITE.BODY.PARA6
    END   ;* Byron - PACS00305233 S/E

    SEL.CMD7 = "SELECT ":FN.DR.REG.ACTIVAS.WORKFILE:" WITH @ID LIKE GROUP7-..."
    CALL EB.READLIST(SEL.CMD7, ID.LIST7, "", ID.CNT7, ERR.SEL7)
    IF ID.LIST7 THEN
        GOSUB WRITE.BODY.PARA7
    END   ;* Byron - PACS00305233 S/E

    CALL F.WRITE(FN.CHK.DIR,EXTRACT.FILE.ID,RETURN.ARR)
    GOSUB ACTIVAS.DETAIL
RETURN

WRITE.BODY.PARA1:
****************
*    REP.LINE0 = FMT("Prestamos en RD$ comerciales",'65L')
    REP.LINE0 = FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.NAME,1>,'65L')
    RETURN.ARR<-1> = REP.LINE0
    CNT.LOAN = ''; CNT.LOAN.PR1 = ''; CNT.LOAN.PR2 = ''; CNT.LOAN.PR3 = ''; CNT.LOAN.PR4 = ''; CNT.LOAN.PR5 = ''; CNT.LOAN.PR6 = ''
    TOT.AMT = ''; TOT.AMT.PR1 = ''; TOT.AMT.PR2 = ''; TOT.AMT.PR3 = ''; TOT.AMT.PR4 = ''; TOT.AMT.PR5 = ''; TOT.AMT.PR6 = ''
    RETURN.ARR.PR1 = ''; RETURN.ARR.PR2 = ''; RETURN.ARR.PR3 = ''; RETURN.ARR.PR4 = ''; RETURN.ARR.PR5 = ''; RETURN.ARR.PR6 = ''
    LOOP
        REMOVE REC.ID1 FROM ID.LIST1 SETTING ID.POS1
    WHILE REC.ID1:ID.POS1
        CALL F.READ(FN.DR.REG.ACTIVAS.WORKFILE, REC.ID1, R.REC1, F.DR.REG.ACTIVAS.WORKFILE, RD.ERR)
        GOSUB BODY.PARA.CASE
    REPEAT
    GOSUB RETURN.ARRY.WRITE
RETURN

BODY.PARA.CASE:
***************
    IF NOT(R.REC1) THEN
        RETURN
    END
    REP.LINE1.1 = ''; REP.LINE1.2 = ''; REP.LINE1.3 = ''; REP.LINE1.4 = ''; REP.LINE1.5 = ''; REP.LINE1.6 = ''
    IF R.REC1<DR.ACT.GRP.SUB1.LOANS> AND R.REC1<DR.ACT.GRP.SUB1.AMT> THEN
        CNT.LOAN.PR1 += R.REC1<DR.ACT.GRP.SUB1.LOANS>
        TOT.AMT.PR1 += R.REC1<DR.ACT.GRP.SUB1.AMT>
        REP.LINE1.1 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,1>,'26L'):FMT(R.REC1<DR.ACT.GRP.SUB1.LOANS>,'10R'):FMT(FIELD(REC.ID1,'-',2),'R6#14'):FMT(R.REC1<DR.ACT.GRP.SUB1.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE1.L1
    END
    IF R.REC1<DR.ACT.GRP.SUB2.LOANS> AND R.REC1<DR.ACT.GRP.SUB2.AMT> THEN
        CNT.LOAN.PR2 += R.REC1<DR.ACT.GRP.SUB2.LOANS>
        TOT.AMT.PR2 += R.REC1<DR.ACT.GRP.SUB2.AMT>
        REP.LINE1.2 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,2>,'26L'):FMT(R.REC1<DR.ACT.GRP.SUB2.LOANS>,'10R'):FMT(FIELD(REC.ID1,'-',2),'R6#14'):FMT(R.REC1<DR.ACT.GRP.SUB2.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE2.L1
    END
    IF R.REC1<DR.ACT.GRP.SUB3.LOANS> AND R.REC1<DR.ACT.GRP.SUB3.AMT> THEN
        CNT.LOAN.PR3 += R.REC1<DR.ACT.GRP.SUB3.LOANS>
        TOT.AMT.PR3 += R.REC1<DR.ACT.GRP.SUB3.AMT>
        REP.LINE1.3 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,3>,'26L'):FMT(R.REC1<DR.ACT.GRP.SUB3.LOANS>,'10R'):FMT(FIELD(REC.ID1,'-',2),'R6#14'):FMT(R.REC1<DR.ACT.GRP.SUB3.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE3.L1
    END
    IF R.REC1<DR.ACT.GRP.SUB4.LOANS> AND R.REC1<DR.ACT.GRP.SUB4.AMT> THEN
        CNT.LOAN.PR4 += R.REC1<DR.ACT.GRP.SUB4.LOANS>
        TOT.AMT.PR4 += R.REC1<DR.ACT.GRP.SUB4.AMT>
        REP.LINE1.4 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,4>,'26L'):FMT(R.REC1<DR.ACT.GRP.SUB4.LOANS>,'10R'):FMT(FIELD(REC.ID1,'-',2),'R6#14'):FMT(R.REC1<DR.ACT.GRP.SUB4.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE4.L1
    END
    IF R.REC1<DR.ACT.GRP.SUB5.LOANS> AND R.REC1<DR.ACT.GRP.SUB5.AMT> THEN
        CNT.LOAN.PR5 += R.REC1<DR.ACT.GRP.SUB5.LOANS>
        TOT.AMT.PR5 += R.REC1<DR.ACT.GRP.SUB5.AMT>
        REP.LINE1.5 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,5>,'26L'):FMT(R.REC1<DR.ACT.GRP.SUB5.LOANS>,'10R'):FMT(FIELD(REC.ID1,'-',2),'R6#14'):FMT(R.REC1<DR.ACT.GRP.SUB5.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE5.L1
    END
    IF R.REC1<DR.ACT.GRP.SUB6.LOANS> AND R.REC1<DR.ACT.GRP.SUB6.AMT> THEN
        CNT.LOAN.PR6 += R.REC1<DR.ACT.GRP.SUB6.LOANS>
        TOT.AMT.PR6 += R.REC1<DR.ACT.GRP.SUB6.AMT>
        REP.LINE1.6 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,6>,'26L'):FMT(R.REC1<DR.ACT.GRP.SUB6.LOANS>,'10R'):FMT(FIELD(REC.ID1,'-',2),'R6#14'):FMT(R.REC1<DR.ACT.GRP.SUB6.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE6.L1
    END
RETURN

RETURN.ARRY.WRITE:
******************
    IF RETURN.ARR.PR1 THEN
        RETURN.ARR<-1> = RETURN.ARR.PR1:@FM:FMT("SubTotal:",'65L'):FMT("","26L"):FMT(CNT.LOAN.PR1,'10R'):FMT('','14R'):FMT(TOT.AMT.PR1,'R2,#18')
    END
    IF RETURN.ARR.PR2 THEN
        RETURN.ARR<-1> = RETURN.ARR.PR2:@FM:FMT("SubTotal:",'65L'):FMT("","26L"):FMT(CNT.LOAN.PR2,'10R'):FMT('','14R'):FMT(TOT.AMT.PR2,'R2,#18')
    END
    IF RETURN.ARR.PR3 THEN
        RETURN.ARR<-1> = RETURN.ARR.PR3:@FM:FMT("SubTotal:",'65L'):FMT("","26L"):FMT(CNT.LOAN.PR3,'10R'):FMT('','14R'):FMT(TOT.AMT.PR3,'R2,#18')
    END
    IF RETURN.ARR.PR4 THEN
        RETURN.ARR<-1> = RETURN.ARR.PR4:@FM:FMT("SubTotal:",'65L'):FMT("","26L"):FMT(CNT.LOAN.PR4,'10R'):FMT('','14R'):FMT(TOT.AMT.PR4,'R2,#18')
    END
    IF RETURN.ARR.PR5 THEN
        RETURN.ARR<-1> = RETURN.ARR.PR5:@FM:FMT("SubTotal:",'65L'):FMT("","26L"):FMT(CNT.LOAN.PR5,'10R'):FMT('','14R'):FMT(TOT.AMT.PR5,'R2,#18')
    END
    IF RETURN.ARR.PR6 THEN
        RETURN.ARR<-1> = RETURN.ARR.PR6:@FM:FMT("SubTotal:",'65L'):FMT("","26L"):FMT(CNT.LOAN.PR6,'10R'):FMT('','14R'):FMT(TOT.AMT.PR6,'R2,#18')
    END
    CNT.LOAN = CNT.LOAN.PR1 + CNT.LOAN.PR2 + CNT.LOAN.PR3 + CNT.LOAN.PR4 + CNT.LOAN.PR5 + CNT.LOAN.PR6
    TOT.AMT = TOT.AMT.PR1 + TOT.AMT.PR2 + TOT.AMT.PR3 + TOT.AMT.PR4 + TOT.AMT.PR5 + TOT.AMT.PR6
    REP.LINE1.TOT = FMT("Total:",'65L'):FMT("","26L"):FMT(CNT.LOAN,'10R'):FMT('','14R'):FMT(TOT.AMT,'R2,#18') ;* Byron - PACS00305233 S/E
    IF CNT.LOAN OR TOT.AMT THEN
        RETURN.ARR<-1> = REP.LINE1.TOT
    END
    GOSUB INIT.VAR
RETURN

INIT.VAR:
*********
    RETURN.ARR.PR1 = ''; RETURN.ARR.PR2 = ''; RETURN.ARR.PR3 = ''; RETURN.ARR.PR4 = ''; TOT.AMT.PR5 = ''
    RETURN.ARR.PR5 = ''; RETURN.ARR.PR6 = ''; REP.LINE1.TOT = ''; CNT.LOAN = ''; TOT.AMT = '';  TOT.AMT.PR6 = ''
    REP.LINE1.TOT = ''; REP.LINE1.1 = ''; TOT.AMT.PR1 = ''; TOT.AMT.PR2 = ''; TOT.AMT.PR3 = ''; TOT.AMT.PR4 = ''
    CNT.LOAN.PR1 = ''; CNT.LOAN.PR2 = ''; CNT.LOAN.PR3 = ''; CNT.LOAN.PR4 = ''; CNT.LOAN.PR5 = ''; CNT.LOAN.PR6 = ''
    REP.LINE1.1 = ''; REP.LINE1.2 = ''; REP.LINE1.3 = ''; REP.LINE1.4 = ''; REP.LINE1.5 = ''; REP.LINE1.6 = ''
RETURN

WRITE.BODY.PARA2:
****************
*    REP.LINE0 = FMT("Prestamos en RD$ al consumo y/o personales",'65L')         ;* Byron - PACS00305233 S/E
    REP.LINE0 = FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.NAME,2>,'65L')
    RETURN.ARR<-1> = REP.LINE0
    GOSUB INIT.VAR

    LOOP
        REMOVE REC.ID2 FROM ID.LIST2 SETTING ID.POS2
    WHILE REC.ID2:ID.POS2
        CALL F.READ(FN.DR.REG.ACTIVAS.WORKFILE, REC.ID2, R.REC2, F.DR.REG.ACTIVAS.WORKFILE, RD.ERR)
        GOSUB BODY.PARA2.CASE
    REPEAT
    GOSUB RETURN.ARRY.WRITE
RETURN

BODY.PARA2.CASE:
****************
    IF NOT(R.REC2) THEN
        RETURN
    END
    REP.LINE1.1 = ''; REP.LINE1.2 = '' ; REP.LINE1.3 = '' ; REP.LINE1.4 = ''; REP.LINE1.5 = ''; REP.LINE1.6 = ''
    IF R.REC2<DR.ACT.GRP.SUB1.LOANS> AND R.REC2<DR.ACT.GRP.SUB1.AMT> THEN
        CNT.LOAN.PR1 += R.REC2<DR.ACT.GRP.SUB1.LOANS>
        TOT.AMT.PR1 += R.REC2<DR.ACT.GRP.SUB1.AMT>
        REP.LINE1.1 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,1>,'26L'):FMT(R.REC2<DR.ACT.GRP.SUB1.LOANS>,'10R'):FMT(FIELD(REC.ID2,'-',2),'R6#14'):FMT(R.REC2<DR.ACT.GRP.SUB1.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE1.L1
    END
    IF R.REC2<DR.ACT.GRP.SUB2.LOANS> AND R.REC2<DR.ACT.GRP.SUB2.AMT> THEN
        CNT.LOAN.PR2 += R.REC2<DR.ACT.GRP.SUB2.LOANS>
        TOT.AMT.PR2 += R.REC2<DR.ACT.GRP.SUB2.AMT>
        REP.LINE1.2 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,2>,'26L'):FMT(R.REC2<DR.ACT.GRP.SUB2.LOANS>,'10R'):FMT(FIELD(REC.ID2,'-',2),'R6#14'):FMT(R.REC2<DR.ACT.GRP.SUB2.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE2.L1
    END
    IF R.REC2<DR.ACT.GRP.SUB3.LOANS> AND R.REC2<DR.ACT.GRP.SUB3.AMT> THEN
        CNT.LOAN.PR3 += R.REC2<DR.ACT.GRP.SUB3.LOANS>
        TOT.AMT.PR3 += R.REC2<DR.ACT.GRP.SUB3.AMT>
        REP.LINE1.3 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,3>,'26L'):FMT(R.REC2<DR.ACT.GRP.SUB3.LOANS>,'10R'):FMT(FIELD(REC.ID2,'-',2),'R6#14'):FMT(R.REC2<DR.ACT.GRP.SUB3.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE3.L1
    END
    IF R.REC2<DR.ACT.GRP.SUB4.LOANS> AND R.REC2<DR.ACT.GRP.SUB4.AMT> THEN
        CNT.LOAN.PR4 += R.REC2<DR.ACT.GRP.SUB4.LOANS>
        TOT.AMT.PR4 += R.REC2<DR.ACT.GRP.SUB4.AMT>
        REP.LINE1.4 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,4>,'26L'):FMT(R.REC2<DR.ACT.GRP.SUB4.LOANS>,'10R'):FMT(FIELD(REC.ID2,'-',2),'R6#14'):FMT(R.REC2<DR.ACT.GRP.SUB4.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE4.L1
    END
    IF R.REC2<DR.ACT.GRP.SUB5.LOANS> AND R.REC2<DR.ACT.GRP.SUB5.AMT> THEN
        CNT.LOAN.PR5 += R.REC2<DR.ACT.GRP.SUB5.LOANS>
        TOT.AMT.PR5 += R.REC2<DR.ACT.GRP.SUB5.AMT>
        REP.LINE1.5 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,5>,'26L'):FMT(R.REC2<DR.ACT.GRP.SUB5.LOANS>,'10R'):FMT(FIELD(REC.ID2,'-',2),'R6#14'):FMT(R.REC2<DR.ACT.GRP.SUB5.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE5.L1
    END
    IF R.REC2<DR.ACT.GRP.SUB6.LOANS> AND R.REC2<DR.ACT.GRP.SUB6.AMT> THEN
        CNT.LOAN.PR6 += R.REC2<DR.ACT.GRP.SUB6.LOANS>
        TOT.AMT.PR6 += R.REC2<DR.ACT.GRP.SUB6.AMT>
        REP.LINE1.6 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,6>,'26L'):FMT(R.REC2<DR.ACT.GRP.SUB6.LOANS>,'10R'):FMT(FIELD(REC.ID2,'-',2),'R6#14'):FMT(R.REC2<DR.ACT.GRP.SUB6.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE6.L1
    END
RETURN

WRITE.BODY.PARA3:
****************
*    REP.LINE0 = FMT("Prestamos en RD$ hipotecarios y/o desarrollo",'65L')       ;* Byron - PACS00305233 S/E
    REP.LINE0 = FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.NAME,3>,'65L')
    RETURN.ARR<-1> = REP.LINE0
    GOSUB INIT.VAR
    LOOP
        REMOVE REC.ID3 FROM ID.LIST3 SETTING ID.POS3
    WHILE REC.ID3:ID.POS3
        CALL F.READ(FN.DR.REG.ACTIVAS.WORKFILE, REC.ID3, R.REC3, F.DR.REG.ACTIVAS.WORKFILE, RD.ERR)
        GOSUB BODY.PARA3.CASE
    REPEAT
    GOSUB RETURN.ARRY.WRITE
RETURN

BODY.PARA3.CASE:
****************
    IF NOT(R.REC3) THEN
        RETURN
    END
    REP.LINE1.1 = ''; REP.LINE1.2 = '' ; REP.LINE1.3 = '' ; REP.LINE1.4 = ''; REP.LINE1.5 = ''; REP.LINE1.6 = ''
    IF R.REC3<DR.ACT.GRP.SUB1.LOANS> AND R.REC3<DR.ACT.GRP.SUB1.AMT> THEN
        CNT.LOAN.PR1 += R.REC3<DR.ACT.GRP.SUB1.LOANS>
        TOT.AMT.PR1 += R.REC3<DR.ACT.GRP.SUB1.AMT>
        REP.LINE1.1 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,1>,'26L'):FMT(R.REC3<DR.ACT.GRP.SUB1.LOANS>,'10R'):FMT(FIELD(REC.ID3,'-',2),'R6#14'):FMT(R.REC3<DR.ACT.GRP.SUB1.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE1.L1
    END
    IF R.REC3<DR.ACT.GRP.SUB2.LOANS> AND R.REC3<DR.ACT.GRP.SUB2.AMT> THEN
        CNT.LOAN.PR2 += R.REC3<DR.ACT.GRP.SUB2.LOANS>
        TOT.AMT.PR2 += R.REC3<DR.ACT.GRP.SUB2.AMT>
        REP.LINE1.2 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,2>,'26L'):FMT(R.REC3<DR.ACT.GRP.SUB2.LOANS>,'10R'):FMT(FIELD(REC.ID3,'-',2),'R6#14'):FMT(R.REC3<DR.ACT.GRP.SUB2.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE2.L1
    END
    IF R.REC3<DR.ACT.GRP.SUB3.LOANS> AND R.REC3<DR.ACT.GRP.SUB3.AMT> THEN
        CNT.LOAN.PR3 += R.REC3<DR.ACT.GRP.SUB3.LOANS>
        TOT.AMT.PR3 += R.REC3<DR.ACT.GRP.SUB3.AMT>
        REP.LINE1.3 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,3>,'26L'):FMT(R.REC3<DR.ACT.GRP.SUB3.LOANS>,'10R'):FMT(FIELD(REC.ID3,'-',2),'R6#14'):FMT(R.REC3<DR.ACT.GRP.SUB3.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE3.L1
    END
    IF R.REC3<DR.ACT.GRP.SUB4.LOANS> AND R.REC3<DR.ACT.GRP.SUB4.AMT> THEN
        CNT.LOAN.PR4 += R.REC3<DR.ACT.GRP.SUB4.LOANS>
        TOT.AMT.PR4 += R.REC3<DR.ACT.GRP.SUB4.AMT>
        REP.LINE1.4 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,4>,'26L'):FMT(R.REC3<DR.ACT.GRP.SUB4.LOANS>,'10R'):FMT(FIELD(REC.ID3,'-',2),'R6#14'):FMT(R.REC3<DR.ACT.GRP.SUB4.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE4.L1
    END
    IF R.REC3<DR.ACT.GRP.SUB5.LOANS> AND R.REC3<DR.ACT.GRP.SUB5.AMT> THEN
        CNT.LOAN.PR5 += R.REC3<DR.ACT.GRP.SUB5.LOANS>
        TOT.AMT.PR5 += R.REC3<DR.ACT.GRP.SUB5.AMT>
        REP.LINE1.5 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,5>,'26L'):FMT(R.REC3<DR.ACT.GRP.SUB5.LOANS>,'10R'):FMT(FIELD(REC.ID3,'-',2),'R6#14'):FMT(R.REC3<DR.ACT.GRP.SUB5.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE5.L1
    END
    IF R.REC3<DR.ACT.GRP.SUB6.LOANS> AND R.REC3<DR.ACT.GRP.SUB6.AMT> THEN
        CNT.LOAN.PR6 += R.REC3<DR.ACT.GRP.SUB6.LOANS>
        TOT.AMT.PR6 += R.REC3<DR.ACT.GRP.SUB6.AMT>
        REP.LINE1.6 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.RANGE,6>,'26L'):FMT(R.REC3<DR.ACT.GRP.SUB6.LOANS>,'10R'):FMT(FIELD(REC.ID3,'-',2),'R6#14'):FMT(R.REC3<DR.ACT.GRP.SUB6.AMT>,'R2,#18')          ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE6.L1
    END
RETURN

WRITE.BODY.PARA4:
****************
*    REP.LINE0 = FMT("Prestamos en RD$ corporativos y/o preferenciales",'65L')   ;* Byron - PACS00305233 S/E
    REP.LINE0 = FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.NAME,4>,'65L')
    RETURN.ARR<-1> = REP.LINE0
    GOSUB INIT.VAR
    LOOP
        REMOVE REC.ID4 FROM ID.LIST4 SETTING ID.POS4
    WHILE REC.ID4:ID.POS4
        CALL F.READ(FN.DR.REG.ACTIVAS.WORKFILE, REC.ID4, R.REC1, F.DR.REG.ACTIVAS.WORKFILE, RD.ERR)
        REC.ID1 = REC.ID4
        GOSUB BODY.PARA.CASE
    REPEAT
    GOSUB RETURN.ARRY.WRITE
RETURN

WRITE.BODY.PARA5:
****************
*    REP.LINE0 = FMT("Prestamos en US$ al consumo",'65L')    ;* Byron - PACS00305233 S/E
    REP.LINE0 = FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.NAME,5>,'65L')
    RETURN.ARR<-1> = REP.LINE0
    GOSUB INIT.VAR
    LOOP
        REMOVE REC.ID5 FROM ID.LIST5 SETTING ID.POS5
    WHILE REC.ID5:ID.POS5
        CALL F.READ(FN.DR.REG.ACTIVAS.WORKFILE, REC.ID5, R.REC1, F.DR.REG.ACTIVAS.WORKFILE, RD.ERR)
        REC.ID1 = REC.ID5
        GOSUB BODY.PARA.CASE
    REPEAT
    GOSUB RETURN.ARRY.WRITE
RETURN

*-------------------------------------------------------------------
WRITE.BODY.PARA6:
****************
*    REP.LINE0 = FMT("Prestamos en US$ al consumo, desarrollo y comerciales",'65L')        ;* Byron - PACS00305233 S/E
    REP.LINE0 = FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.NAME,6>,'65L')
    RETURN.ARR<-1> = REP.LINE0
    GOSUB INIT.VAR
    LOOP
        REMOVE REC.ID6 FROM ID.LIST6 SETTING ID.POS6
    WHILE REC.ID6:ID.POS6
        CALL F.READ(FN.DR.REG.ACTIVAS.WORKFILE, REC.ID6, R.REC1, F.DR.REG.ACTIVAS.WORKFILE, RD.ERR)
        REC.ID1 = REC.ID6
        GOSUB BODY.PARA.CASE
    REPEAT
    GOSUB RETURN.ARRY.WRITE
RETURN

WRITE.BODY.PARA7:
****************
*    REP.LINE0 = FMT("Prestamos interbancarios",'65L')       ;* Byron - PACS00305233 S/E
    REP.LINE0 = FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.REP.NAME,7>,'65L')
    RETURN.ARR<-1> = REP.LINE0
    GOSUB INIT.VAR
    LOOP
        REMOVE REC.ID7 FROM ID.LIST7 SETTING ID.POS7
    WHILE REC.ID7:ID.POS7
        CALL F.READ(FN.DR.REG.ACTIVAS.WORKFILE, REC.ID7, R.REC1, F.DR.REG.ACTIVAS.WORKFILE, RD.ERR)
        IF R.REC1 THEN
            REP.LINE1.1 = ''; REP.LINE1.2 = '' ; REP.LINE1.3 = '' ; REP.LINE1.4 = ''
            GOSUB BODY.PARA7.CASE
        END
    REPEAT
    GOSUB RETURN.ARRY.WRITE
RETURN

BODY.PARA7.CASE:
****************
    IF R.REC1<DR.ACT.GRP.SUB1.LOANS> AND R.REC1<DR.ACT.GRP.SUB1.AMT> THEN
        CNT.LOAN.PR1 += R.REC1<DR.ACT.GRP.SUB1.LOANS>
        TOT.AMT.PR1 += R.REC1<DR.ACT.GRP.SUB1.AMT>
        REP.LINE1.1 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.MM.RANGE,1>,'26L'):FMT(R.REC1<DR.ACT.GRP.SUB1.LOANS>,'10R'):FMT(FIELD(REC.ID7,'-',2),'R6#14'):FMT(R.REC1<DR.ACT.GRP.SUB1.AMT>,'R2,#18') ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE1.L1
    END
    IF R.REC1<DR.ACT.GRP.SUB2.LOANS> AND R.REC1<DR.ACT.GRP.SUB2.AMT> THEN
        CNT.LOAN.PR2 += R.REC1<DR.ACT.GRP.SUB2.LOANS>
        TOT.AMT.PR2 += R.REC1<DR.ACT.GRP.SUB2.AMT>
        REP.LINE1.2 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.MM.RANGE,2>,'26L'):FMT(R.REC1<DR.ACT.GRP.SUB2.LOANS>,'10R'):FMT(FIELD(REC.ID7,'-',2),'R6#14'):FMT(R.REC1<DR.ACT.GRP.SUB2.AMT>,'R2,#18') ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE2.L1
    END
    IF R.REC1<DR.ACT.GRP.SUB3.LOANS> AND R.REC1<DR.ACT.GRP.SUB3.AMT> THEN
        CNT.LOAN.PR3 += R.REC1<DR.ACT.GRP.SUB3.LOANS>
        TOT.AMT.PR3 += R.REC1<DR.ACT.GRP.SUB3.AMT>
        REP.LINE1.3 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.MM.RANGE,3>,'26L'):FMT(R.REC1<DR.ACT.GRP.SUB3.LOANS>,'10R'):FMT(FIELD(REC.ID7,'-',2),'R6#14'):FMT(R.REC1<DR.ACT.GRP.SUB3.AMT>,'R2,#18') ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE3.L1
    END
    IF R.REC1<DR.ACT.GRP.SUB4.LOANS> AND R.REC1<DR.ACT.GRP.SUB4.AMT> THEN
        CNT.LOAN.PR4 += R.REC1<DR.ACT.GRP.SUB4.LOANS>
        TOT.AMT.PR4 += R.REC1<DR.ACT.GRP.SUB4.AMT>
        REP.LINE1.4 = FMT("",'65L'):FMT(R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.GRP.MM.RANGE,4>,'26L'):FMT(R.REC1<DR.ACT.GRP.SUB4.LOANS>,'10R'):FMT(FIELD(REC.ID7,'-',2),'R6#14'):FMT(R.REC1<DR.ACT.GRP.SUB4.AMT>,'R2,#18') ;* Byron - PACS00305233 S/E
        GOSUB ARRY.RANGE4.L1
    END
RETURN

ARRY.RANGE1.L1:
***************
    IF REP.LINE1.1 THEN
        RETURN.ARR.PR1<-1> = REP.LINE1.1
    END
RETURN

ARRY.RANGE2.L1:
***************
    IF REP.LINE1.2 THEN
        RETURN.ARR.PR2<-1> = REP.LINE1.2
    END
RETURN
ARRY.RANGE3.L1:
***************
    IF REP.LINE1.3 THEN
        RETURN.ARR.PR3<-1> = REP.LINE1.3
    END
RETURN
ARRY.RANGE4.L1:
***************
    IF REP.LINE1.4 THEN
        RETURN.ARR.PR4<-1> = REP.LINE1.4
    END
RETURN
ARRY.RANGE5.L1:
***************
    IF REP.LINE1.5 THEN
        RETURN.ARR.PR5<-1> = REP.LINE1.5
    END
RETURN
ARRY.RANGE6.L1:
***************
    IF REP.LINE1.6 THEN
        RETURN.ARR.PR6<-1> = REP.LINE1.6
    END
RETURN

WRITE.HEADER:
*-----------*
    RETURN.ARR = ''
    RETURN.ARR<-1> = R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.FILE.NAME,1>
    LAST.WORK.VAL = R.DATES(EB.DAT.LAST.WORKING.DAY)
    LAST.DATE = LAST.WORK.VAL[7,2]:"/":LAST.WORK.VAL[5,2]:"/":LAST.WORK.VAL[1,4]
    RETURN.ARR<-1> = R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.FILE.NAME,2>:": ":LAST.DATE
    RETURN.ARR<-1> = R.DR.REG.ACTIVAS.PARAM<DR.ACTIVAS.PARAM.FILE.NAME,3>
    RETURN.ARR<-1> = FMT('Instrumento','65L'):FMT('Plazo','26L'):FMT('Cantidad','10R'):FMT('Tasa(%Anual)','14R'):FMT('Monto','18R')
RETURN

ACTIVAS.DETAIL:
***************
    SEL.CMD = ''; SEL.LIST = ''; SEL.CMD2 = ''; FINAL.ARRAY = ''
    SEL.CMD = "SELECT ":FN.DR.REG.ACTIVAS.WORKFILE:" WITH @ID LIKE AA..."
    CALL EB.READLIST(SEL.CMD,SEL.REC,'',SEL.LIST,SEL.ERR)
    LOOP
        REMOVE SEL.ID FROM SEL.REC SETTING SEL.POSN
    WHILE SEL.ID:SEL.POSN
        ERR.DR.REG.ACTIVAS.WORKFILE = ''; R.DR.REG.ACTIVAS.WORKFILE = ''
        CALL F.READ(FN.DR.REG.ACTIVAS.WORKFILE,SEL.ID,R.DR.REG.ACTIVAS.WORKFILE,F.DR.REG.ACTIVAS.WORKFILE,ERR.DR.REG.ACTIVAS.WORKFILE)
        IF R.DR.REG.ACTIVAS.WORKFILE THEN
            FINAL.ARRAY<-1> = R.DR.REG.ACTIVAS.WORKFILE<DR.ACT.GRP.SUB1.LOANS>
        END
    REPEAT

    CRLF = CHARX(013):CHARX(010)
    CHANGE @FM TO CRLF IN FINAL.ARRAY
    WRITE FINAL.ARRAY ON F.CHK.DIR, AAEXTRACT.FILE.ID ON ERROR
        Y.ERR.MSG = "Unable to Write '":F.CHK.DIR:"'"
    END
RETURN
END
