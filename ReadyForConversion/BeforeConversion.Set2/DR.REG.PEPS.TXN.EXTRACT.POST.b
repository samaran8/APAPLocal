*-----------------------------------------------------------------------------
* <Rating>20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.REG.PEPS.TXN.EXTRACT.POST
*----------------------------------------------------------------------------------------------------------------------------------
*
* Description  : This routine will get the details from work file and writes into text file.
*
*-------------------------------------------------------------------------
* Date              Author                    Description
* ==========        ====================      ============
* 15-Aug-2014        V.P.Ashokkumar          PACS00396224 - Initial Release.
*-------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.BATCH
    $INSERT T24.BP I_F.DATES
    $INCLUDE LAPAP.BP I_DR.REG.PEPS.TXN.EXTRACT.COMMON
    $INCLUDE TAM.BP I_F.REDO.H.REPORTS.PARAM
*
    GOSUB OPEN.FILES
    GOSUB INIT.PARA
    GOSUB PROCESS.PARA
    RETURN

OPEN.FILES:
***********
    FN.DR.REG.PEPS.WORKFILE = 'F.DR.REG.PEPS.WORKFILE'
    F.DR.REG.PEPS.WORKFILE = ''
    CALL OPF(FN.DR.REG.PEPS.WORKFILE,F.DR.REG.PEPS.WORKFILE)

    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM"
    F.REDO.H.REPORTS.PARAM  = ""
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)
    RETURN

INIT.PARA:
**********
    PARAM.ERR = ''; R.REDO.H.REPORTS.PARAM = ''; Y.REPORT.PARAM.ID = "REDO.PEPS"
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,Y.REPORT.PARAM.ID,R.REDO.H.REPORTS.PARAM,PARAM.ERR)
    IF R.REDO.H.REPORTS.PARAM THEN
        FN.CHK.DIR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.DIR>
        Y.OUT.FILE.NAME = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.FILE.NAME>
    END
    F.CHK.DIR = ''; CHK.FLG = 0
    CALL OPF(FN.CHK.DIR,F.CHK.DIR)
    Y.TODAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
    LAST.WRK.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
    IF (LAST.WRK.DATE[5,2] EQ '06' OR LAST.WRK.DATE[5,2] EQ '12') THEN
        CHK.FLG = 1
        EXTRACT.FILEHF.ID = Y.OUT.FILE.NAME:Y.TODAY:'_':LAST.WRK.DATE[5,2]:'.txt'
    END
    EXTRACT.FILE.ID = Y.OUT.FILE.NAME:Y.TODAY:'.txt'
    RETURN

PROCESS.PARA:
*************
    RETURN.ARR = ''; RETURN.ARR.HF = ''
    SEL.CMD = "SELECT ":FN.DR.REG.PEPS.WORKFILE
    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)
    ID.CTR = 1
    LOOP
    WHILE ID.CTR LE ID.CNT
        REC.ID = ''
        REC.ID = ID.LIST<ID.CTR>
        YTP.RECID = FIELD(REC.ID,'-',2)
        R.REC = ''; RD.ERR = ''
        CALL F.READ(FN.DR.REG.PEPS.WORKFILE, REC.ID, R.REC, F.DR.REG.PEPS.WORKFILE, RD.ERR)
        IF R.REC THEN
            LOCATE YTP.RECID IN YTP.RECID.ARR<1> SETTING V.POSN THEN
            END ELSE
                RETURN.ARR<-1> = R.REC
                YTP.RECID.ARR<-1> = YTP.RECID
                IF CHK.FLG EQ 1 AND REC.ID[1,2] EQ 'HF' THEN
                    RETURN.ARR.HF<-1> = R.REC
                END
            END
        END
        ID.CTR += 1
    REPEAT
    CALL F.WRITE(FN.CHK.DIR,EXTRACT.FILE.ID,RETURN.ARR)
    IF CHK.FLG EQ 1 THEN
        CALL F.WRITE(FN.CHK.DIR,EXTRACT.FILEHF.ID,RETURN.ARR.HF)
    END
    RETURN
END
