*--------------------------------------------------------------------------------------------------------------------------------
* <Rating>-41</Rating>
*--------------------------------------------------------------------------------------------------------------------------------
    SUBROUTINE DR.REG.DIVISAS.EXTRACT.POST
*----------------------------------------------------------------------------------------------------------------------------------
*
* Description  : This routine will get the details from work file and writes into text file.
*
*-------------------------------------------------------------------------
* Date              Author                    Description
* ==========        ====================      ============
* 05-Dec-2017  Ashokkumar.V.P      CN007023 - Initial Version.
*-------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.BATCH
    $INSERT T24.BP I_F.DATES
    $INCLUDE REGREP.BP I_F.DR.REG.FD01.PARAM
*
    GOSUB OPEN.FILES
    GOSUB INIT.PARA
    GOSUB OPEN.EXTRACT.FILE
    GOSUB PROCESS.PARA
*
    RETURN

*----------------------------------------------------------
OPEN.FILES:
***********

    FN.DR.REG.FD01.PARAM = 'F.DR.REG.FD01.PARAM'
    F.DR.REG.FD01.PARAM = ''
    CALL OPF(FN.DR.REG.FD01.PARAM,F.DR.REG.FD01.PARAM)

    FN.DR.REG.DIVIAS.WORKFILE = "F.DR.REG.DIVIAS.WORKFILE"
    F.DR.REG.DIVIAS.WORKFILE = ""
    CALL OPF(FN.DR.REG.DIVIAS.WORKFILE, F.DR.REG.DIVIAS.WORKFILE)
*
    RETURN
*-------------------------------------------------------------------
INIT.PARA:
**********

    R.DR.REG.FD01.PARAM = ''; DR.REG.FD01.PARAM.ERR = ''
    CALL CACHE.READ(FN.DR.REG.FD01.PARAM,'SYSTEM',R.DR.REG.FD01.PARAM,DR.REG.FD01.PARAM.ERR)

    FN.CHK.DIR = R.DR.REG.FD01.PARAM<DR.FD01.PARAM.OUT.PATH>
    F.CHK.DIR = ''
    CALL OPF(FN.CHK.DIR,F.CHK.DIR)

    R.FILE.DATA = ''
    RETURN

*-------------------------------------------------------------------
OPEN.EXTRACT.FILE:
******************
    OPEN.ERR = '' ; EXTRACT.FILE.ID = '' ; READ.FIL.ERR = '' ; R.FIL = ''
    Y.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
    Y.DATE = Y.DATE[7,2]:Y.DATE[5,2]:Y.DATE[1,4]
    EXTRACT.FILE.ID = 'AR012':Y.DATE:'.txt'

    CALL F.READ(FN.CHK.DIR,EXTRACT.FILE.ID,R.FIL,F.CHK.DIR,READ.FIL.ERR)
    IF R.FIL THEN
        DELETE F.CHK.DIR,EXTRACT.FILE.ID
    END
    RETURN

PROCESS.PARA:
*************

    ID.LIST = ''; ID.POS = '' ;  ERR.SEL = ''; ID.CNT = ''
    R.REC = ''; YSEQ.FLAG = 1
    SEL.CMD = "SELECT ":FN.DR.REG.DIVIAS.WORKFILE
    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)
    LOOP
        REMOVE REC.ID FROM ID.LIST SETTING ID.POS
    WHILE REC.ID:ID.POS
        CALL F.READ(FN.DR.REG.DIVIAS.WORKFILE, REC.ID, R.REC, F.DR.REG.DIVIAS.WORKFILE, RD.ERR)
        IF R.REC THEN
            R.FILE.DATA<-1> = YSEQ.FLAG:'|':R.REC:'|'
            YSEQ.FLAG++
        END
    REPEAT
*
    CRLF = CHARX(013):CHARX(010)
    CHANGE FM TO CRLF IN R.FILE.DATA
    WRITE R.FILE.DATA ON F.CHK.DIR, EXTRACT.FILE.ID ON ERROR
        CALL OCOMO("Unable to write to the file":F.CHK.DIR)
    END

    RETURN
*-------------------------------------------------------------------
END
