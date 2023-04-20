SUBROUTINE DR.REG.RIEN9.EXTRACT.POST
*----------------------------------------------------------------------------------------------------------------------------------
*
* Description  : This routine will get the details from work file and writes into text file.
*
*-------------------------------------------------------------------------
* Date              Author                    Description
* ==========        ====================      ============
* 28-08-2014        V.P.Ashokkumar            PACS00313072- Fixed all the fields
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BATCH
    $INSERT I_F.DATES
    $INSERT I_F.DR.REG.RIEN9.PARAM
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

    FN.DR.REG.RIEN9.PARAM = 'F.DR.REG.RIEN9.PARAM'
    F.DR.REG.RIEN9.PARAM = ''
    CALL OPF(FN.DR.REG.RIEN9.PARAM,F.DR.REG.RIEN9.PARAM)

    FN.DR.REG.RIEN9.WORKFILE = "F.DR.REG.RIEN9.WORKFILE"
    FV.DR.REG.RIEN9.WORKFILE = ""
    CALL OPF(FN.DR.REG.RIEN9.WORKFILE, FV.DR.REG.RIEN9.WORKFILE)

    FN.DR.REG.RIEN9.WORKFILE.FCY = "F.DR.REG.RIEN9.WORKFILE.FCY"
    FV.DR.REG.RIEN9.WORKFILE.FCY = ""
    CALL OPF(FN.DR.REG.RIEN9.WORKFILE.FCY, FV.DR.REG.RIEN9.WORKFILE.FCY)
*
RETURN
*-------------------------------------------------------------------
INIT.PARA:
**********

*    CALL F.READ(FN.DR.REG.RIEN9.PARAM,'SYSTEM',R.DR.REG.RIEN9.PARAM,F.DR.REG.RIEN9.PARAM,DR.REG.RIEN9.PARAM.ERR)
    DR.REG.RIEN9.PARAM.ERR = ''; R.DR.REG.RIEN9.PARAM = ''
    CALL CACHE.READ(FN.DR.REG.RIEN9.PARAM,'SYSTEM',R.DR.REG.RIEN9.PARAM,DR.REG.RIEN9.PARAM.ERR)
    FN.CHK.DIR = R.DR.REG.RIEN9.PARAM<RIEN9.PARAM.OUT.PATH>
RETURN

*-------------------------------------------------------------------
OPEN.EXTRACT.FILE:
******************
    OPEN.ERR = ''
    EXTRACT.FILE.ID = R.DR.REG.RIEN9.PARAM<RIEN9.PARAM.REP.NAME,1>:'.txt'
    OPENSEQ FN.CHK.DIR,EXTRACT.FILE.ID TO FV.EXTRACT.FILE THEN
        DELETESEQ FN.CHK.DIR,EXTRACT.FILE.ID THEN
        END ELSE
            NULL    ;* In case if it exisit DELETE, for Safer side
        END
        OPENSEQ FN.CHK.DIR,EXTRACT.FILE.ID TO FV.EXTRACT.FILE ELSE    ;* After DELETE file pointer will be closed, hence reopen the file
            CREATE FV.EXTRACT.FILE THEN
            END ELSE
                OPEN.ERR = 1
            END
        END
    END ELSE
        CREATE FV.EXTRACT.FILE THEN
        END ELSE
            OPEN.ERR = 1
        END
    END

    IF OPEN.ERR THEN
        TEXT = "Unable to Create a File -> ":EXTRACT.FILE.ID
        CALL FATAL.ERROR("DR.REG.RIEN9.EXTRACT.POST")
    END

    GOSUB OPEN.EXTRACT.FILE.FCY
RETURN

*-------------------------------------------------------------------
OPEN.EXTRACT.FILE.FCY:
*--------------------*
*
    OPEN.ERR1 = ''
    EXTRACT.FILE.ID1 = R.DR.REG.RIEN9.PARAM<RIEN9.PARAM.REP.NAME,2>:'.txt'
    OPENSEQ FN.CHK.DIR,EXTRACT.FILE.ID1 TO FV.EXTRACT.FILE1 THEN
        DELETESEQ FN.CHK.DIR,EXTRACT.FILE.ID1 THEN
        END ELSE
            NULL    ;* In case if it exisit DELETE, for Safer side
        END
        OPENSEQ FN.CHK.DIR,EXTRACT.FILE.ID1 TO FV.EXTRACT.FILE1 ELSE  ;* After DELETE file pointer will be closed, hence reopen the file
            CREATE FV.EXTRACT.FILE1 THEN
            END ELSE
                OPEN.ERR = 1
            END
        END
    END ELSE
        CREATE FV.EXTRACT.FILE1 THEN
        END ELSE
            OPEN.ERR = 1
        END
    END

    IF OPEN.ERR1 THEN
        TEXT = "Unable to Create a File -> ":EXTRACT.FILE.ID
        CALL FATAL.ERROR("DR.REG.RIEN9.EXTRACT.POST")
    END
RETURN
*-------------------------------------------------------------------
PROCESS.PARA:
*************

    SEL.CMD = "SELECT ":FN.DR.REG.RIEN9.WORKFILE
    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)

    GOSUB WRITE.HEADER

    LOOP
        REMOVE REC.ID FROM ID.LIST SETTING ID.POS
    WHILE REC.ID:ID.POS
        CALL F.READ(FN.DR.REG.RIEN9.WORKFILE, REC.ID, R.REC, FV.DR.REG.RIEN9.WORKFILE, RD.ERR)
        IF R.REC THEN
            CRLF = CHARX(013):CHARX(010)
            CHANGE @FM TO CRLF IN R.REC
            WRITESEQ R.REC TO FV.EXTRACT.FILE THEN
            END ELSE
                NULL
            END
        END
    REPEAT
*
    GOSUB PROCESS.PARA.FCY
*
RETURN
*-------------------------------------------------------------------
PROCESS.PARA.FCY:
****************
*
    SEL.CMD = ''
    ID.LIST = ''
    ID.CNT = ''
    ERR.SEL = ''
    SEL.CMD = "SELECT ":FN.DR.REG.RIEN9.WORKFILE.FCY
    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)

    GOSUB WRITE.HEADER.FCY

    LOOP
        REMOVE REC.ID FROM ID.LIST SETTING ID.POS
    WHILE REC.ID:ID.POS
        CALL F.READ(FN.DR.REG.RIEN9.WORKFILE.FCY, REC.ID, R.REC, FV.DR.REG.RIEN9.WORKFILE.FCY, RD.ERR)
        IF R.REC THEN
            CRLF = CHARX(013):CHARX(010)
            CHANGE @FM TO CRLF IN R.REC
            WRITESEQ R.REC TO FV.EXTRACT.FILE1 THEN
            END ELSE
                NULL
            END
        END
    REPEAT
*
RETURN
*-------------------------------------------------------------------
WRITE.HEADER.FCY:
*****************
*
    WRITESEQ R.DR.REG.RIEN9.PARAM<RIEN9.PARAM.RNC> TO FV.EXTRACT.FILE1 THEN
    END ELSE
        NULL
    END
    WRITESEQ R.DR.REG.RIEN9.PARAM<RIEN9.PARAM.REP.NAME,2> TO FV.EXTRACT.FILE1 THEN
    END ELSE
        NULL
    END
    LAST.WORK.VAL = R.DATES(EB.DAT.LAST.WORKING.DAY)
    LAST.DATE = LAST.WORK.VAL[7,2]:"/":LAST.WORK.VAL[5,2]:"/":LAST.WORK.VAL[1,4]
    WRITESEQ LAST.DATE TO FV.EXTRACT.FILE1 THEN
    END ELSE
        NULL
    END
    ID.CNT = FMT(ID.CNT,'R%12')
    WRITESEQ ID.CNT TO FV.EXTRACT.FILE1 THEN
    END ELSE
        NULL
    END
    WRITESEQ R.DR.REG.RIEN9.PARAM<RIEN9.PARAM.STATUS> TO FV.EXTRACT.FILE1 THEN
    END ELSE
        NULL
    END
    WRITESEQ R.DR.REG.RIEN9.PARAM<RIEN9.PARAM.STRECORD> TO FV.EXTRACT.FILE1 THEN
    END ELSE
        NULL
    END
*
RETURN
*-------------------------------------------------------------------
WRITE.HEADER:
*-----------*
*
    WRITESEQ R.DR.REG.RIEN9.PARAM<RIEN9.PARAM.RNC> TO FV.EXTRACT.FILE THEN
    END ELSE
        NULL
    END
    WRITESEQ R.DR.REG.RIEN9.PARAM<RIEN9.PARAM.REP.NAME,1> TO FV.EXTRACT.FILE THEN
    END ELSE
        NULL
    END
    LAST.WORK.VAL = R.DATES(EB.DAT.LAST.WORKING.DAY)
    LAST.DATE = LAST.WORK.VAL[7,2]:"/":LAST.WORK.VAL[5,2]:"/":LAST.WORK.VAL[1,4]
    WRITESEQ LAST.DATE TO FV.EXTRACT.FILE THEN
    END ELSE
        NULL
    END
    ID.CNT = FMT(ID.CNT,'R%12')
    WRITESEQ ID.CNT TO FV.EXTRACT.FILE THEN
    END ELSE
        NULL
    END
    WRITESEQ R.DR.REG.RIEN9.PARAM<RIEN9.PARAM.STATUS> TO FV.EXTRACT.FILE THEN
    END ELSE
        NULL
    END
    WRITESEQ R.DR.REG.RIEN9.PARAM<RIEN9.PARAM.STRECORD> TO FV.EXTRACT.FILE THEN
    END ELSE
        NULL
    END
RETURN
*-------------------------------------------------------------------
END
