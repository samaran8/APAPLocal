* @ValidationCode : MjozMzk2ODM4Mzg6Q3AxMjUyOjE2ODExMjA1MTc1MDQ6YWppdGg6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Apr 2023 15:25:17
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.DRREG
*
*--------------------------------------------------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE DR.REG.RIEN8.EXTRACT.POST
*----------------------------------------------------------------------------------------------------------------------------------
*
* Description  : This routine will get the details from work file and writes into text file.
*
*-------------------------------------------------------------------------
* Date              Author                    Description
* ==========        ====================      ============
*MODIFICATION HISTORY:
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*10-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   FM to @FM
*10-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------





*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BATCH
    $INSERT I_F.DATES
    $INSERT I_F.DR.REG.RIEN8.PARAM
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

    FN.DR.REG.RIEN8.PARAM = 'F.DR.REG.RIEN8.PARAM'
    F.DR.REG.RIEN8.PARAM = ''
    CALL OPF(FN.DR.REG.RIEN8.PARAM,F.DR.REG.RIEN8.PARAM)

    FN.DR.REG.RIEN8.WORKFILE = "F.DR.REG.RIEN8.WORKFILE"
    FV.DR.REG.RIEN8.WORKFILE = ""
    CALL OPF(FN.DR.REG.RIEN8.WORKFILE, FV.DR.REG.RIEN8.WORKFILE)

    FN.DR.REG.RIEN8.WORKFILE.FCY = "F.DR.REG.RIEN8.WORKFILE.FCY"
    FV.DR.REG.RIEN8.WORKFILE.FCY = ""
    CALL OPF(FN.DR.REG.RIEN8.WORKFILE.FCY, FV.DR.REG.RIEN8.WORKFILE.FCY)
*
RETURN
*-------------------------------------------------------------------
INIT.PARA:
**********

*  CALL F.READ(FN.DR.REG.RIEN8.PARAM,'SYSTEM',R.DR.REG.RIEN8.PARAM,F.DR.REG.RIEN8.PARAM,DR.REG.RIEN8.PARAM.ERR) ;*Tus Start
    CALL CACHE.READ(FN.DR.REG.RIEN8.PARAM,'SYSTEM',R.DR.REG.RIEN8.PARAM,DR.REG.RIEN8.PARAM.ERR) ; * Tus End
    FN.CHK.DIR = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.OUT.PATH>

RETURN

*-------------------------------------------------------------------
OPEN.EXTRACT.FILE:
******************
    OPEN.ERR = ''
    EXTRACT.FILE.ID = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.REP.NAME,1>:'.txt'
    OPENSEQ FN.CHK.DIR,EXTRACT.FILE.ID TO FV.EXTRACT.FILE THEN
        DELETESEQ FN.CHK.DIR,EXTRACT.FILE.ID ELSE NULL          ;* In case if it exisit DELETE, for Safer side
        OPENSEQ FN.CHK.DIR,EXTRACT.FILE.ID TO FV.EXTRACT.FILE ELSE        ;* After DELETE file pointer will be closed, hence reopen the file
            CREATE FV.EXTRACT.FILE ELSE OPEN.ERR = 1
        END
    END ELSE
        CREATE FV.EXTRACT.FILE ELSE OPEN.ERR = 1
    END

    IF OPEN.ERR THEN
        TEXT = "Unable to Create a File -> ":EXTRACT.FILE.ID
        CALL FATAL.ERROR("DR.REG.RIEN8.EXTRACT.POST")
    END

    GOSUB OPEN.EXTRACT.FILE.FCY

RETURN

*-------------------------------------------------------------------
OPEN.EXTRACT.FILE.FCY:
*--------------------*
*
    OPEN.ERR1 = ''
    EXTRACT.FILE.ID1 = R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.REP.NAME,2>:'.txt'
    OPENSEQ FN.CHK.DIR,EXTRACT.FILE.ID1 TO FV.EXTRACT.FILE1 THEN
        DELETESEQ FN.CHK.DIR,EXTRACT.FILE.ID1 ELSE NULL         ;* In case if it exisit DELETE, for Safer side
        OPENSEQ FN.CHK.DIR,EXTRACT.FILE.ID1 TO FV.EXTRACT.FILE1 ELSE      ;* After DELETE file pointer will be closed, hence reopen the file
            CREATE FV.EXTRACT.FILE1 ELSE OPEN.ERR = 1
        END
    END ELSE
        CREATE FV.EXTRACT.FILE1 ELSE OPEN.ERR = 1
    END

    IF OPEN.ERR1 THEN
        TEXT = "Unable to Create a File -> ":EXTRACT.FILE.ID
        CALL FATAL.ERROR("DR.REG.RIEN8.EXTRACT.POST")
    END

*
RETURN
*-------------------------------------------------------------------
PROCESS.PARA:
*************

    SEL.CMD = "SELECT ":FN.DR.REG.RIEN8.WORKFILE
    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)

    GOSUB WRITE.HEADER

    LOOP
        REMOVE REC.ID FROM ID.LIST SETTING ID.POS
    WHILE REC.ID:ID.POS
        CALL F.READ(FN.DR.REG.RIEN8.WORKFILE, REC.ID, R.REC, FV.DR.REG.RIEN8.WORKFILE, RD.ERR)
        IF R.REC THEN
            CRLF = CHARX(013):CHARX(010)
            CHANGE @FM TO CRLF IN R.REC
            WRITESEQ R.REC TO FV.EXTRACT.FILE ELSE NULL
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
    SEL.CMD = "SELECT ":FN.DR.REG.RIEN8.WORKFILE.FCY
    CALL EB.READLIST(SEL.CMD, ID.LIST, "", ID.CNT, ERR.SEL)

    GOSUB WRITE.HEADER.FCY

    LOOP
        REMOVE REC.ID FROM ID.LIST SETTING ID.POS
    WHILE REC.ID:ID.POS
        CALL F.READ(FN.DR.REG.RIEN8.WORKFILE.FCY, REC.ID, R.REC, FV.DR.REG.RIEN8.WORKFILE.FCY, RD.ERR)
        IF R.REC THEN
            CRLF = CHARX(013):CHARX(010)
            CHANGE @FM TO CRLF IN R.REC
            WRITESEQ R.REC TO FV.EXTRACT.FILE1 ELSE NULL
        END
    REPEAT
*
RETURN
*-------------------------------------------------------------------
WRITE.HEADER.FCY:
*****************
*
    WRITESEQ R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.RNC> TO FV.EXTRACT.FILE1 ELSE NULL
    WRITESEQ R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.REP.NAME,2> TO FV.EXTRACT.FILE1 ELSE NULL
    LAST.WORK.VAL = R.DATES(EB.DAT.LAST.WORKING.DAY)
    LAST.DATE = LAST.WORK.VAL[7,2]:"/":LAST.WORK.VAL[5,2]:"/":LAST.WORK.VAL[1,4]
    WRITESEQ LAST.DATE TO FV.EXTRACT.FILE1 ELSE NULL
    WRITESEQ ID.CNT TO FV.EXTRACT.FILE1 ELSE NULL
    WRITESEQ R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.STATUS> TO FV.EXTRACT.FILE1 ELSE NULL
    WRITESEQ R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.STRECORD> TO FV.EXTRACT.FILE1 ELSE NULL
*
RETURN
*-------------------------------------------------------------------
WRITE.HEADER:
*-----------*
*
    WRITESEQ R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.RNC> TO FV.EXTRACT.FILE ELSE NULL
    WRITESEQ R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.REP.NAME,1> TO FV.EXTRACT.FILE ELSE NULL
    LAST.WORK.VAL = R.DATES(EB.DAT.LAST.WORKING.DAY)
    LAST.DATE = LAST.WORK.VAL[7,2]:"/":LAST.WORK.VAL[5,2]:"/":LAST.WORK.VAL[1,4]
    WRITESEQ LAST.DATE TO FV.EXTRACT.FILE ELSE NULL
    WRITESEQ ID.CNT TO FV.EXTRACT.FILE ELSE NULL
    WRITESEQ R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.STATUS> TO FV.EXTRACT.FILE ELSE NULL
    WRITESEQ R.DR.REG.RIEN8.PARAM<RIEN8.PARAM.STRECORD> TO FV.EXTRACT.FILE ELSE NULL
*
RETURN
*-------------------------------------------------------------------
END
