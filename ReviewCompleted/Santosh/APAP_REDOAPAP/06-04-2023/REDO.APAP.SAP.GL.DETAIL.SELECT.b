* @ValidationCode : MjotNTk2MDU4ODA3OkNwMTI1MjoxNjgwNzYwNzU0MDc2OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 06 Apr 2023 11:29:14
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.SAP.GL.DETAIL.SELECT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_F.DATES
    $INSERT I_F.COMPANY
    $INSERT I_F.MNEMONIC.COMPANY
    $INSERT I_F.RE.TXN.CODE
    $INSERT I_REDO.APAP.SAP.GL.DETAIL.COMMON
    $INSERT I_F.REDO.GL.H.EXTRACT.PARAMETER
    
*-------------------------------------------------------------------------------------
*Modification
* Date                  who                   Reference              
* 06-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION SM TO @SM
* 06-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*-------------------------------------------------------------------------------------

    GOSUB REMOVE.FILES
    GOSUB GET.SEL.DATE
    GOSUB GET.SEL.DATE.OL
    GOSUB SELECT.RE.STAT.LINE.BAL
RETURN

*************
GET.SEL.DATE:
*************

    Y.DATE = BATCH.DETAILS<3,1,1>
    IF Y.DATE THEN
        Y.SYS.DATE = Y.DATE
    END ELSE
        Y.SYS.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
    END
    Y.MONTH.END.DATE = Y.SYS.DATE
    Y.NEXT.DAY = Y.SYS.DATE
    Y.REGION = ''
    Y.DAYS   = '+1C'
    CALL CDT(Y.REGION,Y.NEXT.DAY,Y.DAYS)

    CALL AWD(Y.REGION,Y.NEXT.DAY,Y.RET.VAL)
    IF Y.RET.VAL EQ 'H' THEN

        Y.DAYS = '+1W'
        CALL CDT(Y.REGION,Y.SYS.DATE,Y.DAYS)
        Y.NEXT.WK.DATE=Y.SYS.DATE
        Y.DAYS   = '-1C'
        CALL CDT(Y.REGION,Y.SYS.DATE,Y.DAYS)
        Y.RE.MONTH = Y.MONTH.END.DATE[5,2]
        Y.NE.MONTH = Y.NEXT.WK.DATE[5,2]
        IF Y.NE.MONTH NE Y.RE.MONTH THEN
            GOSUB GET.MONTH.END
        END
    END
    Y.START.DATE = Y.SYS.DATE
RETURN
*--------------------------------------------------------------------------------------------------------

GET.SEL.DATE.OL:
****************
    IF Y.DATE THEN
        Y.SYS.DATE = Y.DATE
    END ELSE
        Y.SYS.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
    END

    Y.DAYS = '-1W'
    CALL CDT(Y.REGION,Y.SYS.DATE,Y.DAYS)
    Y.MONTH.END.DATE = Y.SYS.DATE
    Y.NEXT.DAY = Y.SYS.DATE
    Y.REGION = ''
    Y.DAYS   = '+1C'
    CALL CDT(Y.REGION,Y.NEXT.DAY,Y.DAYS)

    CALL AWD(Y.REGION,Y.NEXT.DAY,Y.RET.VAL)
    IF Y.RET.VAL EQ 'H' THEN

        Y.DAYS = '+1W'
        CALL CDT(Y.REGION,Y.SYS.DATE,Y.DAYS)
        Y.NEXT.WK.DATE=Y.SYS.DATE
        Y.DAYS   = '-1C'
        CALL CDT(Y.REGION,Y.SYS.DATE,Y.DAYS)
        Y.RE.MONTH = Y.MONTH.END.DATE[5,2]
        Y.NE.MONTH = Y.NEXT.WK.DATE[5,2]
        IF Y.NE.MONTH NE Y.RE.MONTH THEN
            GOSUB GET.MONTH.END
        END
    END
    Y.END.DATE = Y.SYS.DATE
RETURN

**************
GET.MONTH.END:
**************
    Y.ME.YEAR  = Y.MONTH.END.DATE[1,4]
    Y.ME.MONTH = Y.MONTH.END.DATE[5,2]
    Y.ME.DAY   = Y.MONTH.END.DATE[2]

    IF Y.ME.MONTH EQ '01' OR Y.ME.MONTH EQ '03' OR Y.ME.MONTH EQ '05' OR Y.ME.MONTH EQ '07' OR Y.ME.MONTH EQ '08' OR Y.ME.MONTH EQ '10' OR Y.ME.MONTH EQ '12' THEN
        Y.ME.DAY = 31
    END

    IF Y.ME.MONTH EQ '04' OR Y.ME.MONTH EQ '06' OR Y.ME.MONTH EQ '09' OR Y.ME.MONTH EQ '11' THEN
        Y.ME.DAY = 30
    END

    IF Y.ME.MONTH EQ '02' THEN
        Y.MOD = MOD(Y.ME.YEAR,4)
        IF Y.MOD EQ 0 THEN
            Y.ME.DAY = 29
        END ELSE
            Y.ME.DAY = 28
        END
    END

    Y.SYS.DATE = Y.ME.YEAR:Y.ME.MONTH:Y.ME.DAY
RETURN

************************
SELECT.RE.STAT.LINE.BAL:
************************
    Y.REPORT.NAME.LIST = ''
    SEL.LINE.BAL.CMD =''
    Y.REPORT.NAME=''

    DETAIL.GIT.ROUTINE = "SAP.DETAIL.REPORT"
    FIND DETAIL.GIT.ROUTINE IN R.REDO.GL.H.EXTRACT.PARAMETER SETTING DETAIL.REPORT.FM.POS, DETAIL.REPORT.VM.POS THEN
        Y.REPORT.NAME.LIST = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.REPORT.NAME,DETAIL.REPORT.VM.POS>
    END

    Y.TOT.REP.CNT=DCOUNT(Y.REPORT.NAME.LIST,@SM)
    IF Y.TOT.REP.CNT GT 0 THEN  ;*Checking the count of the report name
        Y.REP.FNAME = Y.REPORT.NAME.LIST<1,1,1>
        Y.REP.LNAME = Y.REPORT.NAME.LIST<1,1,2>

        Y.FNAME.STR = SUBSTRINGS(Y.REP.FNAME,0,2)
        Y.LNAME.STR = SUBSTRINGS(Y.REP.LNAME,0,2)

        IF Y.FNAME.STR EQ Y.LNAME.STR THEN
            Y.REPORT.NAME = Y.FNAME.STR
            SEL.LINE.BAL.CMD = "SELECT ":FN.RE.STAT.LINE.BAL:" WITH (@ID LIKE ":Y.REPORT.NAME:"...":Y.START.DATE:"*... OR @ID LIKE ":Y.REPORT.NAME:"...":Y.END.DATE:"*...) AND @ID UNLIKE ...PROFIT... AND @ID UNLIKE ...LOCAL..."
        END ELSE
            SEL.LINE.BAL.CMD = "SELECT ":FN.RE.STAT.LINE.BAL
            SEL.LINE.BAL.CMD := " WITH ((@ID LIKE ":Y.FNAME.STR:"...":Y.START.DATE:"*... OR @ID LIKE ":Y.FNAME.STR:"...":Y.END.DATE:"*...) OR (@ID LIKE ":Y.LNAME.STR:"...":Y.START.DATE:"*... OR @ID LIKE ":Y.LNAME.STR:"...":Y.END.DATE:"*...)) AND @ID UNLIKE ...PROFIT... AND @ID UNLIKE ...LOCAL..."
        END

* 20170926 /S
*SEL.LINE.BAL.CMD = "SELECT ":FN.RE.STAT.LINE.BAL:" WITH (@ID LIKE MB...":Y.START.DATE:"*... OR @ID LIKE MB...":Y.END.DATE:"*...) AND @ID UNLIKE ...PROFIT... AND @ID UNLIKE ...LOCAL..."
* 20170926 /E
        CALL EB.READLIST(SEL.LINE.BAL.CMD,SELECTED.LINE.BAL,'',NO.OF.LINE.BAL,ERR.LINE.BAL)
        CALL BATCH.BUILD.LIST('',SELECTED.LINE.BAL)


        RETURN

*---------------
REMOVE.FILES:
*--------------

        Y.GIT.ROUTINE=R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.GIT.ROUTINE>
        LOCATE 'SAP.DETAIL.REPORT' IN Y.GIT.ROUTINE<1,1> SETTING DET.VM.POS THEN
            Y.EXTRACT.OUT.PATH    = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.EXTRACT.OUT.PATH,DET.VM.POS>
        END

        LOCATE 'SAP.NORMAL.EXTRACT' IN Y.GIT.ROUTINE<1,1> SETTING NORMAL.REPORT.VM.POS THEN
            Y.EXTRACT.OUT.PATH.NOR    = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.EXTRACT.OUT.PATH,NORMAL.REPORT.VM.POS>
        END

        LOCATE 'SAP.REVAL.EXTRACT' IN Y.GIT.ROUTINE<1,1> SETTING REVAL.REPORT.VM.POS THEN
            Y.EXTRACT.OUT.PATH.REV    = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.EXTRACT.OUT.PATH,REVAL.REPORT.VM.POS>
        END

        Y.BACKUP.PATH=R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.BACKUP.PATH>
        SHELL.CMD ='SH -c '
        EXE.MV="mv ":Y.EXTRACT.OUT.PATH:"/*":" ":Y.BACKUP.PATH
        DAEMON.CMD = SHELL.CMD:EXE.MV
        EXECUTE DAEMON.CMD RETURNING RETURN.VALUE CAPTURING CAPTURE.CAT.VALUE
        IF Y.EXTRACT.OUT.PATH NE Y.EXTRACT.OUT.PATH.NOR THEN
            SHELL.CMD ='SH -c '
            EXE.MV="mv ":Y.EXTRACT.OUT.PATH.NOR:"/*":" ":Y.BACKUP.PATH
            DAEMON.CMD = SHELL.CMD:EXE.MV
            EXECUTE DAEMON.CMD RETURNING RETURN.VALUE CAPTURING CAPTURE.CAT.VALUE
        END
        IF Y.EXTRACT.OUT.PATH NE Y.EXTRACT.OUT.PATH.REV AND Y.EXTRACT.OUT.PATH.NOR NE Y.EXTRACT.OUT.PATH.REV THEN
            SHELL.CMD ='SH -c '
            EXE.MV="mv ":Y.EXTRACT.OUT.PATH.REV:"/*":" ":Y.BACKUP.PATH
            DAEMON.CMD = SHELL.CMD:EXE.MV
            EXECUTE DAEMON.CMD RETURNING RETURN.VALUE CAPTURING CAPTURE.CAT.VALUE
        END

        RETURN
    END