* @ValidationCode : MjoxNjA0OTE2MDYxOkNwMTI1MjoxNjg0ODU0Mzg3Mjg2OklUU1M6LTE6LTE6OTQ2OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 23 May 2023 20:36:27
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 946
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.GEN.ACH.REJINW.FILE.POST
*-----------------------------------------------------------------------------
*DESCRIPTION:
*-----------------------------------------------------------------------------
* Input/Output:
*-----------------------------------------------------------------------------
* IN  : -NA-
* OUT : -NA-
*------------------------------------------------------------------------------
* Dependencies:
*-------------------------------------------------------------------------------
* CALLS : -NA-
* CALLED BY : -NA-
*--------------------------------------------------------------------------------
* Revision History:
*---------------------------------------------------------------------------------
*   Date               who           Reference            Description
* 04-Oct-2010        Harish.Y       ODR-2009-12-0290    Initial Creation
* 10-APR-2013        Shesharaj      PERF-CHANGE            Performance Changes
* Date                   who                   Reference              
* 11-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - FM TO @FM AND ++ TO += 1 AND TNO TO C$T24.SESSION.NO 
* 11-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*---------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.USER
    $INSERT I_F.REDO.ACH.PROCESS
    $INSERT I_F.REDO.ACH.PROCESS.DET
    $INSERT I_F.REDO.INTERFACE.PARAM
    $INSERT I_F.REDO.ACH.PARAM

*!*PERF-CHANGE - Start
    $INSERT I_REDO.B.GEN.ACH.REJINW.FILE.COMMON
*!*PERF-CHANGE - End

    GOSUB INIT
    GOSUB PROCESS
    GOSUB ACH.PROC
*    GOSUB INTERFACE.UPDATE -removed the part to update c.22 log.

RETURN
*******
INIT:
*******

    FN.REDO.ACH.PROCESS ='F.REDO.ACH.PROCESS'
    F.REDO.ACH.PROCESS = ''
    CALL OPF(FN.REDO.ACH.PROCESS,F.REDO.ACH.PROCESS)

    FN.REDO.ACH.PROCESS.DET = 'F.REDO.ACH.PROCESS.DET'
    F.REDO.ACH.PROCESS.DET = ''
    CALL OPF(FN.REDO.ACH.PROCESS.DET,F.REDO.ACH.PROCESS.DET)

*!*PERF-CHANGE - Start
*FN.REDO.INTERFACE.PARAM = 'F.REDO.INTERFACE.PARAM' ;* Already Available in I_REDO.B.GEN.ACH.REJINW.FILE.COMMON
*F.REDO.INTERFACE.PARAM = ''
*CALL OPF(FN.REDO.INTERFACE.PARAM,F.REDO.INTERFACE.PARAM)

*FN.REDO.ACH.PARAM = 'F.REDO.ACH.PARAM'
*F.REDO.ACH.PARAM  =''
*CALL OPF(FN.REDO.ACH.PARAM,F.REDO.ACH.PARAM)
*!*PERF-CHANGE - End

    FN.REDO.ACH.PROC.IDS = 'F.REDO.ACH.PROC.IDS'
    F.REDO.ACH.PROC.IDS = ''
    CALL OPF(FN.REDO.ACH.PROC.IDS,F.REDO.ACH.PROC.IDS)

RETURN

*********
PROCESS:
*********

*!*PERF-CHANGE - Start    ;* Already Available in I_REDO.B.GEN.ACH.REJINW.FILE.COMMON
*INT.ID = 'ACH004'

*CALL F.READ(FN.REDO.INTERFACE.PARAM,INT.ID,R.INT.PARAM,F.REDO.INTERFACE.PARAM,INT.PARAM.ERR)
*IF R.INT.PARAM THEN
*Y.INTERF.ID=INT.ID
*Y.OUT.PATH=R.INT.PARAM<REDO.INT.PARAM.DIR.PATH>
*END
*CALL F.READ(FN.REDO.ACH.PARAM,'SYSTEM',R.REDO.ACH.PARAM,F.REDO.ACH.PARAM,ACH.PARAM.ERR)
*CALL CACHE.READ(FN.REDO.ACH.PARAM,"SYSTEM",R.REDO.ACH.PARAM,ACH.PARAM.ERR)
*IF R.REDO.ACH.PARAM THEN
*Y.FILE.PREFIX  = R.REDO.ACH.PARAM<REDO.ACH.PARAM.INW.REJ.FIL.PRE>
*Y.IN.PATH.HIS = R.REDO.ACH.PARAM<REDO.ACH.PARAM.IN.REJ.HIST.PATH>
*Y.PATH.HIST  = R.REDO.ACH.PARAM<REDO.ACH.PARAM.OUT.RJ.HIS.PATH>
*END
*!*PERF-CHANGE - End

    SHELL.CMD ='SH -c '
    EXEC.COM="cat "
    OLD.OUT.FILES = 'Reject.File':'*'

    FINAL.Y.FILE.NAME = Y.FILE.PREFIX:'.':TODAY:'.':TIME()

    EXE.CAT = "cat ":Y.OUT.PATH:"/":OLD.OUT.FILES:" >> ":Y.OUT.PATH:"/":FINAL.Y.FILE.NAME
    EXE.RM="rm ":Y.OUT.PATH:"/":OLD.OUT.FILES
*EXE.CPY="cp ":Y.OUT.PATH:"/":FINAL.Y.FILE.NAME:" ":Y.PATH.HIST    ;*Saving One more Copy in Y.PATH.HIST Location
    EXE.CPY="cp ":Y.OUT.PATH:"/":FINAL.Y.FILE.NAME:" ":Y.IN.PATH.HIS    ;* PERF-CHANGE


    DAEMON.CMD = SHELL.CMD:EXE.CAT
    DAEMON.REM.CMD = SHELL.CMD:EXE.RM
    DAEMON.CPY.CMD = SHELL.CMD:EXE.CPY


    EXECUTE DAEMON.CMD RETURNING RETURN.VALUE CAPTURING CAPTURE.CAT.VALUE
    EXECUTE DAEMON.REM.CMD RETURNING RETURN.VALUE CAPTURING CAPTURE.REM.VALUE
    EXECUTE DAEMON.CPY.CMD RETURNING RETURN.VALUE CAPTURING CAPTURE.CPY.VALUE

RETURN
*---------------------
ACH.PROC:
    CALL F.READ(FN.REDO.ACH.PROC.IDS,TODAY,R.REDO.ACH.PROC.IDS,F.REDO.ACH.PROC.IDS,ID.ERR)
    TOTAL.CNTR = DCOUNT(R.REDO.ACH.PROC.IDS,@FM)
* ONLY ONE FEILD so no need of loop
    LOOP.CNTR = 1
    LOOP
    WHILE LOOP.CNTR LE TOTAL.CNTR
        FETCH.ID = R.REDO.ACH.PROC.IDS<LOOP.CNTR>
        CALL F.READ(FN.REDO.ACH.PROCESS,FETCH.ID,R.REDO.ACH.PROCESS,F.REDO.ACH.PROCESS,Y.ERR.ACH.PROC)
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.FILE.NAME> = FINAL.Y.FILE.NAME

        Y.CURR.NO = 1
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.CURR.NO> = Y.CURR.NO
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.DEPT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.INPUTTER> = C$T24.SESSION.NO:'_':OPERATOR ;*R22 AUTO CONVERSTION TNO TO C$T24.SESSION.NO
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.AUTHORISER> = C$T24.SESSION.NO:'_':OPERATOR  ;*R22 AUTO CONVERSTION TNO TO C$T24.SESSION.NO
        TEMPTIME = OCONV(TIME(),"MTS")
        TEMPTIME1 = TEMPTIME[1,5]
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.DATE.TIME> = OCONV(DATE(),"D2"):' ':TEMPTIME1
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.CO.CODE> = ID.COMPANY

        CALL F.WRITE(FN.REDO.ACH.PROCESS,FETCH.ID,R.REDO.ACH.PROCESS)
        LOOP.CNTR += 1
    REPEAT
    CALL F.DELETE(FN.REDO.ACH.PROC.IDS,TODAY)
RETURN
*-----------------------
END
*--------------------------------------------------------------------------------------------------------------*
