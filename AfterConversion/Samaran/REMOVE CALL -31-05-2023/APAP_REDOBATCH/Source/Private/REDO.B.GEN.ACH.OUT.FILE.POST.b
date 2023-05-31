* @ValidationCode : MjoxNzczMDg2NTU6Q3AxMjUyOjE2ODQ4NTQzODcwNTY6SVRTUzotMTotMToyMjU1OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 23 May 2023 20:36:27
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 2255
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.GEN.ACH.OUT.FILE.POST
****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  :  NATCHIMUTHU
* Program Name  : REDO.B.GEN.ACH.OUT.FILE.POST
*-------------------------------------------------------------------------
* Linked with:
* In parameter :
* out parameter : None
*------------------------------------------------------------------------
* MODIFICATION HISTORY
*--------------------------------------------
*   DATE              ODR                             DESCRIPTION
* 08-10-10          ODR-2009-12-0290                  Initial Creation
*14-04-15                                             performance fix -Prabhu
* Date                   who                   Reference              
* 11-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - FM TO @FM AND ++ TO += 1 AND TNO TO C$T24.SESSION.NO
* 11-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_F.USER
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_BATCH.FILES
    $INSERT I_F.REDO.INTERFACE.PARAM
    $INSERT I_F.REDO.ACH.PARAM
    $INSERT I_F.REDO.ACH.PROCESS
    $INSERT I_F.REDO.ACH.PROCESS.DET
    $INSERT I_F.FUNDS.TRANSFER

    GOSUB INIT
    GOSUB PROCESS
*    GOSUB INTERFACE.UPDATE *removed for performance fix.Dont need to update log for  process complete.
RETURN

INIT:
*****

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.FUNDS.TRANSFER.HIS = 'F.FUNDS.TRANSFER$HIS'
    F.FUNDS.TRANSFER.HIS = ''
    CALL OPF(FN.FUNDS.TRANSFER.HIS,F.FUNDS.TRANSFER.HIS)

    FN.REDO.INTERFACE.PARAM = 'F.REDO.INTERFACE.PARAM'
    F.REDO.INTERFACE.PARAM = ''
    CALL OPF(FN.REDO.INTERFACE.PARAM,F.REDO.INTERFACE.PARAM)

    FN.REDO.ACH.DET.IDS = 'F.REDO.ACH.DET.IDS'
    F.REDO.ACH.DET.IDS = ''
    CALL OPF(FN.REDO.ACH.DET.IDS,F.REDO.ACH.DET.IDS)


    FN.REDO.ACH.PARAM = 'F.REDO.ACH.PARAM'
    F.REDO.ACH.PARAM = ''
    CALL OPF(FN.REDO.ACH.PARAM,F.REDO.ACH.PARAM)

    FN.REDO.ACH.PROCESS = 'F.REDO.ACH.PROCESS'
    F.REDO.ACH.PROCESS = ''
    CALL OPF(FN.REDO.ACH.PROCESS,F.REDO.ACH.PROCESS)

    FN.REDO.ACH.PROCESS.DET = 'F.REDO.ACH.PROCESS.DET'
    F.REDO.ACH.PROCESS.DET = ''
    CALL OPF(FN.REDO.ACH.PROCESS.DET,F.REDO.ACH.PROCESS.DET)

    FN.REDO.DUP.ACH.DATE = 'F.REDO.DUP.ACH.DATE'
    F.REDO.DUP.ACH.DATE = ''
    CALL OPF(FN.REDO.DUP.ACH.DATE,F.REDO.DUP.ACH.DATE)

    CCY.OUT.PATH = ''

    Y.FILE.PREFIX = ''
    Y.OUT.PATH.HIS = ''

    ACH.FILE.NAME = ''

    OLD.OUT.FILES = ''

    SHELL.CMD = ''
    EXEC.COM= ''
    OLD.OUT.FILES = ''

    FINAL.CCY.FILE.NAME = ''

    EXE.CAT = ''
    EXE.RM= ''

    DAEMON.CMD = ''
    DAEMON.REM.CMD = ''

    EXE.HIS.CAT = ''
    EXE.HIS.RM = ''
    DAEMON.HIS.CMD = ''
    DAEMON.HISRM.CMD = ''

RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

    Y.INTERF.ID = 'ACH001'

    CALL CACHE.READ(FN.REDO.INTERFACE.PARAM,Y.INTERF.ID,R.REDO.INTERFACE.PARAM,Y.ERR)
    CCY.OUT.PATH = R.REDO.INTERFACE.PARAM<REDO.INT.PARAM.DIR.PATH>
    CALL CACHE.READ(FN.REDO.ACH.PARAM,"SYSTEM",R.REDO.ACH.PARAM,ACH.ERR)
    Y.FILE.PREFIX  = R.REDO.ACH.PARAM<REDO.ACH.PARAM.OUTW.FILE.PREFX>
    Y.OUT.PATH.HIS  = R.REDO.ACH.PARAM<REDO.ACH.PARAM.OUTW.HIST.PATH>
    Y.TODAY = TODAY
    CALL F.READ(FN.REDO.ACH.DET.IDS,Y.TODAY,R.REDO.ACH.DET.IDS,F.REDO.ACH.DET.IDS,ID.ERR)
    SEL.CMD = "SELECT ":CCY.OUT.PATH:" LIKE APAP.ACHOUT":"..."
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',CNT.REC,RET.CD)

    NOW.TIME = TIMEDATE()
    NOW.HR = NOW.TIME[1,2]
    NOW.MIN = NOW.TIME[4,2]
    TOTAL.CNTR = DCOUNT(SEL.LIST,@FM)
    LOOP.CNTR = 1
    LOOP
    WHILE LOOP.CNTR LE TOTAL.CNTR

        SHELL.CMD ='SH -c '
        EXEC.COM="cat "
        TOTAL.CNTR = DCOUNT(SEL.LIST,@FM)
        ACH.FT.ID = SEL.LIST<LOOP.CNTR>
        OLD.OUT.FILES = ACH.FT.ID

        Y.LIVE.HIS = FIELD(ACH.FT.ID,';',2,1)
        Y.FT.ID = FIELD(ACH.FT.ID,'-',2,1)
        IF Y.LIVE.HIS NE '' THEN
            GOSUB TXN.HISTORY.PROCESS
        END ELSE
            GOSUB TXN.LIVE.PROCESS
        END
        Y.TODAY = TODAY
        CALL F.READU(FN.REDO.DUP.ACH.DATE,Y.TODAY,R.REDO.DUP.ACH.DATE,F.REDO.DUP.ACH.DATE,REDO.DUP.ACH.DATE.ERR,'')
        IF R.REDO.DUP.ACH.DATE THEN
            LOCATE Y.FT.ID IN R.REDO.DUP.ACH.DATE SETTING LOC.POS THEN
                DEL R.REDO.DUP.ACH.DATE<LOC.POS>
            END
            CALL F.WRITE(FN.REDO.DUP.ACH.DATE,Y.TODAY,R.REDO.DUP.ACH.DATE)
        END

        GOSUB UPD.LOC.TABLE
        GOSUB UPD.ACH.DET

        LOOP.CNTR += 1
    REPEAT

RETURN
*---------------------------------------------------------------------------------------------------------------------------------
TXN.HISTORY.PROCESS:
*---------------------------------------------------------------------------------------------------------------------------------
    CALL F.READ(FN.FUNDS.TRANSFER.HIS,Y.FT.ID,R.FUNDS.TRANSFER.HIS,F.FUNDS.TRANSFER.HIS,FT.HIS.ERR)
    Y.DEBIT.ACCT.NO  =  R.FUNDS.TRANSFER.HIS<FT.DEBIT.ACCT.NO>
    Y.DEBIT.CUSTOMER =  R.FUNDS.TRANSFER.HIS<FT.DEBIT.CUSTOMER>
    HIS.FILE.NAME =  Y.DEBIT.CUSTOMER:Y.FT.ID::TODAY:NOW.HR:NOW.MIN:'001.bak'

    EXE.HIS.CAT = "cat ":Y.OUT.PATH.HIS:"/":OLD.OUT.FILES:" >> ":Y.OUT.PATH.HIS:"/":HIS.FILE.NAME
    EXE.HIS.RM ="rm ":Y.OUT.PATH.HIS:"/":OLD.OUT.FILES

    DAEMON.HIS.CMD = SHELL.CMD:EXE.HIS.CAT
    DAEMON.HISRM.CMD = SHELL.CMD:EXE.HIS.RM

    RETURN.VALUE = ''
    CAPTURE.CAT.VALUE = ''
    EXECUTE DAEMON.HIS.CMD RETURNING RETURN.VALUE CAPTURING CAPTURE.CAT.VALUE

    RETURN.VALUE = ''
    CAPTURE.CAT.VALUE = ''
    EXECUTE DAEMON.HISRM.CMD RETURNING RETURN.VALUE CAPTURING CAPTURE.REM.VALUE

RETURN
*---------------------------------------------------------------------------------------------------------------------------------
TXN.LIVE.PROCESS:
*---------------------------------------------------------------------------------------------------------------------------------
    CALL F.READ(FN.FUNDS.TRANSFER,Y.FT.ID,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,FT.ERR)
    Y.DEBIT.ACCT.NO  =  R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO>
    Y.DEBIT.CUSTOMER =  R.FUNDS.TRANSFER<FT.DEBIT.CUSTOMER>
    FINAL.CCY.FILE.NAME = Y.DEBIT.CUSTOMER:Y.FT.ID::TODAY:NOW.HR:NOW.MIN:'001'

    EXE.CAT = "cat ":CCY.OUT.PATH:"/":OLD.OUT.FILES:" >> ":CCY.OUT.PATH:"/":FINAL.CCY.FILE.NAME
    EXE.RM="rm ":CCY.OUT.PATH:"/":OLD.OUT.FILES

    DAEMON.CMD = SHELL.CMD:EXE.CAT
    DAEMON.REM.CMD = SHELL.CMD:EXE.RM

    RETURN.VALUE = ''
    CAPTURE.CAT.VALUE = ''
    EXECUTE DAEMON.CMD RETURNING RETURN.VALUE CAPTURING CAPTURE.CAT.VALUE

    RETURN.VALUE = ''
    CAPTURE.CAT.VALUE = ''
    EXECUTE DAEMON.REM.CMD RETURNING RETURN.VALUE CAPTURING CAPTURE.REM.VALUE

RETURN
*---------------------------------------------------------------------------------------------------------------------------------
UPD.LOC.TABLE:

    Y.ID.TEMP = TODAY:".":Y.FT.ID:".TEMP"
    Y.ID.MASTER = TODAY:".":Y.FT.ID:".":TIME()
    CALL F.READ(FN.REDO.ACH.PROCESS,Y.ID.TEMP,R.REDO.ACH.PROCESS,F.REDO.ACH.PROCESS,ACH.ERR)
    IF R.REDO.ACH.PROCESS THEN
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.FILE.NAME> = FINAL.CCY.FILE.NAME
        TEMPTIME = OCONV(TIME(),"MTS")
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.EXEC.TIME> = TEMPTIME

        Y.CURR.NO = 1
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.CURR.NO> = Y.CURR.NO

        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.DEPT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.INPUTTER> = C$T24.SESSION.NO:'_':OPERATOR  ;*R22 AUTO CONVERSTION TNO TO C$T24.SESSION.NO

        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.AUTHORISER> = C$T24.SESSION.NO:'_':OPERATOR ;*R22 AUTO CONVERSTION TNO TO C$T24.SESSION.NO
        TEMPTIME1 = TEMPTIME[1,5]
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.DATE.TIME> = OCONV(DATE(),"D2"):' ':TEMPTIME1
        R.REDO.ACH.PROCESS<REDO.ACH.PROCESS.CO.CODE> = ID.COMPANY

        CALL F.WRITE(FN.REDO.ACH.PROCESS,Y.ID.MASTER,R.REDO.ACH.PROCESS)
        CALL F.DELETE(FN.REDO.ACH.PROCESS,Y.ID.TEMP)
    END

RETURN
*-----------------
UPD.ACH.DET:


    FETCH.ID = R.REDO.ACH.DET.IDS<LOOP.CNTR>
    CALL F.READ(FN.REDO.ACH.PROCESS.DET,FETCH.ID,R.REDO.ACH.PROCESS.DET,F.REDO.ACH.PROCESS.DET,Y.ERR.ACH.PROCESS)
    R.REDO.ACH.PROCESS.DET<REDO.ACH.PROCESS.DET.EXEC.ID> = Y.ID.MASTER

    Y.CURR.NO = 1
    R.REDO.ACH.PROCESS.DET<REDO.ACH.PROCESS.DET.CURR.NO> = Y.CURR.NO
    R.REDO.ACH.PROCESS.DET<REDO.ACH.PROCESS.DET.DEPT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>
    R.REDO.ACH.PROCESS.DET<REDO.ACH.PROCESS.DET.INPUTTER> = C$T24.SESSION.NO:'_':OPERATOR  ;*R22 AUTO CONVERSTION TNO TO C$T24.SESSION.NO
    R.REDO.ACH.PROCESS.DET<REDO.ACH.PROCESS.DET.AUTHORISER> = C$T24.SESSION.NO:'_':OPERATOR  ;*R22 AUTO CONVERSTION TNO TO C$T24.SESSION.NO
    TEMPTIME = OCONV(TIME(),"MTS")
    TEMPTIME1 = TEMPTIME[1,5]
    R.REDO.ACH.PROCESS.DET<REDO.ACH.PROCESS.DET.DATE.TIME> = OCONV(DATE(),"D2"):' ':TEMPTIME1
    R.REDO.ACH.PROCESS.DET<REDO.ACH.PROCESS.DET.CO.CODE> = ID.COMPANY

    CALL F.WRITE(FN.REDO.ACH.PROCESS.DET,FETCH.ID,R.REDO.ACH.PROCESS.DET)
    Y.TODAY = TODAY
    CALL F.DELETE(FN.REDO.ACH.DET.IDS,Y.TODAY)

RETURN
*-----------------------
END
