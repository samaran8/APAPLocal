SUBROUTINE REDO.PRE.JOB.OFS
*--------------------------------------------------------------
* Description: This is pre Job for Direct debit to check OFS.MESSAGE.QUEUE
* has been cleared for charge messages.
*--------------------------------------------------------------

* Input Argument : NA
* Out Argument   : NA
* Deals With     : BATCH>BNK/REDO.B.DIRECT.DBT
*------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE

    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
*------------------------------------------------------------------------
OPENFILES:
*------------------------------------------------------------------------
    FN.REDO.AA.OFS.FAIL = 'F.REDO.AA.OFS.FAIL'
    F.REDO.AA.OFS.FAIL = ''
    CALL OPF(FN.REDO.AA.OFS.FAIL,F.REDO.AA.OFS.FAIL)

    FN.OFS.MESSAGE.QUEUE = 'F.OFS.MESSAGE.QUEUE'
    F.OFS.MESSAGE.QUEUE = ''
    CALL OPF(FN.OFS.MESSAGE.QUEUE,F.OFS.MESSAGE.QUEUE)

RETURN
*------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------

    INT.CODE = 'COB001'
    INT.TYPE = 'ONLINE'
    BAT.NO   = ''
    BAT.TOT  = ''
    INFO.OR  = ''
    INFO.DE  = ''
    ID.PROC  = ''
    EX.USER  = ''
    EX.PC    = ''

    MON.TP   = '04'
    DESC     = 'Run OFS Message Service(BNK/OFS.MESSAGE.SERVICE)'
    REC.CON  = 'Run OFS Message Service(BNK/OFS.MESSAGE.SERVICE)'

    CALL REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC)

    Y.CNCT.ID = TODAY:'-':'OFS'
    R.RECORD.CNCT = ''
    CALL F.READ(FN.REDO.AA.OFS.FAIL,Y.CNCT.ID,R.RECORD.CNCT,F.REDO.AA.OFS.FAIL,CNCT.ERR)

    IF R.RECORD.CNCT EQ '' THEN
        RETURN
    END

    LOOP
    WHILE R.RECORD.CNCT
        Y.VAR1 =1
        Y.MSG.CNT = DCOUNT(R.RECORD.CNCT,@FM)
        CALL OCOMO("Run OFS.MESSAGE.SERVICE")
        CALL OCOMO("Waiting For OFS.MESSAGE.SERVICE to process ":Y.MSG.CNT:" messages")
        LOOP
        WHILE Y.VAR1 LE Y.MSG.CNT
            R.OFS.MESSAGE = ''
            IF R.RECORD.CNCT<Y.VAR1> THEN
                CALL F.READ(FN.OFS.MESSAGE.QUEUE,R.RECORD.CNCT<Y.VAR1>,R.OFS.MESSAGE,F.OFS.MESSAGE.QUEUE,OFS.MSG.ERR)
                IF R.OFS.MESSAGE THEN
                    Y.VAR1 += 1
                END ELSE
                    DEL R.RECORD.CNCT<Y.VAR1>
                END
            END ELSE

                Y.VAR1 += 1
            END
        REPEAT
        SLEEP 30
    REPEAT
    CALL OCOMO("Messages are processed")
RETURN
END
