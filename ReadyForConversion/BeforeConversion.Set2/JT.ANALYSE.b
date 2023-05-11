*-----------------------------------------------------------------------------
* <Rating>1694</Rating>
*-----------------------------------------------------------------------------
    PROGRAM JT.ANALYSE

    OPEN 'F.SPF' TO F.SPF ELSE CRT 'Unable to open F.SPF file' ; STOP
    OPEN 'F.DATES' TO F3 ELSE CRT 'Unable to open F.DATES file' ; STOP
    OPEN 'F.BATCH' TO F2 ELSE CRT 'Unable to open F.BATCH file' ; STOP
    OPEN 'F.JOB.TIMES' TO F1 ELSE CRT 'Unable to open F.JOB.TIMES file' ; STOP
    READ Y.R.SPF FROM F.SPF, 'SYSTEM' THEN
        RELS = Y.R.SPF<8>
        RELS=FIELD(FIELD(RELS,'.',1),'R',2)
    END
    EXECUTE 'SELECT F.DATES'
    READLIST DATES.LIST ELSE DATES.LIST = ''
    DATE.ID = DATES.LIST<1>
    READ DATES.REC FROM F3,DATE.ID ELSE
        DATES.REC=''
        CRT 'Dates record not found'
    END
    IF NOT(DATES.REC) THEN
READ.DATE:
        CRT 'Input COB Date (Ex: 20110101):';INPUT COB.DATE
        IF LEN(COB.DATE) NE 8 THEN CRT 'Invalid Date' ; GOTO READ.DATE:
    END ELSE
        COB.DATE = DATES.REC<3>
    END

    CRT 'Extracting JOB.TIMES for COB.DATE ':COB.DATE
    OUT.FILE='JT.':COB.DATE:'.CSV'
    OPENSEQ OUT.FILE TO F.OUTFILE ELSE CRT 'New file ':OUT.FILE:' Opened'
    CRT 'Selecting BATCH record....'
    HUSH ON
    EXECUTE 'SSELECT F.BATCH WITH BATCH.STAGE NE "" BY BATCH.STAGE'
    HUSH OFF
    CRT.ARRAY=''
    IF RELS GT 8 THEN
        CRT.ARRAY<-1>="BATCH.STAGE,BATCH,JOB.NAME,START.TIME,END.TIME,ELAPSED.TIME,PROCESSED RECS,THROUGHPUT,COMPLETED RECS,SELECTION.TIME,BULK.NO,TXN.MGMT,WRITE.CACHE,LOCK.COLLISION,TOTAL WRITE,,DET PROCESSED,DET COMPLETED,DET THROUGHPUT,DET BULK.NUMBER,DET SELECT,TYPE OF SELECT, DET SELECT TIME"
    END ELSE
        CRT.ARRAY<-1>="BATCH.STAGE,BATCH,JOB.NAME,START.TIME,END.TIME,ELAPSED.TIME,PROCESSED RECS,THROUGHPUT,COMPLETED RECS,NO OF AGENTS,SELECTION.TIME"
    END
    APPL.ANALYSE='';SYS.ANALYSE='';REP.ANALYSE='';STR.ANALYSE='';ONL.ANALYSE=''
    LOOP
        READNEXT ID ELSE ID=''
    WHILE ID
        READ BATCH.REC FROM F2,ID ELSE
            BATCH.REC=''
        END
        BATCH.STAGE=BATCH.REC<1>
        IF BATCH.STAGE[1,1] = 'A' AND NOT(APPL.ANALYSE) THEN
            APPL.ANALYSE='1'
            CRT 'Processing APPLICATION Stage....'
        END
        IF BATCH.STAGE[1,1] = 'S' AND NOT(SYS.ANALYSE) THEN
            SYS.ANALYSE='1'
            CRT 'Processing SYSTEM Stage....'
        END
        IF BATCH.STAGE[1,1] = 'R' AND NOT(REP.ANALYSE) THEN
            REP.ANALYSE='1'
            CRT 'Processing REPORTING Stage....'
        END
        IF BATCH.STAGE[1,1] = 'D' AND NOT(STR.ANALYSE) THEN
            STR.ANALYSE='1'
            CRT 'Processing START.OF.DAY Stage....'
        END
        IF BATCH.STAGE[1,1] = 'O' AND NOT(ONL.ANALYSE) THEN
            ONL.ANALYSE='1'
            CRT 'Processing ONLINE Stage....'
        END
        JOB.LIST=BATCH.REC<6>
        NO.JOBS=DCOUNT(JOB.LIST,@VM)
        ECF.FLAG = ''
        FOR I = 1 TO NO.JOBS
            JT.ID=ID:"-":JOB.LIST<1,I>
            READ JT.REC FROM F1,JT.ID ELSE
                JT.REC=''
            END
            J.DATE=JT.REC<3>

            LOCATE COB.DATE IN J.DATE<1,1> SETTING POS ELSE POS=''
            IF JOB.LIST<1,I> EQ 'EB.CLEAR.FILES' THEN
                IF NOT(ECF.FLAG) THEN
                    ECF.FLAG = '1'
                END ELSE
                    POS = POS + 1
                END
            END
            IF POS THEN

                IF JT.REC<5,POS> LT JT.REC<4,POS> THEN
                    ELAPSED.TIME=86400 - JT.REC<4,POS>
                    ELAPSED.TIME+=JT.REC<5,POS>
                END ELSE
                    ELAPSED.TIME=JT.REC<6,POS>
                END
                ELAPSED.TIME=OCONV(ELAPSED.TIME,'MTS')
                START.TIME=JT.REC<4,POS>

                START.TIME=OCONV(START.TIME,'MTS')
                END.TIME=JT.REC<5,POS>
                END.TIME=OCONV(END.TIME,'MTS')

                SELECT.TIME = JT.REC<12,POS>
                SELECT.TIME = OCONV(SELECT.TIME,'MTS')
                IF RELS GT 8 THEN
                    GOSUB R9U
                END ELSE
                    GOSUB R8
                END

!PROCESSED RECORDS

            END
        NEXT I
    REPEAT
    WRITE.FLAG=''
    NO.OF.RECORDS=DCOUNT(CRT.ARRAY,@FM)
    IF NO.OF.RECORDS EQ '1' THEN NO.OF.RECORDS=''
    FOR I = 1 TO NO.OF.RECORDS
        WRITESEQ CRT.ARRAY<I> ON F.OUTFILE ELSE CRT 'Unable to write out'
        WRITE.FLAG='1'
    NEXT I
    CRT '*******************************************************************'
    IF WRITE.FLAG THEN
        CRT 'Kindly Extract the file JT.':COB.DATE:'.CSV from your RUN directory'
    END ELSE
        CRT 'JOB.TIMES not found'
    END
    CRT '*******************************************************************'
    STOP

R8:
    CURRENT.LINE=BATCH.STAGE:'-':JT.ID:'-':START.TIME:'-':END.TIME:'-':ELAPSED.TIME:'-':JT.REC<7,POS>:'-':JT.REC<8,POS>:'-':JT.REC<9,POS>:'-':JT.REC<11,POS>
    CURRENT.LINE = CURRENT.LINE:'-':SELECT.TIME
    CONVERT '-' TO ',' IN CURRENT.LINE
    CRT.ARRAY<-1>=CURRENT.LINE
    RETURN

R9U:

    PROCESSED= JT.REC<20,POS>
    CONVERT @SM TO @FM IN PROCESSED
    NO.P = DCOUNT(PROCESSED,@FM)
    Y.PROCESSED = ''
    FOR SS = 1 TO NO.P
        Y.PROCESSED+=PROCESSED<SS>
    NEXT SS
    CONVERT @FM TO '*' IN PROCESSED
!COMPLETED RECORDS
    COMPLETED=JT.REC<21,POS>
    CONVERT @SM TO @FM IN COMPLETED
    NO.P = DCOUNT(COMPLETED,@FM)
    Y.COMPLETED = ''
    FOR SS = 1 TO NO.P
        Y.COMPLETED+=COMPLETED<SS>
    NEXT SS
    CONVERT @FM TO '*' IN COMPLETED
!BULK.NUMBER
    BULK.NUMBER=JT.REC<16,POS>
    Y.BULK.NUMBER = BULK.NUMBER<1,1,1>
    CONVERT @SM TO '*' IN BULK.NUMBER
!TYPE OF SELECT
    T.SELECT=JT.REC<17,POS>
    CONVERT @SM TO '*' IN T.SELECT
!SELECTED RECORDS
    SS.SELECT = JT.REC<18,POS>
    CONVERT @SM TO '*' IN SS.SELECT
!SELECT TIME
    S.TIME = JT.REC<19,POS>
    CONVERT @SM TO @FM IN S.TIME
    NO.P = DCOUNT(S.TIME,@FM)
    Y.S.TIME = ''
    FOR SS = 1 TO NO.P
        Y.S.TIME+=S.TIME<SS>
    NEXT SS
    CONVERT @FM TO '*' IN S.TIME
!THROUGHPUT
    THROUGHPUT = JT.REC<8,POS>
    CONVERT @SM TO @FM IN THROUGHPUT
    NO.P = DCOUNT(THROUGHPUT,@FM)
    Y.THROUGHPUT = ''
    FOR SS = 1 TO NO.P
        Y.THROUGHPUT+=THROUGHPUT<SS>
    NEXT SS
    CONVERT @FM TO '*' IN THROUGHPUT
    CURRENT.LINE=BATCH.STAGE:'-':JT.ID:'-':START.TIME:'-':END.TIME:'-':ELAPSED.TIME:'-':Y.PROCESSED:'-':Y.THROUGHPUT:'-':Y.COMPLETED:'-':Y.S.TIME:'-':Y.BULK.NUMBER:'-':JT.REC<22,POS>:'-':JT.REC<23,POS>:'-':JT.REC<24,POS>:'-':JT.REC<25,POS>:'--':PROCESSED:'-':COMPLETED:'-':THROUGHPUT:'-':BULK.NUMBER:'-':SS.SELECT:'-':T.SELECT:'-':S.TIME
    CONVERT '-' TO ',' IN CURRENT.LINE
    CRT.ARRAY<-1>=CURRENT.LINE
    RETURN
