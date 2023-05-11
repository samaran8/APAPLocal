*-----------------------------------------------------------------------------
* <Rating>945</Rating>
*-----------------------------------------------------------------------------
    PROGRAM JOB.LIST.CHECK
*-----------------------------------------------------------------------------
* 15/09/2015 - Eashwar - ITSS
*            - This jshell program is used to check all the job lists in the system & it will clear the job list that are not used by the batch jobs
*-----------------------------------------------------------------------------

    $INSERT I_F.BATCH

    OPEN 'F.LOCKING' TO F.LOCKING ELSE STOP 201

    OPEN 'VOC' TO F.VOC ELSE STOP 201

    F.BATCH = ''
    OPEN 'F.BATCH' TO F.BATCH ELSE STOP 201

    SEL.CMD = 'SELECT VOC LIKE F.JOB.LIST...'
    EXECUTE SEL.CMD CAPTURING OUTPUT
    READLIST SEL.LIST ELSE SEL.LIST = ''

    LOOP
        REMOVE LOCKING.ID FROM SEL.LIST SETTING POS
    WHILE LOCKING.ID:POS
        READ LOCKING.REC FROM F.LOCKING,LOCKING.ID THEN
            OUTPUT = '' ; DEL.FILE = 0
            HUSH ON
            EXECUTE 'COUNT ':LOCKING.ID CAPTURING OUTPUT
            HUSH OFF
            READ LOCKING.REC.JOB FROM F.LOCKING,LOCKING.REC<1> THEN
                IF LOCKING.REC.JOB<1> NE LOCKING.ID THEN
                    CRT LOCKING.ID:' -> contains ':LOCKING.REC<1>:' batch job & it has ':OUTPUT<2>:' whereas ':LOCKING.REC<1>:' batch job is processing under another job list now ':LOCKING.REC<1>
                    DEL.FILE = 1
                END ELSE
                    BATCH.REC = ''; BATCH.ID = ''; BATCH.ID = FIELD(LOCKING.REC<1>,'-',1,1)
					JOB.NAME = ''; JOB.NAME = FIELD(LOCKING.REC<1>,'-',2,1)
                    READ BATCH.REC FROM F.BATCH,BATCH.ID THEN
* Check the job.status & then process status for the batch
                        NO.OF.JOBS = DCOUNT(BATCH.REC<BAT.JOB.STATUS>,@VM)
                        IF NO.OF.JOBS = 0 THEN
* Impossible case
                            CRT LOCKING.ID:' -> contains ':LOCKING.REC<1>:' batch job & it has ':OUTPUT<2>:' whereas no job exists under the batch'
							DEL.FILE = 1
                        END ELSE
                            JOB.PROGRESS = ''
                                LOCATE JOB.NAME IN BATCH.REC<BAT.JOB.NAME,1> SETTING JOB.IDX THEN
								   IF BATCH.REC<BAT.JOB.STATUS,JOB.IDX> NE 0 THEN
                                      JOB.PROGRESS = 1
                                CRT 'JOB.LIST ---->':LOCKING.ID:' is in progress & it has ':OUTPUT<2>:' for ':LOCKING.REC<1>
									  END ELSE
* This is the most case in APAP. Requested to place a Temenos issue on 11th Sep 2015 to APAP pmo team
                                CRT LOCKING.ID:' -> contains ':LOCKING.REC<1>:' batch job & it has ':OUTPUT<2>:' whereas batch itself is marked as complete'
                                DEL.FILE = 1								   
								   END
								END
                        END
                    END ELSE
                        CRT LOCKING.ID:' -> contains ':LOCKING.REC<1>:' batch job & it has ':OUTPUT<2>:' whereas the batch itself does not exist'
                        DEL.FILE = 1
                    END
                END
            END ELSE
                CRT LOCKING.ID:' has ':OUTPUT<2>:' but no additional LOCKING record ':LOCKING.REC<1>
                DEL.FILE = 1
            END

            IF DEL.FILE THEN
                CRT 'Deleting details for JOB.LIST ':LOCKING.ID
                EXECUTE 'CLEAR.FILE ':LOCKING.ID
                DELETE F.LOCKING,LOCKING.ID
                DELETE F.LOCKING,LOCKING.REC<1>
            END
        END ELSE
* Just for one time. All the other records are cleared
            PRINT 'Clearing ':LOCKING.ID
            EXECUTE 'CLEAR.FILE ':LOCKING.ID
        END
    REPEAT
    STOP
END				
