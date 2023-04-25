*-----------------------------------------------------------------------------
* <Rating>198</Rating>
*-----------------------------------------------------------------------------
* Realializado por Elvis, Lo siguiente obtendra la fecha al dia.
    PROGRAM COPY.REPORTS
    $INSERT T24.BP I_F.DATES

    FV.DATES = ''
    OPEN 'F.DATES' TO FV.DATES ELSE STOP 201

    TODAY.REC = ''
    READ TODAY.REC FROM FV.DATES,'DO0010001' THEN
        TODAY.VAR = TODAY.REC<EB.DAT.TODAY>
*        TODAY.VAR = TODAY.REC<EB.DAT.LAST.WORKING.DAY>
*        SEL.CMD = 'SELECT F.HOLD.CONTROL WITH REPORT.NAME EQ CRB.MBGL AND DATE.CREATED EQ ':TODAY.VAR
        SEL.CMD = 'SELECT F.HOLD.CONTROL WITH REPORT.NAME EQ CRB.MBGL AND BANK.DATE EQ ':TODAY.VAR
        EXECUTE SEL.CMD CAPTURING OUTPUT
        READLIST REC.LIST ELSE REC.LIST = ''

        FOR REC.IDX = 1 TO DCOUNT(REC.LIST,@FM)
            PRINT 'Copying ':REC.LIST<REC.IDX>
            EXECUTE 'COPY FROM &HOLD& TO ../bnk.interface/REG.REPORTS/CRBs ':REC.LIST<REC.IDX>
        NEXT REC.IDX
    END

    RETURN

END
