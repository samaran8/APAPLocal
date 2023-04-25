*-----------------------------------------------------------------------------
* <Rating>199</Rating>
*-----------------------------------------------------------------------------
    PROGRAM COPY.REPORTS.MISMATCH

    $INCLUDE T24.BP I_F.DATES

    FV.DATES = ''
    OPEN 'F.DATES' TO FV.DATES ELSE STOP 201

    TODAY.REC = ''
    READ TODAY.REC FROM FV.DATES,'DO0010001' THEN
        TODAY.VAR = TODAY.REC<EB.DAT.TODAY>

        SEL.CMD = 'SELECT F.HOLD.CONTROL WITH REPORT.NAME EQ CRD.MBGL CRD.MBPL AND BANK.DATE EQ ':TODAY.VAR
*        SEL.CMD = 'SELECT F.HOLD.CONTROL WITH REPORT.NAME EQ CRD.MBGL CRD.MBPL'
        EXECUTE SEL.CMD CAPTURING OUTPUT
        READLIST REC.LIST ELSE REC.LIST = ''

        FOR REC.IDX = 1 TO DCOUNT(REC.LIST,@FM)
            PRINT 'Copying ':REC.LIST<REC.IDX>
            EXECUTE 'COPY FROM &HOLD& TO ../bnk.interface/REG.REPORTS/CRBs/CRD ':REC.LIST<REC.IDX>
        NEXT REC.IDX
    END

    RETURN

END
