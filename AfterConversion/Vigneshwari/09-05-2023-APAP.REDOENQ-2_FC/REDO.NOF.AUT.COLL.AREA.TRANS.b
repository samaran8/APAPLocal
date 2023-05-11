$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOF.AUT.COLL.AREA.TRANS(Y.FINAL.ARR)

*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :MARIMUTHU S
*Program   Name    :REDO.NOF.AUT.COLL.AREA.TRANS
*---------------------------------------------------------------------------------

*DESCRIPTION       : This is nofile routine used in the enquiry REDO.AUT.COLL.AREA.RECS

* ----------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date               who           Reference            Description
* 10-08-2010        MARIMUTHU S      PACS00094144       Initial Creation
*---------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.REDO.REPAY.NEXT.VER.PROCESS
    $INSERT I_F.REDO.TEMP.VERSION.IDS

MAIN:

    GOSUB OPENFILES
    GOSUB PROCESS
    GOSUB PGM.END

OPENFILES:

    FN.REDO.REPAY.NEXT.VER.PROCESS = 'F.REDO.REPAY.NEXT.VER.PROCESS'
    F.REDO.REPAY.NEXT.VER.PROCESS = ''

    FN.REDO.TEMP.VERSION.IDS = 'F.REDO.TEMP.VERSION.IDS'
    F.REDO.TEMP.VERSION.IDS = ''
    CALL OPF(FN.REDO.TEMP.VERSION.IDS,F.REDO.TEMP.VERSION.IDS)

RETURN

PROCESS:


    CALL CACHE.READ(FN.REDO.REPAY.NEXT.VER.PROCESS,'SYSTEM',R.REDO.REPAY.NEX,RE.ERR)
    Y.VERSIONS = R.REDO.REPAY.NEX<REP.NX.PAYMENT.VERSION>
    LOCATE 'TXN.ID' IN D.FIELDS SETTING POS THEN
        Y.TXN.ID = D.RANGE.AND.VALUE<POS>
        SEL.CMD = 'SELECT ':FN.REDO.TEMP.VERSION.IDS:' WITH TXN.ID EQ ':Y.TXN.ID
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,SEL.ERR)
        IF NO.OF.REC GE 1 THEN
            Y.FINAL.ARR = SEL.LIST:'*':Y.TXN.ID
        END
        RETURN
    END

    Y.CNT = DCOUNT(Y.VERSIONS,@VM)
    FLG = ''

    LOOP
    WHILE Y.CNT GT 0 DO
        FLG += 1
        Y.VER.ID = Y.VERSIONS<1,FLG>
        CALL F.READ(FN.REDO.TEMP.VERSION.IDS,Y.VER.ID,R.REDO.TEMP.VERSION.IDS,F.REDO.TEMP.VERSION.IDS,VER.ERR)
        Y.TXN.IDS = R.REDO.TEMP.VERSION.IDS<REDO.TEM.TXN.ID>
        IF Y.TXN.IDS THEN
            Y.FINAL.ARR<-1> = Y.VER.ID:'*':Y.TXN.IDS
        END
        Y.CNT -= 1
    REPEAT

RETURN

PGM.END:

END
