*-----------------------------------------------------------------------------
* <Rating>99</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.APAP.CUST.SWIFT.UPD

* One time routine update the REDO.APAP.CUST.SWIFT.DET table
* Ashokkumar
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT LAPAP.BP I_F.REDO.APAP.CUST.SWIFT.DET

    FN.REDO.APAP.CUST.SWIFT.DET = 'F.REDO.APAP.CUST.SWIFT.DET'; F.REDO.APAP.CUST.SWIFT.DET = ''
    CALL OPF(FN.REDO.APAP.CUST.SWIFT.DET,F.REDO.APAP.CUST.SWIFT.DET)
    FN.SAVELST = '&SAVEDLISTS&'; F.SAVELST = ''
    CALL OPF(FN.SAVELST,F.SAVELST)

    READ R.SAVELST FROM F.SAVELST,'SWIFT.CUST' ELSE RETURN

    LOOP
        REMOVE SAVELST.ID FROM R.SAVELST SETTING SL.POSN
    WHILE SAVELST.ID:SL.POSN
        YSWIFT = ''; YDESCRIPT = ''; YCUSTOMR = ''
        YSWIFT = FIELD(SAVELST.ID,'|',1)
        YDESCRIPT = FIELD(SAVELST.ID,'|',2)
        YCUSTOMR = FIELD(SAVELST.ID,'|',3)

        ERR.REDO.APAP.CUST.SWIFT.DET = ''; R.REDO.APAP.CUST.SWIFT.DET = ''
        CALL F.READ(FN.REDO.APAP.CUST.SWIFT.DET,YSWIFT,R.REDO.APAP.CUST.SWIFT.DET,F.REDO.APAP.CUST.SWIFT.DET,ERR.REDO.APAP.CUST.SWIFT.DET)
        R.REDO.APAP.CUST.SWIFT.DET<REDO.CUSW.DESCRIPTION> = YDESCRIPT
        R.REDO.APAP.CUST.SWIFT.DET<REDO.CUSW.CUSTOMER.ID> = YCUSTOMR

        OFS.SOURCE.ID = 'OFS.LOAD'
        APPLICATION.NAME = 'REDO.APAP.CUST.SWIFT.DET'
        TRANS.FUNC.VAL = 'I'
        TRANS.OPER.VAL = 'PROCESS'
        APPLICATION.NAME.VERSION = 'REDO.APAP.CUST.SWIFT.DET,INP'
        NO.AUT = '0'
        OFS.MSG.ID = ''
        APPLICATION.ID = YSWIFT
        OFS.POST.MSG = ''; TEMP.REST = ''
        CALL LOAD.COMPANY(YCO.CODE)
        CALL OFS.BUILD.RECORD(APPLICATION.NAME,TRANS.FUNC.VAL,TRANS.OPER.VAL,APPLICATION.NAME.VERSION,"",NO.AUT,APPLICATION.ID,R.REDO.APAP.CUST.SWIFT.DET,OFS.COLLRGT)
        CRT OFS.COLLRGT
        CALL OFS.GLOBUS.MANAGER('OFS.LOAD',OFS.COLLRGT)
        CRT OFS.COLLRGT
        CALL JOURNAL.UPDATE('')
    REPEAT
    PRINT "Process Completed"
    RETURN
END
