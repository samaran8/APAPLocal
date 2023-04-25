*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    PROGRAM L.APAP.COUNT.NCF
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    CRT @(-1)
    CRT "TOTAL DE NCF:"
    *MSLEEP 400
    FN.NCF.IDS = 'F.REDO.AA.NCF.IDS'; F.NCF.IDS = ''
    CALL OPF(FN.NCF.IDS,F.NCF.IDS)
    R.FILE.DATA = ''
    Y.SUMATORIA = 0
    SEL.CMD = "SELECT ":FN.NCF.IDS : " WITH @ID NE OTHERS.500 AND @ID LIKE OTHERS... BY-DSND @ID"
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    LOOP
        REMOVE Y.TEMP.ID FROM SEL.LIST SETTING TEMP.POS
    WHILE Y.TEMP.ID
        R.REDO.REPORT.TEMP = '';        TEMP.ERR = ''
        CALL F.READ(FN.NCF.IDS,Y.TEMP.ID,R.NCF.IDS,F.NCF.IDS,TEMP.ERR)
        IF R.NCF.IDS THEN
            R.FILE.DATA<-1> = R.NCF.IDS
            CNT.ACTUAL = DCOUNT(R.NCF.IDS,FM)
            CRT "ID: " : Y.TEMP.ID : ", QUANTITY: " : CNT.ACTUAL
            Y.SUMATORIA += CNT.ACTUAL
*DEBUG
        END
    REPEAT
    CRT "---------------------------------------"
    CRT "SUMATORIA DE CANTIDADES DE NCFS : " : Y.SUMATORIA
    CRT "---------------------------------------"

END
