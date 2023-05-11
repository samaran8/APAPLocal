*
*************************************************************************
*
SUBROUTINE REDO.E.BUILD.NOS.BAL
*
* Descrption: This routine is attached to the Enquiry REDO.NOSTRO.POSITION to get
*              the NOSTRO account future values.
* Dev by: Ashokkumar
*************************************************************************
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CURRENCY
    $INSERT I_F.ENQUIRY
*
*************************************************************************
*

    GOSUB INIT
    GOSUB PROCESS
RETURN

INIT:
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ""
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)
    LOC.FLAG = ''
    IF O.DATA[6] EQ '*LOCAL' THEN
        LENGTH.OF.O.DATA = LEN(O.DATA)
        O.DATA = O.DATA[1,(LENGTH.OF.O.DATA - 6)]
        LOC.FLAG = 1
        LCCY.CODE = LCCY[1,2]
    END
*
    MULT.SIGN = 1
    LOCATE "LONG.POS.SIGN" IN ENQ.SELECTION<2,1> SETTING LP.POS THEN
        IF ENQ.SELECTION<4,LP.POS> EQ "PLUS" THEN
            MULT.SIGN = -1
        END
    END
*
    CONV.NCU = ''
    LOCATE "MERGE.NCU" IN ENQ.SELECTION<2,1> SETTING EU.POS THEN
        IF ENQ.SELECTION<4,EU.POS>[1,1] EQ 'Y' THEN
            CONV.NCU = 1
        END
    END
RETURN

PROCESS:
********
*
    IF R.ENQ<ENQ.FILE.NAME>['$',1,1] NE 'ACCOUNT' THEN
        R.RECORD = ''; ERR.ACCT = ''
        CALL F.READ(FN.ACCOUNT,O.DATA,R.RECORD,F.ACCOUNT,ERR.ACCT)
    END
*
    C.DATE = TODAY
    C.BAL = R.RECORD<AC.WORKING.BALANCE>
    V.DATE = R.RECORD<AC.AVAILABLE.DATE>
    D.MOV = R.RECORD<AC.AV.AUTH.DB.MVMT>
    C.MOV = R.RECORD<AC.AV.AUTH.CR.MVMT>
    V.BAL = R.RECORD<AC.AVAILABLE.BAL>
    CCY = R.RECORD<AC.CURRENCY>
    READ CCY.REC FROM F.CURRENCY, CCY ELSE CCY.REC = ''
    FIX.CCY = CCY.REC<EB.CUR.FIXED.CCY>
    FIX.RATE = CCY.REC<EB.CUR.FIXED.RATE>
    COUNTRY.CODE = R.RECORD<AC.CURRENCY>[1,2]
    IF LOC.FLAG THEN
        REGION.CODE = LCCY.CODE:'00'
    END ELSE
        REGION.CODE = COUNTRY.CODE:'00'

        TEMP.DATE = C.DATE
        CALL CDT(REGION.CODE,TEMP.DATE,'+1W')
        NEXT.DATE = TEMP.DATE
        CALL CDT(REGION.CODE,TEMP.DATE,'-1W')
        IF C.DATE NE TEMP.DATE THEN
            C.DATE = NEXT.DATE
        END

    END
    O.DATA = ''

    IF NOT(C.BAL) THEN
        C.BAL = 0
    END
    IF FIX.CCY AND CONV.NCU THEN
        GOSUB CONVERT.AMOUNT
    END
    FOR I.VAR = 1 TO 5
        LOCATE C.DATE IN V.DATE<1,1> BY 'AL' SETTING POS THEN
            C.BAL = V.BAL<1,POS>
            IF FIX.CCY AND CONV.NCU THEN
                GOSUB CONVERT.AMOUNT
            END
        END ELSE
            GOSUB SUB.PROCESS
        END
        O.DATA<1,1,I.VAR> = C.DATE
        O.DATA<1,2,I.VAR> = C.BAL * MULT.SIGN
        CALL CDT(REGION.CODE,C.DATE,'+1W')
    NEXT I.VAR
RETURN

SUB.PROCESS:
*************
    BEGIN CASE
        CASE V.DATE EQ ''
            NULL
        CASE V.DATE<1,POS>
            C.BAL = V.BAL<1,POS> - D.MOV<1,POS> - C.MOV<1,POS>
            IF FIX.CCY AND CONV.NCU THEN
                GOSUB CONVERT.AMOUNT
            END
        CASE OTHERWISE
            C.BAL = V.BAL<1,POS-1>
            IF FIX.CCY AND CONV.NCU THEN
                GOSUB CONVERT.AMOUNT
            END
    END CASE
RETURN

***************************************************************************
CONVERT.AMOUNT:
*==============
*
    OUT.AMT = ''
    CALL EXCHRATE('1', CCY, C.BAL, FIX.CCY, OUT.AMT, '', FIX.RATE, '', '', '')
    C.BAL = OUT.AMT
RETURN
*
****************************************************************************
END
