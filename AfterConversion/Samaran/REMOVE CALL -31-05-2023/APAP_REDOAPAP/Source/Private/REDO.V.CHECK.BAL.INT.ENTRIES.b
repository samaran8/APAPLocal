* @ValidationCode : Mjo0MDIxMjE5NDE6Q3AxMjUyOjE2ODU1MzQ2NDkwMzg6c2FtYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 31 May 2023 17:34:09
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.V.CHECK.BAL.INT.ENTRIES(ACCT.ID,BAL.CR.AMT,BAL.DR.AMT,INT.CR.AMT,INT.DR.AMT,Y.BAL.ACC.NO,Y.INT.ACC.NO)
*---------------------------------------------------------------------------------
*This is call routine to check the balance and interest reclassification for entries
*----------------------------------------------------------------------------------
* Company Name  : ASOCIACION POPUL
* Developed By  : Edwin Charles
* Program Name  : REDO.V.CHECK.BAL.INT.ENTRIES
*
* Routine Name   :REDO.V.CHECK.BAL.INT.ENTRIES
* LINKED WITH:
*----------------------------------------------------------------------
*Input param = none
*output param =none
*-----------------------------------------------------------------------
* MODIFICATION DETAILS:
*DATE          NAME                REFERENCE             DESCRIPTION
*31 JAN 2023   Edwin Charles D     ACCOUNTING-CR           TSR479892
*25-05-2023    CONVERSION TOOL     R22 AUTO CONVERSION     VM TO @VM,FM TO @FM,SM TO @SM,++ TO +=1
*25-05-2023    VICTORIA S          R22 MANUAL CONVERSION   call routine modified
*----------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.T.STATSEQ.BY.ACCT
    $INSERT I_F.REDO.PREVALANCE.STATUS
    $INSERT I_REDO.CRF.NWGL.COMMON
    $INSERT I_F.RE.STAT.REP.LINE
    $INSERT I_F.RE.STAT.LINE.CONT
    $INSERT I_REDO.PREVAL.STATUS.COMMON
    $INSERT I_F.EB.CONTRACT.BALANCES

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS
RETURN
*----------------------------
INIT:

RETURN
*------------------------------
OPENFILE:

    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.T.STATSEQ.BY.ACCT = 'F.REDO.T.STATSEQ.BY.ACCT'
    F.REDO.T.STATSEQ.BY.ACCT = ''
    CALL OPF(FN.REDO.T.STATSEQ.BY.ACCT,F.REDO.T.STATSEQ.BY.ACCT)

    FN.REDO.PREVALANCE.STATUS = 'F.REDO.PREVALANCE.STATUS'
    F.REDO.PREVALANCE.STATUS = ''
    CALL OPF(FN.REDO.PREVALANCE.STATUS, F.REDO.PREVALANCE.STATUS)

    FN.RE.STAT.LINE.CONT = 'F.RE.STAT.LINE.CONT'
    F.RE.STAT.LINE.CONT = ''
    CALL OPF(FN.RE.STAT.LINE.CONT, F.RE.STAT.LINE.CONT)

    FN.EB.CONTRACT.BALANCES = 'F.EB.CONTRACT.BALANCES'
    F.EB.CONTRACT.BALANCES = ''
    CALL OPF(FN.EB.CONTRACT.BALANCES, F.EB.CONTRACT.BALANCES)

    FN.RE.STAT.REP.LINE = 'F.RE.STAT.REP.LINE'
    F.RE.STAT.REP.LINE = ''
    CALL OPF(FN.RE.STAT.REP.LINE, F.RE.STAT.REP.LINE)

    LF.APP = 'RE.STAT.REP.LINE'
    LF.FLD = 'L.BALEM':@VM:'L.BALPG':@VM:'L.BALFA':@VM:'L.ACCEM':@VM:'L.ACCPG':@VM:'L.ACCFA' ;*R22 AUTO CONVERSION
    LF.POS = ''
    CALL MULTI.GET.LOC.REF(LF.APP,LF.FLD,LF.POS)
    L.BALEM.POS = LF.POS<1,1>
    L.BALPG.POS = LF.POS<1,2>
    L.BALFA.POS = LF.POS<1,3>
    L.ACCEM.POS = LF.POS<1,4>
    L.ACCPG.POS = LF.POS<1,5>
    L.ACCFA.POS = LF.POS<1,6>
RETURN

*----------------------------
PROCESS:
*********
*-------Modification ends--------------------------------

    Y.BAL.RECLASSIFY = '' ; Y.INT.RECLASSIFY = '' ; BAL.CR.AMT = '' ; BAL.DR.AMT = ''; INT.CR.AMT = '' ; INT.DR.AMT = ''
    CALL F.READ(FN.ACCOUNT,ACCT.ID,REC.ACCOUNT,F.ACCOUNT,ERR.ACC)
    BALANCE = REC.ACCOUNT<AC.WORKING.BALANCE>
    LOCK.TOT = DCOUNT(REC.ACCOUNT<AC.LOCKED.AMOUNT>,@VM) ;*R22 AUTO CONVERSION
    Y.LOCKED.AMT = REC.ACCOUNT<AC.LOCKED.AMOUNT,LOCK.TOT>
    Y.ACC.INT = REC.ACCOUNT<AC.ACCR.CR.AMOUNT,1>
    Y.AC.CATEG = REC.ACCOUNT<AC.CATEGORY>
    Y.AZ.ACCOUNT = REC.ACCOUNT<AC.ALL.IN.ONE.PRODUCT>
    Y.STATUS = ''
*CALL REDO.CONV.MNEM.TO.STATUS(ACCT.ID,Y.STATUS)
    APAP.REDOAPAP.redoConvMnemToStatus(ACCT.ID,Y.STATUS) ;*R22 MANUAL CONVERSION
    BEGIN CASE
        CASE Y.AZ.ACCOUNT
            Y.CATEG = 'DEP'

        CASE Y.AC.CATEG GE '6000' AND Y.AC.CATEG LE '6999'
            Y.CATEG = 'SAV'

        CASE Y.AC.CATEG GE '1000' AND Y.AC.CATEG LE '1999'
            Y.CATEG = 'ALL'

    END CASE
    FLAG.RECLASSIFY = ''
    IF NOT(RUNNING.UNDER.BATCH) THEN
        GOSUB GET.PREVALANCE.STATUS
    END
    GOSUB FM.COUNTER.CHECK
    GOSUB GET.ACCOUNTING.ACCOUNT

    IF Y.BAL.RECLASSIFY THEN
* Get balances and splitted balances for entries
        IF Y.BAL.RECLASSIFY[1,3] EQ 'LCK' THEN
            BAL.DR.AMT = Y.LOCKED.AMT
            BAL.CR.AMT = BALANCE - Y.LOCKED.AMT
            FLAG.RECLASSIFY = '1'
            IF Y.INT.RECLASSIFY[1,3] EQ 'PER' AND BALANCE THEN
                Y.PERC = Y.LOCKED.AMT / BALANCE
                INT.DR.AMT = Y.ACC.INT * Y.PERC
                INT.CR.AMT = Y.ACC.INT - INT.DR.AMT
                INT.CR.AMT = FMT(INT.CR.AMT,"R2#10")
                INT.DR.AMT = FMT(INT.DR.AMT,"R2#10")
            END
        END
    END ELSE
        IF ACCT.STATUS MATCHES 'AC':@VM:'IL':@VM:'IM':@VM:'LE':@VM:'ME':@VM:'FA':@VM:'PG' THEN ;*R22 AUTO CONVERSION
            IF BALANCE GT '0' THEN
                BAL.CR.AMT = BALANCE
                INT.CR.AMT = Y.ACC.INT
            END ELSE
                BAL.DR.AMT = BALANCE
                INT.DR.AMT = Y.ACC.INT
            END

        END
    END
* Get accounting account for entries
    IF BAL.CR.AMT OR INT.CR.AMT OR BAL.DR.AMT OR INT.DR.AMT THEN
        IF FLAG.RECLASSIFY THEN
            Y.BAL.ACC.NO = R.STAT.REP.LINE.CAP<RE.SRL.LOCAL.REF,L.BALEM.POS>:'*':Y.NAME
            Y.INT.ACC.NO = R.STAT.REP.LINE.INT<RE.SRL.LOCAL.REF,L.ACCEM.POS>:'*':Y.NAME
        END ELSE
            BEGIN CASE

                CASE ACCT.STATUS MATCHES 'AC':@VM:'IL':@VM:'IM':@VM:'LE':@VM:'ME' ;*R22 AUTO CONVERSION
                    Y.BAL.ACC.NO = R.STAT.REP.LINE.CAP<RE.SRL.LOCAL.REF,L.BALEM.POS>:'*':Y.NAME
                    Y.INT.ACC.NO = R.STAT.REP.LINE.INT<RE.SRL.LOCAL.REF,L.ACCEM.POS>:'*':Y.NAME

                CASE ACCT.STATUS EQ 'FA'
                    Y.BAL.ACC.NO = R.STAT.REP.LINE.CAP<RE.SRL.LOCAL.REF,L.BALFA.POS>:'*':Y.NAME
                    Y.INT.ACC.NO = R.STAT.REP.LINE.INT<RE.SRL.LOCAL.REF,L.ACCFA.POS>:'*':Y.NAME

                CASE ACCT.STATUS EQ 'PG'
                    Y.BAL.ACC.NO = R.STAT.REP.LINE.CAP<RE.SRL.LOCAL.REF,L.BALPG.POS>:'*':Y.NAME
                    Y.INT.ACC.NO = R.STAT.REP.LINE.INT<RE.SRL.LOCAL.REF,L.ACCPG.POS>:'*':Y.NAME
            END CASE
        END

    END


RETURN
*--------------------------------------------------------------------------------------------
FM.COUNTER.CHECK:
*--------------------------------------------------------------------------------------------
* 20170327 /S TUS
    Y.FINAL.STATUS = '' ; AC.FLAG = '' ; CNT.AC = 1 ; Y.AC.TYPE = ''

    TOT.AC.CNT = DCOUNT(ACCT.TYPE.LIST,@FM) ;*R22 AUTO CONVERSION
    LOOP
    WHILE CNT.AC LE TOT.AC.CNT
        Y.AC.TYPE = ACCT.TYPE.LIST<CNT.AC>
        BEGIN CASE
            CASE Y.AC.TYPE EQ Y.CATEG
                GOSUB STATUS.CHECK
            CASE Y.AC.TYPE EQ 'ALL'
                GOSUB STATUS.CHECK
        END CASE

        CNT.AC += 1 ;*R22 AUTO CONVERSION
    REPEAT

STATUS.CHECK:
*************
    IF Y.STATUS EQ PARAM.STATUS<CNT.AC> THEN
        Y.FINAL.STATUS = PREVALANCE.STATUS<CNT.AC>
        Y.BAL.RECLASSIFY = Y.BAL.RECLASSIFY.LIST<CNT.AC>
        Y.INT.RECLASSIFY = Y.INT.RECLASSIFY.LIST<CNT.AC>
        CNT.AC = TOT.AC.CNT
    END
RETURN


GET.ACCOUNTING.ACCOUNT:
***********************
    R.EB.CONTRACT.BALANCES = '' ; EB.CONT.ERROR = '' ; ACCT.STATUS = ''
    BAL.CR.AMT = ''; BAL.DR.AMT = '' ; INT.CR.AMT = '' ; INT.DR.AMT = ''
    CALL F.READ(FN.EB.CONTRACT.BALANCES,ACCT.ID,R.EB.CONTRACT.BALANCES,F.EB.CONTRACT.BALANCES,EB.CONT.ERROR)
    IF R.EB.CONTRACT.BALANCES THEN
        CONSOL.KEY.TEMP = R.EB.CONTRACT.BALANCES<ECB.CONSOL.KEY>
        ACCT.STATUS = FIELD(CONSOL.KEY.TEMP,".",11)

        Y.SAP.TXN.COMP = FIELD(CONSOL.KEY.TEMP,".",17)
        Y.RE.STAT.LINE.BAL.CUR = FIELD(CONSOL.KEY.TEMP,".",4)
        RE.STAT.REP.ID = ''
*        SEL.CMD  = "SELECT ":FN.RE.STAT.LINE.CONT:" WITH ASST.CONSOL.KEY EQ ":CONSOL.KEY.TEMP
*        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
*        RE.STAT.REP.ID = SEL.LIST<1>
    END
    RE.STAT.REP.ID = RE.STAT.REP.ID[1,9]
    RE.STAT.REP.ID = 'NWGL.5386' ; Y.NAME = '' ; R.STAT.REP.LINE.INT = ''
    CALL F.READ(FN.RE.STAT.REP.LINE,RE.STAT.REP.ID,R.STAT.REP.LINE.INT,F.RE.STAT.REP.LINE,STAT.REP.LINE.ERR)
    Y.SAP.ACC.NO = R.STAT.REP.LINE.INT<RE.SRL.DESC,3,1>
    Y.SIB.ACC.NO = R.STAT.REP.LINE.INT<RE.SRL.DESC,1,1>
    Y.NAME = R.STAT.REP.LINE.INT<RE.SRL.DESC,2,1>
    Y.INT.ACC.NO = Y.SAP.ACC.NO:'*':Y.NAME

    RE.STAT.REP.ID = 'NWGL.3367' ; Y.NAME = '' ; R.STAT.REP.LINE.CAP = ''
    CALL F.READ(FN.RE.STAT.REP.LINE,RE.STAT.REP.ID,R.STAT.REP.LINE.CAP,F.RE.STAT.REP.LINE,STAT.REP.LINE.ERR)
    Y.SAP.ACC.NO = R.STAT.REP.LINE.CAP<RE.SRL.DESC,3,1>
    Y.SIB.ACC.NO = R.STAT.REP.LINE.CAP<RE.SRL.DESC,1,1>
    Y.NAME = R.STAT.REP.LINE.CAP<RE.SRL.DESC,2,1>
    Y.BAL.ACC.NO = Y.SAP.ACC.NO:'*':Y.NAME
RETURN
GET.PREVALANCE.STATUS:
**********************
    PARAM.STATUS = ''; PREVALANCE.STATUS = ''; STAT.FM.CNTR = 0 ; ACCT.TYPE.LIST = ''
    FN.REDO.PREVALANCE.STATUS = 'F.REDO.PREVALANCE.STATUS'
    F.REDO.PREVALANCE.STATUS = ''
    CALL OPF(FN.REDO.PREVALANCE.STATUS,F.REDO.PREVALANCE.STATUS)

    CALL CACHE.READ(FN.REDO.PREVALANCE.STATUS,'SYSTEM',R.REDO.PREVALANCE.STATUS,F.ERR)

    PARAM.STATUS.VAL = R.REDO.PREVALANCE.STATUS<REDO.PRE.STATUS>
    PREVALANCE.STATUS.VAL = CHANGE(R.REDO.PREVALANCE.STATUS<REDO.PRE.PREVALANT.STATUS>,@VM,@FM) ;*R22 AUTO CONVERSION START
    ACCT.TYPE.VAL = CHANGE(R.REDO.PREVALANCE.STATUS<REDO.PRE.ACCT.TYPE>,@VM,@FM)
    Y.BAL.RECLASSIFY.VAL = CHANGE(R.REDO.PREVALANCE.STATUS<REDO.PRE.BAL.RECLASS>,@VM,@FM)
    Y.INT.RECLASSIFY.VAL = CHANGE(R.REDO.PREVALANCE.STATUS<REDO.PRE.INT.RECLASS>,@VM,@FM) ;*R22 AUTO CONVERSION END

    STAT.FM.CNTR = DCOUNT(PARAM.STATUS.VAL,@VM) ;*R22 AUTO CONVERSION
    LOOP.FM.CNTR = 1

    LOOP
    WHILE LOOP.FM.CNTR LE STAT.FM.CNTR
        Y.FM.STATUS = PARAM.STATUS.VAL<1,LOOP.FM.CNTR>
        Y.FM.STATUS = CHANGE(Y.FM.STATUS,@FM,':') ;*R22 AUTO CONVERSION
        Y.FM.STATUS = CHANGE(Y.FM.STATUS,@SM,':') ;*R22 AUTO CONVERSION
        PARAM.STATUS<-1> = Y.FM.STATUS
        PREVALANCE.STATUS<-1> = PREVALANCE.STATUS.VAL<LOOP.FM.CNTR>
        ACCT.TYPE.LIST<-1> = ACCT.TYPE.VAL<LOOP.FM.CNTR>
        Y.BAL.RECLASSIFY.LIST<-1> = Y.BAL.RECLASSIFY.VAL<LOOP.FM.CNTR>
        Y.INT.RECLASSIFY.LIST<-1> = Y.INT.RECLASSIFY.VAL<LOOP.FM.CNTR>
        LOOP.FM.CNTR + = 1
    REPEAT
RETURN
END
