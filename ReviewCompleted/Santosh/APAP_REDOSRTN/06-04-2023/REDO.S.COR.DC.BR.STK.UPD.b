* @ValidationCode : Mjo1NzEzNjQ5MzpDcDEyNTI6MTY4MDc3ODMyMzQ5Mjo5MTYzODotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 06 Apr 2023 16:22:03
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 91638
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOSRTN
SUBROUTINE REDO.S.COR.DC.BR.STK.UPD
***********************************************************
*------------------------------------------------------------

*    IN PARAMETER: NONE
*    OUT PARAMETER: NONE

*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO                    REFERENCE         DESCRIPTION
*06 JUL 2015      Prabhu                 Correction DC stok in the Branch
*----------------------------------------------------------------------
*Modification history
*Date                Who               Reference                  Description
*06-04-2023      conversion tool     R22 Auto code conversion     FM TO @FM, VM TO @VM,++ TO +=1,Y.REQ.COUNT+1 TO +=1
*06-04-2023      Mohanraj R          R22 Manual code conversion   No changes



*-------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.STOCK.ENTRY
    $INSERT I_F.COMPANY
    $INSERT I_F.REDO.CARD.REQUEST
    $INSERT I_F.REDO.CARD.SERIES.PARAM
    $INSERT I_F.LOCKING



    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

*-------------------------------------------------------------
INIT:
*Initialising
*-------------------------------------------------------------

    R.REDO.STOCK.ENTRY = ''



RETURN

*-------------------------------------------------------------
OPENFILES:
*Opening File

    FN.LOCKING='F.LOCKING'
    F.LOCKING = ''
    CALL OPF(FN.LOCKING,F.LOCKING)

    FN.REDO.STOCK.ENTRY = 'F.REDO.STOCK.ENTRY'
    F.REDO.STOCK.ENTRY = ''
    CALL OPF(FN.REDO.STOCK.ENTRY,F.REDO.STOCK.ENTRY)

    FN.REDO.CARD.REQ = 'F.REDO.CARD.REQUEST'
    F.REDO.CARD.REQ = ''
    CALL OPF(FN.REDO.CARD.REQ,F.REDO.CARD.REQ)

    FN.REDO.SER.PARAM = 'F.REDO.CARD.SERIES.PARAM'
    F.REDO.SER.PARAM = ''
    CALL OPF(FN.REDO.SER.PARAM,F.REDO.SER.PARAM)

RETURN
*-------------------------------------------------------------
PROCESS:

    Y.REDO.CARD.REQ.LIST    ='REQ.20150526003.0002'
    Y.REDO.CARD.REQ.LIST<-1>='REQ.20150519020.0020'
    Y.REDO.CARD.REQ.LIST<-1>='REQ.20150522001.0020'

    Y.TOT.REQ=DCOUNT(Y.REDO.CARD.REQ.LIST,@FM)
    Y.REQ.CNT=1
    LOOP
    WHILE Y.REQ.CNT LE Y.TOT.REQ
        Y.CUR.REQ.ID=Y.REDO.CARD.REQ.LIST<Y.REQ.CNT>
        CALL F.READ(FN.REDO.CARD.REQ,Y.CUR.REQ.ID,R.REDO.CARD.REQ,F.REDO.CARD.REQ,Y.ERR)
        Y.BR.REC.ST.ID=''
        Y.BR.REC.ST.ID=R.REDO.CARD.REQ<REDO.CARD.REQ.BRANCH.SE.ID>
        IF NOT(Y.BR.REC.ST.ID) THEN
            GOSUB SUB.PROCESS.REQ
        END

        Y.REQ.CNT += 1
    REPEAT
RETURN
****************
SUB.PROCESS.REQ:
****************
    CALL CACHE.READ(FN.REDO.SER.PARAM,'SYSTEM',R.REDO.SER.PARAM,PROD.ERR)

    IF R.REDO.SER.PARAM THEN
        EMBOSS.DEPT.CODE = R.REDO.SER.PARAM<REDO.CARD.SERIES.PARAM.EMBOSS.DEPT.CODE>
        RECEIVE.DEPT.CODE = R.REDO.SER.PARAM<REDO.CARD.SERIES.PARAM.RECEIVE.DEPT.CODE>
        VIRGIN.DEPT.CODE = R.REDO.SER.PARAM<REDO.CARD.SERIES.PARAM.VIRGIN.DEPT.CODE>
        TRANSIT.DEPT.CODE = R.REDO.SER.PARAM<REDO.CARD.SERIES.PARAM.TRANSIT.DEPT.CODE>
    END



    FINANCIAL.COMP = R.COMPANY(EB.COM.FINANCIAL.COM)

    Y.AGENCY = R.REDO.CARD.REQ<REDO.CARD.REQ.AGENCY>


    GOSUB COMMON.ARRAY

    R.REDO.STOCK.ENTRY<STK.FROM.REGISTER>= 'CARD.':FINANCIAL.COMP:'-':TRANSIT.DEPT.CODE
    R.REDO.STOCK.ENTRY<STK.TO.REGISTER>= 'CARD.':Y.AGENCY:'-':RECEIVE.DEPT.CODE
    R.REDO.STOCK.ENTRY<STK.NOTES> = "DELIVERY TO BRANCH"
    GOSUB CALL.OFS.PROCESS

RETURN

*--------------
COMMON.ARRAY:

    R.REDO.STOCK.ENTRY<STK.IN.OUT.DATE> = TODAY
    R.REDO.STOCK.ENTRY<STK.BATCH.NO> = Y.CUR.REQ.ID
    Y.REQ.TOT.COUNT = DCOUNT(R.REDO.CARD.REQ<REDO.CARD.REQ.REGOFF.ACCEPTQTY>,@VM)
    Y.REQ.COUNT = 1
    LOOP
    WHILE Y.REQ.COUNT LE Y.REQ.TOT.COUNT
        R.REDO.STOCK.ENTRY<STK.STOCK.QUANTITY,Y.REQ.COUNT> = R.REDO.CARD.REQ<REDO.CARD.REQ.REGOFF.ACCEPTQTY><1,Y.REQ.COUNT>

        Y.NEW.STK.SERIES = R.REDO.CARD.REQ<REDO.CARD.REQ.CARD.TYPE><1,Y.REQ.COUNT>

        R.REDO.STOCK.ENTRY<STK.STOCK.SERIES,Y.REQ.COUNT> = Y.NEW.STK.SERIES
        Y.REQ.COUNT += 1 ;*R22 Auto code conversion
    REPEAT


RETURN
*-------------------
CALL.OFS.PROCESS:



    Y.NEW.ID = ''

    Y.LOCKING.ID = 'F.REDO.STOCK.ENTRY'

    R.LOCKING   = ''
    LOCKING.ERR = ''
    CALL F.READU(FN.LOCKING,Y.LOCKING.ID,R.LOCKING,F.LOCKING,LOCKING.ERR,'P')
    IF R.LOCKING THEN
        Y.NEW.ID = R.LOCKING<EB.LOK.CONTENT>
        FETCH.NEXT.ID = Y.NEW.ID[3,10]
        FETCH.NEXT.ID += 1
        Y.NEW.ID = 'SE':FETCH.NEXT.ID
        R.LOCKING<EB.LOK.CONTENT> = Y.NEW.ID
    END

    CALL F.WRITE(FN.LOCKING,Y.LOCKING.ID,R.LOCKING)

    APP.NAME = 'REDO.STOCK.ENTRY'
    OFSFUNCT = 'I'
    PROCESS  = 'PROCESS'
    OFSVERSION = 'REDO.STOCK.ENTRY,REDO.CARD.MV'
    GTSMODE = ''
    NO.OF.AUTH = '0'
    TRANSACTION.ID = Y.NEW.ID
    OFSRECORD = ''
    OFS.MSG.ID =''
    OFS.SOURCE.ID = 'DEBIT.CARD'
    OFS.ERR = ''
    CALL OFS.BUILD.RECORD(APP.NAME,OFSFUNCT,PROCESS,OFSVERSION,GTSMODE,NO.OF.AUTH,TRANSACTION.ID,R.REDO.STOCK.ENTRY,OFSRECORD)
    CALL OFS.POST.MESSAGE(OFSRECORD,OFS.MSG.ID,OFS.SOURCE.ID,OFS.ERR)
    CALL JOURNAL.UPDATE('')

RETURN
*-----------------------

END
