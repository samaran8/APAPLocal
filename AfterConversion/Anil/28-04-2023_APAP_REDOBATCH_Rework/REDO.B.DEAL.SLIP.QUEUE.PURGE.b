* @ValidationCode : MjotMTQ4NTg1Mzg0NDpDcDEyNTI6MTY4MjY1ODIyMDQ3NTpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 28 Apr 2023 10:33:40
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
*-----------------------------------------------------------------------------
* <Rating>-78</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.B.DEAL.SLIP.QUEUE.PURGE(Y.PURGE.ID)
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : TEMENOS DEVELOPMENT
* Program Name  : REDO.B.DEAL.SLIP.QUEUE.PURGE
* ODR           :
*------------------------------------------------------------------------------------------
*DESCRIPTION  : REDO.B.TRANS.PROCESS Multithreading routine responsible for generates
*FUNDS.TRANSFER Record
*------------------------------------------------------------------------------------------
* Linked with:
* In parameter : None
* out parameter : None
*------------------------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------------------------
***********************************************************************
*DATE                WHO                   REFERENCE         DESCRIPTION
*24-12-2010        C.SRIRAMAN              ODR-2011-01-0103    INITIAL CREATION
* Date                  who                   Reference              
* 28-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION - No Change
* 28-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -VM TO @VM AND SM TO @SM AND FM TO @FM
****************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_REDO.B.DEAL.SLIP.QUEUE.PURGE.COMMON
    $INSERT I_F.REDO.APAP.H.DEAL.SLIP.QUEUE
    $INSERT I_F.REDO.APAP.H.DEAL.SLIP.QUEUE.PARAM
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.DATES
    $INSERT I_F.USER


    GOSUB INIT
    GOSUB PROCESS

RETURN

*****
INIT:
*****

    Y.DS.QUEUE.LIST = '' ; Y.DS.QUEUE.TOT.CNT = '';Y.TYPE.TRANS.VAL = '' ; Y.FUNDS.TRANS.DATE.TIME = '';
    Y.DS.QUEUE.TRANSACTION.TIME = ''; Y.REQ.COMPANY.CODE = ''; Y.DS.QUEUE.PARAM.ID ='';
    Y.DS.QUEUE.PARAM.RETENTION.PERIOD = ''; Y.DATES.ID.VAL = ''; Y.CO.LAST.WORK.DAY ='';
    Y.CUR.VALUE = ''; Y.PREV.VALUE = '' ; NOF.DAYS = ''

RETURN
*------------------------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------------------------

    Y.DS.QUEUE.ID = Y.PURGE.ID

    Y.DS.QUEUE.ERR = ''
    CALL F.READ(FN.REDO.APAP.H.DEAL.SLIP.QUEUE,Y.DS.QUEUE.ID,R.REDO.APAP.H.DEAL.SLIP.QUEUE,F.REDO.APAP.H.DEAL.SLIP.QUEUE,Y.DS.QUEUE.ERR)
    IF NOT(Y.DS.QUEUE.ERR) THEN
        Y.TYPE.TRANS.VAL = Y.DS.QUEUE.ID[1,2]
        IF ALPHA(Y.TYPE.TRANS.VAL) THEN
            GOSUB CHECK.FUND.TELLER
        END ELSE
            GOSUB CHECK.T24.SERVICES
        END
    END

    GOSUB DEAL.SLIP.QUEUE.PARAM

RETURN
******************
CHECK.FUND.TELLER:
******************

    IF Y.TYPE.TRANS.VAL EQ 'FT' THEN
        Y.FUNDS.TRANS.ERR = ''
        CALL F.READ(FN.FUNDS.TRANSFER,Y.DS.QUEUE.ID,R.FUNDS.TRANSFER.REC,F.FUNDS.TRANSFER,Y.FUNDS.TRANS.ERR)
        IF NOT(Y.FUNDS.TRANS.ERR) THEN
            Y.FUNDS.TRANS.DATE.TIME = R.FUNDS.TRANSFER.REC<FT.DEBIT.VALUE.DATE>
*            Y.DS.QUEUE.TRANSACTION.TIME = '20':Y.FUNDS.TRANS.DATE.TIME[1,6]
            Y.DS.QUEUE.TRANSACTION.TIME = Y.FUNDS.TRANS.DATE.TIME
            Y.REQ.COMPANY.CODE = R.FUNDS.TRANSFER.REC<FT.CO.CODE>
        END ELSE

            CALL EB.READ.HISTORY.REC(F.FUNDS.TRANSFER.HIS,Y.DS.QUEUE.ID,R.FT.HIS.REC,FT.ERR)
            IF R.FT.HIS.REC THEN
                Y.FUNDS.TRANS.DATE.TIME = R.FT.HIS.REC<FT.DEBIT.VALUE.DATE>
*                Y.DS.QUEUE.TRANSACTION.TIME = '20':Y.FUNDS.TRANS.DATE.TIME[1,6]
                Y.DS.QUEUE.TRANSACTION.TIME = Y.FUNDS.TRANS.DATE.TIME

                Y.REQ.COMPANY.CODE = R.FT.HIS.REC<FT.CO.CODE>

            END
        END
    END
    IF Y.TYPE.TRANS.VAL EQ 'TT' THEN
        Y.TELLER.REC.ERR = ''
        CALL F.READ(FN.TELLER,Y.DS.QUEUE.ID,R.TELLER.REC,F.TELLER,Y.TELLER.REC.ERR)
        IF NOT(Y.TELLER.REC.ERR) THEN
            Y.TELLER.TRANS.DATE.TIME = R.TELLER.REC<TT.TE.VALUE.DATE.1>
*            Y.DS.QUEUE.TRANSACTION.TIME = '20':Y.TELLER.TRANS.DATE.TIME[1,6]
            Y.DS.QUEUE.TRANSACTION.TIME = Y.TELLER.TRANS.DATE.TIME

            Y.REQ.COMPANY.CODE = R.TELLER.REC<TT.TE.CO.CODE>

        END ELSE

            CALL EB.READ.HISTORY.REC(F.TELLER.HIS,Y.DS.QUEUE.ID,R.TELLER.HIS.REC,TT.HIS.ERR)
            IF R.TELLER.HIS.REC THEN
                Y.TELLER.TRANS.DATE.TIME = R.TELLER.HIS.REC<TT.TE.VALUE.DATE.1>
*                Y.DS.QUEUE.TRANSACTION.TIME = '20':Y.TELLER.TRANS.DATE.TIME[1,6]
                Y.DS.QUEUE.TRANSACTION.TIME = Y.TELLER.TRANS.DATE.TIME

                Y.REQ.COMPANY.CODE = R.TELLER.HIS.REC<TT.TE.CO.CODE>
            END
        END
    END

RETURN
*******************
CHECK.T24.SERVICES:
*******************
    Y.TYPE.T24FS.TRANS.VAL = Y.DS.QUEUE.ID[1,5]

    IF Y.TYPE.T24FS.TRANS.VAL EQ 'T24FS' THEN
        Y.T24.FUND.SERVICE.ERR = ''

        CALL F.READ(FN.T24.FUND.SERVICES,Y.DS.QUEUE.ID,R.T24.FUND.SERVICES.REC,F.T24.FUND.SERVICES,Y.T24.FUND.SERVICE.ERR)

        IF NOT(Y.T24.FUND.SERVICE.ERR) THEN
            Y.T24FS.TRANS.DATE.TIME = R.T24.FUND.SERVICES.REC<TFS.BOOKING.DATE>
*            Y.DS.QUEUE.TRANSACTION.TIME = '20':Y.T24FS.TRANS.DATE.TIME[1,6]
            Y.DS.QUEUE.TRANSACTION.TIME = Y.T24FS.TRANS.DATE.TIME

            Y.REQ.COMPANY.CODE = R.T24.FUND.SERVICES.REC<TFS.CO.CODE>
        END ELSE
            CALL EB.READ.HISTORY.REC(F.T24.FUND.SERVICES.HIS,Y.DS.QUEUE.ID,R.TFS.HIS.REC,TFS.ERR)
            IF R.TFS.HIS.REC THEN
                Y.T24FS.TRANS.DATE.TIME = R.TFS.HIS.REC<TFS.BOOKING.DATE>
*                Y.DS.QUEUE.TRANSACTION.TIME = '20':Y.T24FS.TRANS.DATE.TIME[1,6]
                Y.DS.QUEUE.TRANSACTION.TIME = Y.T24FS.TRANS.DATE.TIME

                Y.REQ.COMPANY.CODE = R.TFS.HIS.REC<TFS.CO.CODE>
            END
        END

    END

RETURN
*********************
DEAL.SLIP.QUEUE.PARAM:
**********************

    IF NOT(Y.DS.QUEUE.PARAM.ERR) THEN
        Y.DS.QUEUE.PARAM.RETENTION.PERIOD = R.REDO.APAP.H.DEAL.SLIP.QUEUE.PARAM.REC<REDO.DS.QUEUE.PARAM.RECORD.RETENTION>
    END

    Y.DATES.ID.VAL = Y.REQ.COMPANY.CODE
    Y.DATES.ERR = ''
    CALL F.READ(FN.DATES,Y.DATES.ID.VAL,R.DATES.REC,F.DATES,Y.DATES.ERR)

    IF NOT(Y.DATES.ERR) THEN
        Y.CO.LAST.WORK.DAY = R.DATES.REC<EB.DAT.LAST.WORKING.DAY>
    END

    IF Y.CO.LAST.WORK.DAY OR Y.DS.QUEUE.TRANSACTION.TIME THEN
        REGION = ''
        Y.CUR.VALUE = Y.CO.LAST.WORK.DAY
        Y.PREV.VALUE = Y.DS.QUEUE.TRANSACTION.TIME
        NOF.DAYS = "C"
        CALL CDD (REGION,Y.CUR.VALUE,Y.PREV.VALUE,NOF.DAYS)
        Y.DIFF.DAYS = ABS(NOF.DAYS)
    END ELSE
        Y.DIFF.DAYS = ''
    END

    IF Y.DIFF.DAYS GT Y.DS.QUEUE.PARAM.RETENTION.PERIOD THEN
        GOSUB ACTUAL.PROCESS
    END

RETURN

*------------------------------------------------------------------------------------------
ACTUAL.PROCESS:
*------------------------------------------------------------------------------------------

    CALL F.READ(FN.REDO.APAP.H.DEAL.SLIP.QUEUE,Y.PURGE.ID,R.REDO.APAP.H.DEAL.SLIP.QUEUE,F.REDO.APAP.H.DEAL.SLIP.QUEUE,Y.DS.QUEUE.ERR)
    Y.TELLER.USER =R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.TELLER.USER>
    Y.INIT.PRINT = R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.INIT.PRINT>
    Y.REPRINT.SEQ = R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.REPRINT.SEQ>

* CHANGE SM TO VM IN Y.REPRINT

    Y.DATE.TIME = R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.TXN.DAY.TIME>
    Y.DEAL.SLIP.ID  = R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.DEAL.SLIP.ID>
    Y.REP = R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.REPRINT>
    CURR.NO = R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.CURR.NO>



    Y.COUNT = 1
    Y.DEAL.COUNT = DCOUNT(Y.DEAL.SLIP.ID,@VM) 
    LOOP
    WHILE Y.COUNT LE Y.DEAL.COUNT
        Y.REPRINT = R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.REPRINT,Y.COUNT>

        GOSUB DELETE.REPRINT
        Y.COUNT = Y.COUNT + 1
    REPEAT

RETURN

**************
DELETE.REPRINT:
***************

    Y.ALL.REPRINT = DCOUNT(Y.REPRINT,@SM) 

    CHANGE @SM TO @FM IN Y.REPRINT 

    Y.CNT = 1
    LOOP
    WHILE Y.CNT LE Y.ALL.REPRINT

        LOCATE "YES" IN Y.REPRINT SETTING FIND.POS THEN

            DEL Y.REP<1,Y.COUNT,FIND.POS>
            DEL Y.TELLER.USER<1,Y.COUNT,FIND.POS>
            DEL Y.INIT.PRINT<1,Y.COUNT,FIND.POS>
            DEL Y.REPRINT.SEQ<1,Y.COUNT,FIND.POS>
            DEL Y.DATE.TIME<1,Y.COUNT,FIND.POS>

        END

        Y.CNT = Y.CNT + 1

    REPEAT

    R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.REPRINT> = Y.REP
    R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.TELLER.USER>   = Y.TELLER.USER
    R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.INIT.PRINT>  = Y.INIT.PRINT
    R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.REPRINT.SEQ> = Y.REPRINT.SEQ
    R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.TXN.DAY.TIME> = Y.DATE.TIME


    SYS.TIME.NOW = OCONV(DATE(),"D-")
    SYS.TIME.NOW = SYS.TIME.NOW[9,2]:SYS.TIME.NOW[1,2]:SYS.TIME.NOW[4,2]
    SYS.TIME.NOW := TIMEDATE()[1,2]:TIMEDATE()[4,2]

    R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.CURR.NO> =  CURR.NO + 1
    R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.INPUTTER> = TNO:"_":OPERATOR
    R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.DATE.TIME> = SYS.TIME.NOW
    R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.AUTHORISER> = TNO:"_":OPERATOR
    R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.CO.CODE> = ID.COMPANY
    R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.DEPT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>


    CALL F.WRITE(FN.REDO.APAP.H.DEAL.SLIP.QUEUE,Y.PURGE.ID,R.REDO.APAP.H.DEAL.SLIP.QUEUE)

RETURN
******************************************************************************************************
END
*-----------------------------------------*END OF SUBROUTINE*------------------------------
