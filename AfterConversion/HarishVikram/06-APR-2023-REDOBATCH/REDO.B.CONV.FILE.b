* @ValidationCode : Mjo2NTUzMDA0OTpDcDEyNTI6MTY4MDc1OTE0Njg1NzpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Apr 2023 11:02:26
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.CONV.FILE(BUILD.LIST)
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Riyas
* Program Name  : REDO.B.CONV.FILE
* ODR           : ODR-2010-08-0031
*------------------------------------------------------------------------------------------
*DESCRIPTION  : REDO.B.CONV.FILE  Multithreading routine responsible for generates
*------------------------------------------------------------------------------------------
* In parameter  : None
* out parameter : None
*------------------------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------------------------
* DATE             WHO                         REFERENCE            DESCRIPTION
*==============    ==============              =================    =================
* 19.10.2010       Sakthi Sellappillai         ODR-2010-08-0031     INITIAL CREATION
* 04-APR-2023     Conversion tool    R22 Auto conversion             FM TO @FM
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.SUPPLIER.PAYMENT
    $INSERT I_F.EB.FILE.UPLOAD.PARAM
    $INSERT I_F.REDO.FILE.DATE.PROCESS
    $INSERT I_REDO.B.CONV.FILE.COMMON
    $INSERT I_F.REDO.SUPPLIER.PAY.DATE
    $INSERT I_F.AI.REDO.ACCT.RESTRICT.PARAMETER
    $INSERT I_F.AI.REDO.ARCIB.PARAMETER
    $INSERT I_F.LOCKING
    $INSERT I_F.ACCOUNT

    GOSUB READ.CSV.FILE
    GOSUB GOEND
RETURN
*------------------------------------------------------------------------------------------
READ.CSV.FILE:
*------------------------------------------------------------------------------------------
* Read the Flat file
    Y.TOTAL.AMOUNT = ''
    Y.SRC.ACCT = ''
    Y.SRC.CUSTOMER = ''
    Y.FILE.PATH = F.FILE.PROC.PATH
    Y.DUM.FILE.NAME = BUILD.LIST
    Y.DUM.FILE.NAME.SEC.VAL = Y.DUM.FILE.NAME['.',2,1]
    R.REDO.FILE.DATE.PROCESS = ''
    Y.REDO.SUPPLIER.SEQ = ''
    IF Y.DUM.FILE.NAME.SEC.VAL EQ 'T24' THEN
        Y.FILE.NAME = Y.DUM.FILE.NAME
        READ Y.REC.ARRAY FROM Y.FILE.PATH,Y.FILE.NAME THEN
            CRLF = CHARX(013):CHARX(254)
            CHANGE CRLF TO @FM IN Y.REC.ARRAY
            LOOP
                REMOVE Y.REC FROM Y.REC.ARRAY SETTING Y.POS
            WHILE Y.REC : Y.POS
                Y.SRC.ACCT = FIELD(Y.REC,',',1)
                CALL F.READ(FN.ACCOUNT,Y.SRC.ACCT,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)

* IF ACCOUNT.ERR THEN
*     CONTINUE
* END

                CR =  CHARX(013)
                CHANGE CR TO '' IN Y.REC
                Y.INIT.UPLOAD.VAL = FIELD(Y.REC,',',1)
                IF NOT(Y.INIT.UPLOAD.VAL) THEN
                    CONTINUE
                END

                IF NOT(Y.REDO.SUPPLIER.ID) THEN
                    Y.REDO.SUPPLIER.SEQ = 1
                END
                Y.REDO.SUPPLIER.SEQ = FMT(Y.REDO.SUPPLIER.SEQ,'6"0"R')
                Y.REDO.SUPPLIER.ID = Y.FILE.NAME:'.':Y.REDO.SUPPLIER.SEQ
                Y.CNT.FILE.VALUES  = DCOUNT(Y.REC,',')
                GOSUB FILE.RECORD.PROCESS

                R.REDO.FILE.DATE.PROCESS<REDO.FILE.PRO.PAY.RECORD.ID,-1> = Y.REDO.SUPPLIER.ID
                R.REDO.FILE.DATE.PROCESS<REDO.FILE.PRO.FILE.TOT.AMOUNT> = Y.TOTAL.AMOUNT
                R.REDO.FILE.DATE.PROCESS<REDO.FILE.PRO.FILE.COM.AMOUNT> = Y.TOT.COMM.AMT
                CALL F.WRITE(FN.REDO.FILE.DATE.PROCESS,Y.FILE.NAME,R.REDO.FILE.DATE.PROCESS)
                Y.REDO.SUPPLIER.SEQ+=1
            REPEAT
        END
        GOSUB FILE.DATE.PROCESS
    END

RETURN
*------------------------------------------------------------------------------------------
FILE.RECORD.PROCESS:
*------------------------------------------------------------------------------------------
    Y.SRC.ACCT = FIELD(Y.REC,',',1)
    CALL F.READ(FN.ACCOUNT,Y.SRC.ACCT,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    IF R.ACCOUNT THEN
        Y.SRC.CUSTOMER = R.ACCOUNT<AC.CUSTOMER>
    END

    IF Y.CNT.FILE.VALUES EQ 7 THEN
        GOSUB GEN.PAYROLL.RECORD
    END  ELSE
        GOSUB GEN.SUPPLIER.RECORD
    END
RETURN
*------------------------------------------------------------------------------------------
GEN.SUPPLIER.RECORD:
*------------------------------------------------------------------------------------------
    CR =  CHARX(013)
    CHANGE CR TO '' IN Y.REC
    Y.INIT.UPLOAD.VAL = FIELD(Y.REC,',',1)
    IF NOT(Y.INIT.UPLOAD.VAL) THEN
        CALL F.DELETE(FN.FILE.PROC.PATH,Y.FILE.NAME)
        RETURN
    END
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.FILE.NAME>=Y.FILE.NAME
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.SOURCE.ACCOUNT>=FIELD(Y.REC,',',1)
    Y.DATE.VAL = FIELD(Y.FILE.NAME,'.',6)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.PAYMENT.DATE>= Y.DATE.VAL
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.BANK.CODE>=FIELD(Y.REC,',',3)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.BANK.NAME>=FIELD(Y.REC,',',4)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.BEN.ACCOUNT>=FIELD(Y.REC,',',5)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.BEN.ID.NO>=FIELD(Y.REC,',',7)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.IDENTIFY.TYPE>=FIELD(Y.REC,',',6)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.BEN.CUSTOMER>=FIELD(Y.REC,',',8)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.INVOICE.NO>=FIELD(Y.REC,',',9)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.NCF.NO>=FIELD(Y.REC,',',10)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.CURRENCY>=FIELD(Y.REC,',',11)

    Y.TEMP.LAST=FIELD(Y.REC,',',12)
    CHANGE ',' TO '' IN Y.TEMP.LAST
*   CHANGE '.' TO '' IN Y.TEMP.LAST
*   Y.TEMP.LAST.LEN = LEN(Y.TEMP.LAST)
*   Y.TEMP.LAST=Y.TEMP.LAST[1,Y.TEMP.LAST.LEN-2]

    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.AMOUNT>=Y.TEMP.LAST
    IF Y.PAY.COM.FLAT.AMT THEN
        Y.COMMISSION.AMT = Y.TEMP.LAST + Y.SUP.COM.FLAT.AMT
    END
    IF Y.PAY.COM.PERCENT THEN
        Y.COMMISSION.AMT = Y.TEMP.LAST * (Y.SUP.COM.PERCENT/100)
    END
    DEAL.CURRENCY = FIELD(Y.REC,',',6)
    IF Y.SUPPLIER.CHG.TYPE THEN

        CUSTOMER = Y.SRC.CUSTOMER
        DEAL.AMOUNT = Y.TEMP.LAST
        CALL CALCULATE.CHARGE(CUSTOMER, DEAL.AMOUNT, DEAL.CURRENCY, CCY.MARKET,CROSS.RATE, CROSS.CURRENCY, DRAWDOWN.CCY, Y.SUPPLIER.CHG.TYPE,CUST.COND,CHG.AMT.TOT, TOT.CHARGE.FCCY)
        Y.COMMISSION.AMT + = CHG.AMT.TOT
    END

    Y.COMMISSION.AMT = DROUND(Y.COMMISSION.AMT,2)

    Y.TOTAL.AMOUNT + = Y.TEMP.LAST
    Y.TOT.COMM.AMT + = Y.COMMISSION.AMT
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.FILE.TOT.AMT> = Y.TEMP.LAST+Y.COMMISSION.AMT
    GOSUB DELETE.FILE.PATH.PARA
RETURN
*------------------------------------------------------------------------------------------
GEN.PAYROLL.RECORD:
*------------------------------------------------------------------------------------------

    CR =  CHARX(013)
    CHANGE CR TO '' IN Y.REC
    Y.INIT.UPLOAD.VAL = FIELD(Y.REC,',',1)
    IF NOT(Y.INIT.UPLOAD.VAL) THEN
        CALL F.DELETE(FN.FILE.PROC.PATH,Y.FILE.NAME)
        RETURN
    END
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.FILE.NAME>=Y.FILE.NAME
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.SOURCE.ACCOUNT>=FIELD(Y.REC,',',1)
    Y.DATE.VAL = FIELD(Y.FILE.NAME,'.',6)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.PAYMENT.DATE>= Y.DATE.VAL
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.BEN.ACCOUNT>=FIELD(Y.REC,',',2)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.BEN.ID.NO>=FIELD(Y.REC,',',4)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.IDENTIFY.TYPE>=FIELD(Y.REC,',',3)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.BEN.CUSTOMER>=FIELD(Y.REC,',',5)
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.CURRENCY>=FIELD(Y.REC,',',6)


    Y.TEMP.LAST=FIELD(Y.REC,',',7)
    CHANGE ',' TO '' IN Y.TEMP.LAST
*   CHANGE '.' TO '' IN Y.TEMP.LAST
*   Y.TEMP.LAST.LEN = LEN(Y.TEMP.LAST)
*   Y.TEMP.LAST = Y.TEMP.LAST[1,Y.TEMP.LAST.LEN-2]

    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.AMOUNT>=Y.TEMP.LAST
    IF Y.PAY.COM.FLAT.AMT THEN
        Y.COMMISSION.AMT = Y.TEMP.LAST + Y.PAY.COM.FLAT.AMT
    END
    IF Y.PAY.COM.PERCENT THEN
        Y.COMMISSION.AMT = Y.TEMP.LAST * (Y.PAY.COM.PERCENT/100)
    END
    DEAL.CURRENCY = FIELD(Y.REC,',',6)

    IF Y.PAYROLL.CHG.TYPE THEN

        CUSTOMER = Y.SRC.CUSTOMER
        DEAL.AMOUNT = Y.TEMP.LAST
        CALL CALCULATE.CHARGE(CUSTOMER, DEAL.AMOUNT, DEAL.CURRENCY, CCY.MARKET,CROSS.RATE, CROSS.CURRENCY, DRAWDOWN.CCY, Y.PAYROLL.CHG.TYPE, CUST.COND, CHG.AMT.TOT, TOT.CHARGE.FCCY)
        Y.COMMISSION.AMT + = CHG.AMT.TOT
    END
    Y.COMMISSION.AMT = DROUND(Y.COMMISSION.AMT,2)

    Y.TOTAL.AMOUNT + = Y.TEMP.LAST
    Y.TOT.COMM.AMT + = Y.COMMISSION.AMT
    R.REDO.SUPPLIER.PAYMNET<REDO.SUP.PAY.FILE.TOT.AMT> = Y.TEMP.LAST+Y.COMMISSION.AMT
    GOSUB DELETE.FILE.PATH.PARA
RETURN
**********************
DELETE.FILE.PATH.PARA:
**********************
    LOCATE Y.DATE.VAL IN Y.DATE.ARRAY SETTING POS THEN
        Y.ID.ARRAY<POS,-1> = Y.REDO.SUPPLIER.ID
    END ELSE
        Y.DATE.ARRAY<-1> = Y.DATE.VAL
        Y.DATE.LAST.CNT = DCOUNT(Y.DATE.ARRAY,@FM)
        Y.ID.ARRAY<Y.DATE.LAST.CNT,-1> = Y.REDO.SUPPLIER.ID
    END
    CALL F.WRITE(FN.REDO.SUPPLIER.PAYMENT,Y.REDO.SUPPLIER.ID,R.REDO.SUPPLIER.PAYMNET)
    CALL F.DELETE(FN.FILE.PROC.PATH,Y.FILE.NAME)
RETURN
*------------------------------------------------------------------------------------------
FILE.DATE.PROCESS:
*------------------------------------------------------------------------------------------
    Y.DATE.FINAL.COUNT = DCOUNT(Y.DATE.ARRAY,@FM)
    Y.DATE.LOC=1
    LOOP
        REMOVE Y.INIT.DATE FROM Y.DATE.ARRAY  SETTING Y.DATE.POS
    WHILE Y.INIT.DATE : Y.DATE.POS
        Y.SUPPLIER.VAL = Y.ID.ARRAY<Y.DATE.LOC>
        Y.DATE.LOC += 1
        CHANGE @VM TO @FM IN Y.SUPPLIER.VAL
        CALL F.READU(FN.REDO.SUPPLIER.PAY.DATE,Y.INIT.DATE,R.REC.SUP.PAY,F.REDO.SUPPLIER.PAY.DATE,Y.REDO.SUPP.ERR,Y.REDO.SUPP.RETRY)
        IF R.REC.SUP.PAY THEN
            R.REC.SUP.PAY<-1> = Y.SUPPLIER.VAL
        END ELSE
            R.REC.SUP.PAY<1> = Y.SUPPLIER.VAL
        END
        CALL F.WRITE(FN.REDO.SUPPLIER.PAY.DATE,Y.INIT.DATE,R.REC.SUP.PAY)
    REPEAT
RETURN
*------------------------------------------------------------------------------------------
GOEND:
*------------------------------------------------------------------------------------------
END
*------------------------------*END OF SUBROUTINE*-----------------------------------------
