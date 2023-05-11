* @ValidationCode : MjotMTk4MjY2MTc0NjpDcDEyNTI6MTY4MjUwMjY1MzY5NzpJVFNTOi0xOi0xOjIwNjoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 26 Apr 2023 15:20:53
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 206
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.NOF.CASH.WINDOW.WIT.CCCA.R32(Y.CCY.LIST,TELLER.ID,Y.TT.PARAM.REC,R.REDO.H.TELLER.TXN.CODES,Y.FINAL.ARRAY,Y.TT.LIST,SET.CUR,Y.DUP.CUR)
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.NOF.CASH.WINDOW.WIT.CCCA.R32
*--------------------------------------------------------------------------------------------------------
*Description       : REDO.APAP.NOF.CASH.WINDOW.WIT.CCCA is a routine called from another routine
*                    REDO.APAP.NOF.CASH.WINDOW.WIT.SAV.CUR, this routine is used to fetch CREDIT CARD
*                    ADVANCES details
*Linked With       : Enquiry - REDO.APAP.ENQ.CASH.WINDOW.WIT
*In  Parameter     : Y.CCY.LIST     - This variable holds the processed currency list
*                    TELLER.ID      - Holds the teller ID
*                    Y.TT.PARAM.REC - Holds the teller and parameter records
*                    R.REDO.H.TELLER.TXN.CODES - The teller transaction code details record
*Out Parameter     : Y.FINAL.ARRAY  - THe final Array to be passed out
*                    Y.CCY.LIST     - This variable holds the processed currency list
*                    Y.TT.LIST      - This variable holds the processed TELLER records
*Files  Used       : TELLER                           As              I               Mode
*                    ACCOUNT                          As              I               Mode
*                    REDO.H.TELLER.TXN.CODES          As              I               Mode
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date               Who                         Reference                 Description
*   ------             -----                       -------------             -------------
* 15 Jun 2011       Marimuthu S              ODR-2011-04-0007 35         Changes made in REDO.APAP.NOF.CASH.WINDOW.WIT.CCCA
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*17-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   VMto@VM,FMto@FM
*17-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------

*
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.TELLER
    $INSERT I_F.REDO.H.TELLER.TXN.CODES
    $INSERT I_F.ACCOUNT
*-------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
* This is the para from where the execution of the code starts
    GOSUB OPEN.PARA
    GOSUB PROCESS.PARA
RETURN
*--------------------------------------------------------------------------------------------------------
**********
OPEN.PARA:
**********
* In this para of the code, file variables are initialised and opened
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    FN.ACCOUNT.HST = 'F.ACCOUNT$HIS'
    F.ACCOUNT.HST = ''
    CALL OPF(FN.ACCOUNT.HST,F.ACCOUNT.HST)
RETURN
*--------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************
* This is the main processing para
    R.TELLER                     = FIELD(Y.TT.PARAM.REC,'##',1)
    R.REDO.ADMIN.CHQ.PARAM       = FIELD(Y.TT.PARAM.REC,'##',2)
    R.REDO.MANAGER.CHQ.PARAM     = FIELD(Y.TT.PARAM.REC,'##',3)
    R.CERTIFIED.CHEQUE.PARAMETER = FIELD(Y.TT.PARAM.REC,'##',4)
    R.REDO.H.TELLER.TXN.CODES    = FIELD(Y.TT.PARAM.REC,'##',5)

    Y.FIN.FT.MG.CASH = ''; Y.FIN.FT.MG.CHQ  = ''; Y.FIN.FT.MG.TFR  = ''
    GOSUB FIND.MULTI.LOCAL.REF
    IF NOT(R.TELLER<TT.TE.LOCAL.REF,LOC.L.TT.POS.AUTHNM.POS>) THEN
        RETURN
    END

    Y.TT.CCY = R.TELLER<TT.TE.CURRENCY.1>
    YTT.MARKER = R.TELLER<TT.TE.DR.CR.MARKER>
    Y.TT.AMT = R.TELLER<TT.TE.LOCAL.REF,LOC.L.DEBIT.AMOUNT.POS>
    IF YTT.MARKER EQ 'CREDIT' AND R.TELLER<TT.TE.CURRENCY.1> NE LCCY THEN
        Y.TT.AMT = R.TELLER<TT.TE.LOCAL.REF,LOC.L.CREDIT.AMOUNT.POS>
    END
    IF NOT(Y.TT.AMT) THEN
        IF Y.TT.CCY EQ LCCY THEN
            Y.TT.AMT = R.TELLER<TT.TE.AMOUNT.LOCAL.1,1>
        END ELSE
            Y.TT.AMT = R.TELLER<TT.TE.AMOUNT.FCY.1>
        END
    END

    IF NOT(R.TELLER<TT.TE.LOCAL.REF,LOC.L.TT.CR.CARD.NO.POS>) THEN
        RETURN
    END

*    Y.CASH.CATEG    = R.REDO.H.TELLER.TXN.CODES<TT.TXN.CASH.ACC.CATEG>
*    CHANGE VM TO FM IN Y.CASH.CATEG

    Y.INT.ACCT.APAP = R.REDO.H.TELLER.TXN.CODES<TT.TXN.INTAC.APAP.CASH>
    CHANGE @VM TO @FM IN Y.INT.ACCT.APAP

    Y.INT.ACCT.OTHR = R.REDO.H.TELLER.TXN.CODES<TT.TXN.INTAC.OTH.CASH>
    CHANGE @VM TO @FM IN Y.INT.ACCT.OTHR

    Y.APAP.CASH = '' ;  Y.OTHER.CASH = '';    Y.APAP.TFR  = '' ;  Y.OTHER.TFR  = ''

    GOSUB CHECK.APAP.CASH
    GOSUB CHECK.APAP.TFR
    GOSUB CHECK.OTHER.CASH
    GOSUB CHECK.OTHER.TFR
RETURN
*--------------------------------------------------------------------------------------------------------
****************
CHECK.APAP.CASH:
****************
* In this para of the code, the credit card advences of APAP CASH transaction are checked

    LOCATE R.TELLER<TT.TE.ACCOUNT.2> IN Y.INT.ACCT.APAP SETTING Y.APAP.POS ELSE
        RETURN
    END

    Y.APAP.CASH = 1
    Y.VM.POS    = 17
    Y.ADD.AMT   = Y.TT.AMT
    GOSUB AMEND.FINAL.ARRAY
RETURN
*--------------------------------------------------------------------------------------------------------
***************
CHECK.APAP.TFR:
***************
* In this para of the code, the credit card advences of APAP TRANSFER transaction are checked

    LOCATE TELLER.ID IN Y.TT.ARR.IDS SETTING POS.ID THEN
        RETURN
    END

    LOCATE R.TELLER<TT.TE.ACCOUNT.1> IN Y.INT.ACCT.APAP SETTING Y.APAP.POS ELSE
        RETURN
    END

    ACCOUNT.ID = R.TELLER<TT.TE.ACCOUNT.2>
    GOSUB READ.ACCOUNT
    IF NOT(R.ACCOUNT<AC.CUSTOMER>) THEN
        RETURN
    END

    Y.APAP.TFR = 1
    Y.VM.POS   = 17
    Y.ADD.AMT  = Y.TT.AMT
    GOSUB AMEND.FINAL.ARRAY
RETURN
*--------------------------------------------------------------------------------------------------------
*****************
CHECK.OTHER.CASH:
*****************
* In this para of the code, the credit card advences of other bank CASH transaction are checked

    LOCATE TELLER.ID IN Y.TT.ARR.IDS SETTING POS.ID THEN
        RETURN
    END

    LOCATE R.TELLER<TT.TE.ACCOUNT.2> IN Y.INT.ACCT.OTHR SETTING Y.OTHR.POS ELSE
        RETURN
    END

    Y.OTHER.CASH = 1
    Y.VM.POS     = 18
    Y.ADD.AMT    = Y.TT.AMT
    GOSUB AMEND.FINAL.ARRAY
RETURN
*--------------------------------------------------------------------------------------------------------
****************
CHECK.OTHER.TFR:
****************
* In this para of the code, the credit card advences of other bank TRANSFER transaction are checked

    LOCATE TELLER.ID IN Y.TT.ARR.IDS SETTING POS.ID THEN
        RETURN
    END

    LOCATE R.TELLER<TT.TE.ACCOUNT.1> IN Y.INT.ACCT.OTHR SETTING Y.OTHR.POS ELSE
        RETURN
    END

    ACCOUNT.ID = R.TELLER<TT.TE.ACCOUNT.2>
    GOSUB READ.ACCOUNT
    IF NOT(R.ACCOUNT<AC.CUSTOMER>) THEN
        RETURN
    END

    Y.OTHER.TFR = 1
    Y.VM.POS    = 18
    Y.ADD.AMT   = Y.TT.AMT
    GOSUB AMEND.FINAL.ARRAY
RETURN
*--------------------------------------------------------------------------------------------------------
******************
AMEND.FINAL.ARRAY:
******************
* In this para of the code, the Y.FINAL.ARRAY is amended with increment in the total number of transactions
** and the amount is added up

    LOCATE Y.TT.CCY IN Y.CCY.LIST<1> SETTING Y.CCY.POS ELSE
        Y.CCY.LIST<-1> = Y.TT.CCY
        Y.CCY.POS = DCOUNT(Y.CCY.LIST,@FM)
    END

    IF Y.APAP.CASH THEN
        Y.FINAL.ARRAY<Y.CCY.POS,Y.VM.POS,3>  += 1
        Y.FINAL.ARRAY<Y.CCY.POS,Y.VM.POS,4>  += Y.ADD.AMT
        Y.TT.ARR.IDS<-1> = TELLER.ID
    END
    IF Y.APAP.TFR THEN
        Y.FINAL.ARRAY<Y.CCY.POS,Y.VM.POS,3>  += 1
        Y.FINAL.ARRAY<Y.CCY.POS,Y.VM.POS,6>  += Y.ADD.AMT
        Y.TT.ARR.IDS<-1> = TELLER.ID
    END

    IF Y.OTHER.CASH THEN
        Y.FINAL.ARRAY<Y.CCY.POS,Y.VM.POS,3>  += 1
        Y.FINAL.ARRAY<Y.CCY.POS,Y.VM.POS,4>  += Y.ADD.AMT
        Y.TT.ARR.IDS<-1> = TELLER.ID
    END
    IF Y.OTHER.TFR THEN
        Y.FINAL.ARRAY<Y.CCY.POS,Y.VM.POS,3>  += 1
        Y.FINAL.ARRAY<Y.CCY.POS,Y.VM.POS,6>  += Y.ADD.AMT
        Y.TT.ARR.IDS<-1> = TELLER.ID
    END

    Y.TT.LIST<-1> = TELLER.ID
RETURN
*--------------------------------------------------------------------------------------------------------
*************
READ.ACCOUNT:
*************
* In this para of the code, file ACCOUNT is read
    R.ACCOUNT  = '';    ACCOUNT.ER = ''
    CALL F.READ(FN.ACCOUNT,ACCOUNT.ID,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ER)
    IF NOT(R.ACCOUNT) THEN
        ACCOUNT.IDH = ACCOUNT.ID; ERR.AC = ''
        CALL EB.READ.HISTORY.REC(F.ACCOUNT.HST,ACCOUNT.IDH,R.ACCOUNT,ERR.AC)
    END
RETURN
*--------------------------------------------------------------------------------------------------------
*********************
FIND.MULTI.LOCAL.REF:
*********************
* In this para of the code, local reference field positions are obtained
    APPL.ARRAY = 'TELLER'
    FLD.ARRAY  = 'L.TT.CR.CARD.NO':@VM:'L.DEBIT.AMOUNT':@VM:'L.CREDIT.AMOUNT':@VM:'L.TT.POS.AUTHNM'
    FLD.POS    = ''
    CALL MULTI.GET.LOC.REF(APPL.ARRAY,FLD.ARRAY,FLD.POS)
    LOC.L.TT.CR.CARD.NO.POS  =  FLD.POS<1,1>
    LOC.L.DEBIT.AMOUNT.POS = FLD.POS<1,2>
    LOC.L.CREDIT.AMOUNT.POS = FLD.POS<1,3>
*LOC.L.TT.POS.AUTHNM.POS  =  FLD.POS<1,1>
*tus-start
    LOC.L.TT.POS.AUTHNM.POS  =  FLD.POS<1,4>
*tus-end
RETURN
*--------------------------------------------------------------------------------------------------------
END       ;* End of Program
