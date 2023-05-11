* @ValidationCode : MjotOTMxODQ1MjkwOkNwMTI1MjoxNjgyNDEyMzMwOTY2OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:30
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.INP.CHQ.ISS
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: H GANESH
* PROGRAM NAME: REDO.INP.CHQ.ISS
* ODR NO      : ODR-2009-12-0275
*----------------------------------------------------------------------
*DESCRIPTION: It is neccessary to create routine REDO.INP.CHQ.ISS to
* to check the TAX.AMOUNT with ACCOUNT BALANCE. And to default the CHQ.NO.START
* And to check the MONTO.CK is modified or not

*IN PARAMETER:NONE
*OUT PARAMETER:NONE
*LINKED WITH: REDO.H.SOLICITUD.CK
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*19.02.2010  H GANESH     ODR-2009-12-0275     INITIAL CREATION
*16.04.2011  H GANESH     PACS00032454            Changes made to get account balance from local field L.AC.AV.BAL
*03-06-2011  H GANESH     PACS00072713            MODIFICATION
*27.01.2012  SHANKAR RAJU PACS00176990         Tax Calculation part changes[*0.15/100], CANT.CK is assigned to COMI in case of browser.
* 06-04-2023	CONVERSION TOOL		AUTO R22 CODE CONVERSION	 FM to @FM, VM to @VM
* 06-04-2023	MUTHUKUMAR M		MANUAL R22 CODE CONVERSION	 NO CHANGE

*----------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_EB.EXTERNAL.COMMON
    $INSERT I_F.OFS.SOURCE
    $INSERT I_F.REDO.H.CHEQUE.REGISTER
    $INSERT I_F.STOCK.ENTRY
    $INSERT I_F.REDO.H.SOLICITUD.CK
    $INSERT I_F.FT.COMMISSION.TYPE
    $INSERT I_GTS.COMMON

    IF V$FUNCTION EQ 'R' THEN
        RETURN
    END

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB MULTI.GET.LOC.REF
    GOSUB PROCESS
    GOSUB CHQ.START.NO
    GOSUB MONTO.CHANGE
*Logic from the routine REDO.V.VAL.WAIVE.CHARGES - If the field WAIVE.CHARGES is YES, an Override message has to be displayed
    GOSUB V.VAL.WAIVE.CHARGES

RETURN
*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------
    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    R.ACCT=''
    FN.REDO.H.CHEQUE.REGISTER='F.REDO.H.CHEQUE.REGISTER'
    F.REDO.H.CHEQUE.REGISTER=''
    R.REDO.H.CHEQUE.REGISTER=''
    FN.STOCK.ENTRY='F.STOCK.ENTRY'
    F.STOCK.ENTRY=''
    R.STOCK.ENTRY=''
    FN.FT.COMMISSION.TYPE='F.FT.COMMISSION.TYPE'
    F.FT.COMMISSION.TYPE=''
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER  = ''
    CURR.NO = 0
    CALL STORE.OVERRIDE(CURR.NO)
    CURRENT.CHANNEL = EB.EXTERNAL$CHANNEL
RETURN
*----------------------------------------------------------------------
OPENFILES:
*----------------------------------------------------------------------
    CALL OPF(FN.REDO.H.CHEQUE.REGISTER,F.REDO.H.CHEQUE.REGISTER)
    CALL OPF(FN.STOCK.ENTRY,F.STOCK.ENTRY)
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.FT.COMMISSION.TYPE,F.FT.COMMISSION.TYPE)
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
RETURN
*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
*Populate the value to Tax.Amount Field


    Y.ACCOUNT.ID=R.NEW(REDO.H.SOL.ACCOUNT)
    CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.ID,R.ACCT,F.ACCOUNT,ACC.ERR)
    Y.CUS.ID = R.ACCT<AC.CUSTOMER>
    CALL F.READ(FN.CUSTOMER,Y.CUS.ID,R.CUST,F.CUSTOMER,CUS.ERR)
    Y.CUS.TYPE = R.CUST<EB.CUS.LOCAL.REF,POS.L.CU.TIPO.CL>

    Y.CHEQUE.STATUS=R.NEW(REDO.H.SOL.CHEQUE.STATUS)

    IF Y.CHEQUE.STATUS EQ 10 THEN
        Y.MONT.CK=R.NEW(REDO.H.SOL.MONTO.CK)
        CNT.MONTO.CK=DCOUNT(Y.MONT.CK,@VM)
        CNT=1
        LOOP
        WHILE CNT LE CNT.MONTO.CK
            Y.MONT.CK1=R.NEW(REDO.H.SOL.MONTO.CK)<1,CNT>
            TOT.SUM+=Y.MONT.CK1
            CNT += 1 ;*AUTO R22 CODE CONVERSION
        REPEAT
*** PACS00176990 - Tax Calculation part changes[*0.15/100] S ->
        Y.TAX.AMT = (TOT.SUM*0.15)/100
        Y.CURRENCY = LCCY
        CALL EB.ROUND.AMOUNT(Y.CURRENCY,Y.TAX.AMT,"","")

        R.NEW(REDO.H.SOL.TAX.AMOUNT)= Y.TAX.AMT
*** PACS00176990 - Tax Calculation part changes[*0.15/100] -> R
****************************************
*PACS00032454-S                        * ;*AUTO R22 CODE CONVERSION
*ACC.BAL=R.ACCT<AC.ONLINE.ACTUAL.BAL>  *
*PACS00032454-E                        * ;*AUTO R22 CODE CONVERSION
****************************************
        ACC.BAL=R.ACCT<AC.LOCAL.REF,POS.L.AC.AV.BAL>
        IF ACC.BAL LT (Y.TAX.AMT+TOT.SUM) AND R.NEW(REDO.H.SOL.WAIVE.CHARGES) NE 'YES' THEN
            IF PGM.VERSION EQ ',INPUT.PF' OR PGM.VERSION EQ ',INPUT.PJ' THEN
                GOSUB CHG.OVERRIDE
            END
            IF PGM.VERSION EQ ',AI.REDO.PF.INPUT' OR PGM.VERSION EQ ',AI.REDO.PJ.INPUT' THEN
                GOSUB CHG.ERROR
            END
        END ELSE

*PACS00072713-S/E ;*AUTO R22 CODE CONVERSION
            IF PGM.VERSION EQ ',AI.REDO.PF.INPUT' OR PGM.VERSION EQ ',AI.REDO.PJ.INPUT' THEN

                Y.DEL.TIME = R.NEW(REDO.H.SOL.TIEMPO.ENT)
                IF CURRENT.CHANNEL NE 'INTERNET' THEN
                    CURR.NO=DCOUNT(R.NEW(REDO.H.SOL.OVERRIDE),@VM) + 1
                    TEXT='REDO.MSG.FOR.ARCIB':@FM:Y.DEL.TIME
                    CALL STORE.OVERRIDE(CURR.NO)
                END
            END
        END
    END
RETURN
****************
CHG.OVERRIDE:
****************
*PACS00032454-S ;*AUTO R22 CODE CONVERSION
    IF (CURRENT.CHANNEL NE 'INTERNET') THEN
        IF (OFS$OPERATION NE 'VALIDATE') THEN
            CURR.NO=DCOUNT(R.NEW(REDO.H.SOL.OVERRIDE),@VM) + 1
            TEXT='REDO.NO.ENGH.FUND'
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END ELSE

        ETEXT='AI-REDO.NO.ENGH.FUND'
        CALL STORE.END.ERROR
    END

*PACS00032454-E ;*AUTO R22 CODE CONVERSION
RETURN

*************
CHG.ERROR:
**************


    ETEXT='EB-DESC.CHQ.STATUS.1'
    CALL STORE.END.ERROR


RETURN
*-------------
CHQ.START.NO:
*-------------

    Y.REDO.H.CHEQUE.REGISTER=R.NEW(REDO.H.SOL.CHEQUE.TYPE):".":R.NEW(REDO.H.SOL.ACCOUNT)
    CALL F.READ(FN.REDO.H.CHEQUE.REGISTER,Y.REDO.H.CHEQUE.REGISTER,R.REDO.H.CHEQUE.REGISTER,F.REDO.H.CHEQUE.REGISTER,ERR.CHQ.REG)
    IF R.REDO.H.CHEQUE.REGISTER EQ '' THEN
        STK.ENT.ID=R.NEW(REDO.H.SOL.FICHA.IMPR)
        CALL F.READ(FN.STOCK.ENTRY,STK.ENT.ID,R.STOCK.ENTRY,F.STOCK.ENTRY,ERR.STK.ENT)
        STOCK.START.NO.CNT=DCOUNT(R.STOCK.ENTRY<STO.ENT.STOCK.START.NO>,@VM)
        IF R.NEW(REDO.H.SOL.CHEQUE.TYPE) EQ 'DDAF' THEN
            R.NEW(REDO.H.SOL.CHQ.NO.START)=FMT(R.STOCK.ENTRY<STO.ENT.STOCK.START.NO,STOCK.START.NO.CNT>,"R%4")
        END
        IF R.NEW(REDO.H.SOL.CHEQUE.TYPE) EQ 'DDA' THEN
            R.NEW(REDO.H.SOL.CHQ.NO.START)=FMT(R.STOCK.ENTRY<STO.ENT.STOCK.START.NO,STOCK.START.NO.CNT>,"R%6")
        END


    END
    IF R.REDO.H.CHEQUE.REGISTER NE '' THEN
        CNT.OF.CHEQ= DCOUNT(R.REDO.H.CHEQUE.REGISTER<CHEQ.REG.CHEQUE.NOS>,@VM)
        Y.CHEQUE.NO=R.REDO.H.CHEQUE.REGISTER<CHEQ.REG.CHEQUE.NOS,CNT.OF.CHEQ>
        LAST.CHQ.NUM = FIELD(Y.CHEQUE.NO,"-",2)
        NEXT.CHQ.NUM =LAST.CHQ.NUM+1
        IF R.NEW(REDO.H.SOL.CHEQUE.TYPE) EQ 'DDAF' THEN
            R.NEW(REDO.H.SOL.CHQ.NO.START)=FMT(NEXT.CHQ.NUM,"R%4")
        END
        IF R.NEW(REDO.H.SOL.CHEQUE.TYPE) EQ 'DDA' THEN
            R.NEW(REDO.H.SOL.CHQ.NO.START)=FMT(NEXT.CHQ.NUM,"R%6")
        END
    END
RETURN
*----------------------------------------------------------------------
MONTO.CHANGE:
*----------------------------------------------------------------------
    Y.MONTO.CK=R.NEW(REDO.H.SOL.MONTO.CK)
    Y.MONTO.CK.CNT=DCOUNT(Y.MONTO.CK,@VM)
    VAR2=1
    LOOP
    WHILE VAR2 LE Y.MONTO.CK.CNT
        CHANGED.MONTO.CK = Y.MONTO.CK<1,VAR2>
        FT.COMM.ID =  R.NEW(REDO.H.SOL.COSTO.CK)<1,VAR2>
        CALL CACHE.READ(FN.FT.COMMISSION.TYPE, FT.COMM.ID, R.FT.COMMISSION.TYPE, FT.COMMI.ERR) ;*AUTO R22 CODE CONVERSION
        FLAT.AMT.CNT= DCOUNT(R.FT.COMMISSION.TYPE<FT4.FLAT.AMT>,@VM)
        TOTAL.FLAT.AMT=''
        TOT.MONTO.CK=''
        VAR1=1
        LOOP
        WHILE VAR1 LE FLAT.AMT.CNT
            TOTAL.FLAT.AMT=R.FT.COMMISSION.TYPE<FT4.FLAT.AMT,VAR1>+TOTAL.FLAT.AMT
            VAR1 += 1
        REPEAT
        Y.CANT.TAL=R.NEW(REDO.H.SOL.CANT.TAL)
        TOT.MONTO.CK = TOTAL.FLAT.AMT*Y.CANT.TAL
        IF CURRENT.CHANNEL NE 'INTERNET' THEN
            IF (CHANGED.MONTO.CK NE TOT.MONTO.CK) THEN
                IF (OFS$OPERATION NE 'VALIDATE') THEN
                    CURR.NO=DCOUNT(R.NEW(REDO.H.SOL.OVERRIDE),@VM) + 1
                    TEXT = 'REDO.AMT.GEN.MOD'
                    CALL STORE.OVERRIDE(CURR.NO)
                END
            END
        END
        VAR2 += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT
RETURN
*---------------------------------------------------------------
MULTI.GET.LOC.REF:
*---------------------------------------------------------------
* This part gets local ref field position
    LOC.REF.APPLICATION="ACCOUNT":@FM:"CUSTOMER"
    LOC.REF.FIELDS='L.AC.AV.BAL':@FM:'L.CU.TIPO.CL'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.AC.AV.BAL  = LOC.REF.POS<1,1>
    POS.L.CU.TIPO.CL = LOC.REF.POS<2,1>
RETURN
*---------------------------------------------------------------
V.VAL.WAIVE.CHARGES:
*-------------------
    Y.WAIVE.CHARGES=R.NEW(REDO.H.SOL.WAIVE.CHARGES)
    IF Y.WAIVE.CHARGES EQ 'YES' THEN
        IF (OFS$OPERATION NE 'VALIDATE') THEN
            CURR.NO=DCOUNT(R.NEW(REDO.H.SOL.OVERRIDE),@VM) + 1
            TEXT='REDO.WAIVE.CHARGES'
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END
RETURN
*-----------------------------------------------------------------------------
END
