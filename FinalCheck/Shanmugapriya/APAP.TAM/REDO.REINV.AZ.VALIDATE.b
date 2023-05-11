* @ValidationCode : MjotNTczNzUxNDc6Q3AxMjUyOjE2ODEzNzM0NzkzODI6MzMzc3U6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 13:41:19
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.REINV.AZ.VALIDATE

*--------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Program Name : REDO.REINV.AZ.VALIDATE
*--------------------------------------------------------------------------------
* Description: This Validation routine attached to version AZ.ACCOUNT,MAIN
* to check the source of fund
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*
*  DATE             WHO             REFERENCE         DESCRIPTION
* 04-Jul-2011   H GANESH            PACS00072695_N.11 INITIAL CREATION
* 06/03/2013    Vignesh Kummar R    PACS00253693      DECEASED ACCOUNT CHECK
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*13/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION         FM TO @FM, VM TO @VM, SM TO @SM, F.READ TO CACHE.READ, ++ TO +=
*13/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.AZ.PRODUCT.PARAMETER
    $INSERT I_F.ENQUIRY
    $INSERT I_F.MULTI.BRANCH.INTERNAL.ACCOUNT

 
    GOSUB INIT
    GOSUB PROCESS
RETURN
*---------------------------------------------------------------------------------
INIT:
*---------------------------------------------------------------------------------

    LOC.REF.APPLICATION="AZ.ACCOUNT":@FM:"AZ.PRODUCT.PARAMETER":@FM:"ACCOUNT"
    LOC.REF.FIELDS='L.AZ.METHOD.PAY':@VM:'L.AZ.AMOUNT':@VM:'L.AZ.SRC.FUND':@VM:'L.AZ.DEBIT.ACC':@VM:'L.AZ.IN.TRANSIT':@FM:'L.AZP.TRAN.DAYS':@FM:'L.AC.AV.BAL':@VM:'L.AC.STATUS2'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.AZ.METHOD.PAY = LOC.REF.POS<1,1>
    POS.L.AZ.AMOUNT     = LOC.REF.POS<1,2>
    POS.L.AZ.SRC.FUND   = LOC.REF.POS<1,3>
    POS.L.AZ.DEBIT.ACC  = LOC.REF.POS<1,4>
    POS.L.AZ.IN.TRANSIT = LOC.REF.POS<1,5>
    POS.L.AZP.TRAN.DAYS = LOC.REF.POS<2,1>
    POS.L.AC.AV.BAL     = LOC.REF.POS<3,1>
    GET.ACCT.STATUS.POS = LOC.REF.POS<3,2>

    FN.AZ.PRODUCT.PARAMETER = 'F.AZ.PRODUCT.PARAMETER'
    F.AZ.PRODUCT.PARAMETER= ''
    CALL OPF(FN.AZ.PRODUCT.PARAMETER,F.AZ.PRODUCT.PARAMETER)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    Y.TOT.AMT=''

    FN.ENQUIRY = 'F.ENQUIRY'
    F.ENQUIRY = ''
    CALL OPF(FN.ENQUIRY,F.ENQUIRY)

    FN.MULTI.BRANCH.INTERNAL.ACCOUNT = 'F.MULTI.BRANCH.INTERNAL.ACCOUNT'
    F.MULTI.BRANCH.INTERNAL.ACCOUNT = ''
    CALL OPF(FN.MULTI.BRANCH.INTERNAL.ACCOUNT,F.MULTI.BRANCH.INTERNAL.ACCOUNT)

    Y.INT.ACC.ID = ''
    Y.ENQUIRY.ID = 'REDO.ENQ.PAY.LIST.ACC'
    Y.VERSION.NAME = ''


RETURN
*---------------------------------------------------------------------------------
PROCESS:
*---------------------------------------------------------------------------------
    Y.PRINCIPAL     = R.NEW(AZ.PRINCIPAL)
    GOSUB READ.APP
    IF R.NEW(AZ.REPAY.ACCOUNT) EQ '' THEN
        GOSUB CHECK.SOURCE.OF.FUND
    END ELSE
        Y.DEBIT.ACC = R.NEW(AZ.REPAY.ACCOUNT)

        IF Y.DEBIT.ACC NE '' AND R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.METHOD.PAY,1> THEN
            AF = AZ.REPAY.ACCOUNT
            ETEXT='EB-REDO.REINV.PAY'
            CALL STORE.END.ERROR
            GOSUB END1
        END
    END

RETURN
*---------------------------------------------------------------------------------
READ.APP:
*---------------------------------------------------------------------------------

    CALL CACHE.READ(FN.AZ.PRODUCT.PARAMETER,R.NEW(AZ.ALL.IN.ONE.PRODUCT),R.APP,APP.ERR)
    Y.TRANS.DAYS = R.APP<AZ.APP.LOCAL.REF,POS.L.AZP.TRAN.DAYS>

    CALL CACHE.READ(FN.ENQUIRY, Y.ENQUIRY.ID, R.ENQ, EN.ERR)  ;*AUTO R22 CODE CONVERSION
    FINDSTR 'VERSION' IN  R.ENQ<ENQ.FIXED.SELECTION> SETTING POS.FM,POS.VM THEN
        Y.VERSION.NAME = FIELD(R.ENQ<ENQ.FIXED.SELECTION,POS.VM>,' ',3)
    END
    IF Y.VERSION.NAME THEN
        CALL CACHE.READ(FN.MULTI.BRANCH.INTERNAL.ACCOUNT,'SYSTEM',R.MULTI.BRANCH,F.MULTI.BRANCH.INTERNAL.ACCOUNT)
        LOCATE Y.VERSION.NAME IN R.MULTI.BRANCH<REDO.BR.ACCT.VERSION,1> SETTING VER.POS THEN
            Y.INT.ACC.ID = R.MULTI.BRANCH<REDO.BR.ACCT.ACCOUNT,VER.POS>
        END
    END


RETURN
*---------------------------------------------------------------------------------
CHECK.SOURCE.OF.FUND:
*---------------------------------------------------------------------------------

    Y.METHOD.OF.PAY = R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.METHOD.PAY>
    Y.AZ.AMOUNT     = R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.AMOUNT>
    Y.L.AZ.SRC.FUND = R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.SRC.FUND>
    Y.L.AZ.DEBIT.ACC= R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.DEBIT.ACC>
    CHANGE @SM TO @VM IN Y.METHOD.OF.PAY
    CHANGE @SM TO @VM IN Y.AZ.AMOUNT
    CHANGE @SM TO @VM IN Y.L.AZ.SRC.FUND
    CHANGE @SM TO @VM IN Y.L.AZ.DEBIT.ACC

    Y.SOURCE.OF.FUND.CNT = DCOUNT(Y.METHOD.OF.PAY,@VM)
    Y.VAR1=1
    LOOP
    WHILE Y.VAR1 LE Y.SOURCE.OF.FUND.CNT
        GOSUB ERROR.CHECK
        IF Y.SOURCE.OF.FUND.CNT GT 1 THEN
            IF Y.AZ.AMOUNT<1,Y.VAR1> EQ '' THEN
                AF=AZ.LOCAL.REF
                AV=POS.L.AZ.AMOUNT
                AS=Y.VAR1
                ETEXT='EB-AMOUNT.MANDATORY'
                CALL STORE.END.ERROR
                GOSUB END1
            END
            C.AMT=1
            LOOP
            WHILE C.AMT LE Y.SOURCE.OF.FUND.CNT
                GOSUB CHECK.AMOUNT
                C.AMT += 1 ;*AUTO R22 CODE CONVERSION
            REPEAT
        END
        Y.VAR1 += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT

*This part is handled in another routine based on the currency-REDO.V.INP.CHECK.AMT
* IF Y.TOT.AMT NE Y.PRINCIPAL THEN
*     AF = AZ.LOCAL.REF
*     AV = POS.L.AZ.AMOUNT
*     AS = 1
*     ETEXT="EB-AZ.AMOUNT.TOTAL"
*     CALL STORE.END.ERROR
*    GOSUB END1
*
*END
    R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.IN.TRANSIT> = ''

    LOCATE 'CHEQUE.DEPOSIT' IN Y.METHOD.OF.PAY<1,1> SETTING POS1 THEN
        IF Y.TRANS.DAYS GT 0 THEN
            R.NEW(AZ.LOCAL.REF)<1,POS.L.AZ.IN.TRANSIT> = 'YES'
        END
    END


RETURN
*---------------------------------------------------------
ERROR.CHECK:
*---------------------------------------------------------


    IF Y.METHOD.OF.PAY<1,Y.VAR1> EQ "" THEN
        AF = AZ.LOCAL.REF
        AV = POS.L.AZ.METHOD.PAY
        AS = Y.VAR1
        ETEXT="EB-AZSET"
        CALL STORE.END.ERROR
        GOSUB END1

    END
    IF Y.AZ.AMOUNT<1,Y.VAR1> EQ "" THEN
        AF = AZ.LOCAL.REF
        AV = POS.L.AZ.AMOUNT
        AS = Y.VAR1
        ETEXT="EB-AZSET"
        CALL STORE.END.ERROR
        GOSUB END1
    END
*    IF Y.L.AZ.SRC.FUND<1,Y.VAR1> EQ "" THEN
*        AF = AZ.LOCAL.REF
*        AV = POS.L.AZ.SRC.FUND
*        AS = Y.VAR1
*        ETEXT="EB-AZSET"
*        CALL STORE.END.ERROR
*        GOSUB END1
*    END

    IF (Y.METHOD.OF.PAY<1,Y.VAR1> EQ "FROM.CUST.ACC" OR Y.METHOD.OF.PAY<1,Y.VAR1> EQ "FROM.INT.ACC" OR Y.METHOD.OF.PAY<1,Y.VAR1> EQ "FROM.NOST.ACC") AND Y.L.AZ.DEBIT.ACC<1,Y.VAR1> EQ "" THEN
        AF = AZ.LOCAL.REF
        AV = POS.L.AZ.DEBIT.ACC
        AS = Y.VAR1
        ETEXT="EB-AZSET"
        CALL STORE.END.ERROR
        GOSUB END1
    END




    IF (Y.METHOD.OF.PAY<1,Y.VAR1> NE "FROM.CUST.ACC" AND Y.METHOD.OF.PAY<1,Y.VAR1> NE "FROM.INT.ACC" AND Y.METHOD.OF.PAY<1,Y.VAR1> NE "FROM.NOST.ACC") AND Y.METHOD.OF.PAY<1,Y.VAR1> NE "" AND Y.L.AZ.DEBIT.ACC<1,Y.VAR1> NE "" THEN
        AF = AZ.LOCAL.REF
        AV = POS.L.AZ.DEBIT.ACC
        AS = Y.VAR1
        ETEXT = 'EB-REDO.ACC.NOT.ALLOW'
        CALL STORE.END.ERROR
        GOSUB END1

    END



    CALL F.READ(FN.ACCOUNT,Y.L.AZ.DEBIT.ACC<1,Y.VAR1>,R.AZ.DEBIT.ACC,F.ACCOUNT,AZ.DB.ERR)
    IF R.AZ.DEBIT.ACC THEN
        Y.AZ.DB.CURRENCY=R.AZ.DEBIT.ACC<AC.CURRENCY>
*  IF Y.AZ.DB.CURRENCY NE R.NEW(AZ.CURRENCY) THEN
*      AF=AZ.LOCAL.REF
*      AV=POS.L.AZ.DEBIT.ACC
*      AS = Y.VAR1
*      ETEXT="EB-AZ.DEB.ACC"
*      CALL STORE.END.ERROR
*      GOSUB END1
*  END
    END
    IF Y.METHOD.OF.PAY<1,Y.VAR1> EQ "FROM.INT.ACC" THEN
        IF Y.INT.ACC.ID THEN
            IF Y.L.AZ.DEBIT.ACC<1,Y.VAR1> NE Y.INT.ACC.ID THEN
                AF=AZ.LOCAL.REF
                AV = POS.L.AZ.DEBIT.ACC
                AS = Y.VAR1
                ETEXT = 'EB-REDO.MISS.INT.ACC'
                CALL STORE.END.ERROR
                GOSUB END1
            END

        END

    END

* Fix for PACS00253693 [DECEASED ACCOUNT CHECK]

    IF Y.METHOD.OF.PAY<1,Y.VAR1> EQ "FROM.CUST.ACC" AND Y.L.AZ.DEBIT.ACC<1,Y.VAR1> NE '' THEN

        GET.ACCT.STATUS.VAL = R.AZ.DEBIT.ACC<AC.LOCAL.REF,GET.ACCT.STATUS.POS>

        IF GET.ACCT.STATUS.VAL EQ 'DECEASED' THEN
            AF = AZ.LOCAL.REF
            AV = POS.L.AZ.DEBIT.ACC
            AS = Y.VAR1
            ETEXT = "EB-REDO.AC.DECEASED"
            CALL STORE.END.ERROR
        END
    END

* End of Fix

    Y.ONE.AMT = Y.AZ.AMOUNT<1,Y.VAR1>
    Y.TOT.AMT+=Y.ONE.AMT

RETURN
*--------------------------
CHECK.AMOUNT:
*--------------------------
    IF C.AMT NE Y.VAR1 THEN
        IF Y.METHOD.OF.PAY<1,Y.VAR1> EQ Y.METHOD.OF.PAY<1,C.AMT> AND Y.L.AZ.DEBIT.ACC<1,Y.VAR1> EQ Y.L.AZ.DEBIT.ACC<1,C.AMT> THEN
            AF=AZ.LOCAL.REF
            AV=POS.L.AZ.METHOD.PAY
            AS=C.AMT
            ETEXT ='EB-TYPE.MORE.THEN.ONCE.ENTER'
            CALL STORE.END.ERROR
            GOSUB END1
        END
    END
RETURN
*--------------------------
*------
END1:
*------

END
