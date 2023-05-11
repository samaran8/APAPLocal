* @ValidationCode : MjotMTU4NDE0NTY4MjpDcDEyNTI6MTY4MDYwNTAwODQ1NzpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 04 Apr 2023 16:13:28
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

SUBROUTINE REDO.B2.FT.DATA.AUTHORISE
*-----------------------------------------------------------------------------
* ********************************
* Subroutine Type : AUTHORIZATION ROUTINE
* Attached to     : REDO.B2.FT.DATA,INGRESO
* Attached as     : AUTHORIZATION ROUTINE
* Primary Purpose : Trigger OFS message FT for payment insurance companies
*
* Incoming:
* ---------
* N/A
**
* Outgoing:
* ---------
*
*
* Error Variables:
* N/A
*
*-----------------------------------------------------------------------------
* Modification History:
*
* Development for : APAP
* Development by  : Santiago Jijon - TAM Latin America
* Date            : Ago, 15 - 2012
* Modify by       :
* Modify date     :
* Notes           :
* DATE              WHO                REFERENCE                 DESCRIPTION
* 04-APR-2023     Conversion tool   R22 Auto conversion   	VM to @VM, if condition added, F.READ to CACHE.READ, I to I.VAR, F.RULE to R.RULE
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes

*-----------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AC.ALLOCATION.RULE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.ACCOUNT
    $INSERT I_System

    $INSERT I_F.REDO.B2.FT.DATA
    $INSERT I_F.REDO.B2.FT.PARAMETERS
    $INSERT I_F.REDO.BRANCH.INT.ACCT.PARAM
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.CATEG.ENTRY

    GOSUB INITIALISE
    GOSUB GET.LOCAL.FIELDS
    GOSUB PROCESS
* GOSUB PROCESS.OFS
* GOSUB UPDATE.PARAM

***<region name = INITIALISE>
*---------------------------------------------------------------------------------------------------------
INITIALISE:

    FFTT.REQUEST = ''
    Y.DEBIT.ACC = ''

    FN.RULE = 'F.AC.ALLOCATION.RULE'
    F.RULE = ''
    R.RULE = ''
    CALL OPF(FN.RULE, F.RULE)

    FN.B2FT = 'F.REDO.B2.FT.DATA'
    F.B2FT = ''
    R.B2FT = ''
    CALL OPF(FN.B2FT,F.B2FT)

    FN.PARAM = 'F.REDO.B2.FT.PARAMETERS'
    F.PARAM = ''
    R.PARAM = ''
    CALL OPF(FN.PARAM,F.PARAM)

    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    R.ACC = ''
    CALL OPF(FN.ACC,F.ACC)

    FN.REDO.BRANCH.INT.ACCT.PARAM = 'F.REDO.BRANCH.INT.ACCT.PARAM'
    F.REDO.BRANCH.INT.ACCT.PARAM = ''

RETURN
***</region>

***<region name = PROCESS>
*---------------------------------------------------------------------------------------------------------
PROCESS:


    Y.RULE = R.NEW(PAY.DAT.CHARGE)
    CALL CACHE.READ(FN.RULE, Y.RULE, R.RULE, YERR)
    Y.TOTTARGET = DCOUNT(R.RULE<AC.AR.OPP.TARGET>, @VM)
    FOR I.VAR = 1 TO Y.TOTTARGET
        Y.CAMPO = R.RULE<AC.AR.OPP.TARGET><1,I.VAR>
        Y.DETCAMPO = FIELD(Y.CAMPO, '*', 1)
        IF Y.DETCAMPO EQ 'AC' THEN
            Y.DEBIT.ACC = FIELD(Y.CAMPO, '*', 2)
            Y.DEBIT.ACC.DUP = Y.DEBIT.ACC
            EXIT
        END
    NEXT


    Y.POLICY.TYPE = System.getVariable("CURRENT.POL")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ; * R22 Auto conversion
        Y.POLICY.TYPE = ""
    END
    CALL F.READ(FN.PARAM,Y.POLICY.TYPE,R.PARAM,F.PARAM,YERR)
*    Y.CR.TYPE = R.PARAM<PAY.PAR.CHARGE.TYPE><1,1>

*  FFTT.REQUEST<FT.CHARGE.TYPE,1,1> = R.PARAM<PAY.PAR.CHARGE.TYPE><1,1>
*  FFTT.REQUEST<FT.CHARGE.AMT,1,1> = 'DOP ': R.NEW(PAY.DAT.APAP.COMMISION)

*  IF R.NEW(PAY.DAT.NET.COMMISION) GT 0 THEN
*      FFTT.REQUEST<FT.CHARGE.TYPE,2,1> = R.PARAM<PAY.PAR.CHARGE.TYPE><1,2>
*      FFTT.REQUEST<FT.CHARGE.AMT,2,1> = 'DOP ': R.NEW(PAY.DAT.NET.COMMISION)
*  END


    IF R.NEW(PAY.DAT.PAYMENT.TYPE) EQ 'CREDITO' THEN
        Y.DEBIT.ACC = Y.DEBIT.ACC.DUP
        Y.DEBIT.ACC.N = R.NEW(PAY.DAT.ACC.CREDIT)
        Y.PAY.VAL = R.NEW(PAY.DAT.PAYMENT.VALUE)
        GOSUB FORM.OFS
        Y.VERSION = 'FUNDS.TRANSFER,B2'
        GOSUB PROCESS.OFS
        GOSUB PROCESS.COMM
    END ELSE
        IF R.NEW(PAY.DAT.PAYMENT.TYPE) EQ 'CHEQUE' THEN
            Y.PAR.ID = 'SYSTEM'
            CALL CACHE.READ(FN.REDO.BRANCH.INT.ACCT.PARAM,Y.PAR.ID,R.REDO.BRANCH.INT.ACCT.PARAM,PAR.ERR)
            Y.DEBIT.ACC.N = R.REDO.BRANCH.INT.ACCT.PARAM<BR.INT.ACCT.INSUR.CHQ.ACCT>
            Y.PAY.VAL = R.NEW(PAY.DAT.PAYMENT.VALUE)
            GOSUB FORM.OFS
            Y.VERSION = 'FUNDS.TRANSFER,B2'
            GOSUB PROCESS.OFS
            GOSUB PROCESS.COMM
        END
    END

RETURN

PROCESS.COMM:

    IF Y.POLICY.TYPE NE 'FHA' THEN
        Y.TAX.AC = R.PARAM<PAY.PAR.TAX.INT.AC>
        Y.DEBIT.ACC.N = Y.TAX.AC
        Y.PAY.VAL = R.NEW(PAY.DAT.ITBIS.VALUE)
        GOSUB FORM.OFS
        Y.VERSION = 'FUNDS.TRANSFER,B2.C'
        GOSUB PROCESS.OFS

        Y.PL.CAT = 'PL':R.PARAM<PAY.PAR.PL.COMMISSION>
        Y.COM.AMT = R.NEW(PAY.DAT.APAP.COMMISION)
        GOSUB COMMISION.ENTRY
    END

RETURN

COMMISION.ENTRY:

    Y.DEBIT.ACC.N = Y.PL.CAT
    Y.PAY.VAL = Y.COM.AMT
    GOSUB FORM.OFS
    Y.VERSION = 'FUNDS.TRANSFER,B2.C'
    GOSUB PROCESS.OFS

RETURN

FORM.OFS:

    FFTT.REQUEST<FT.TRANSACTION.TYPE> = 'AC'
    IF Y.POLICY.TYPE NE 'FHA' THEN
        FFTT.REQUEST<FT.DEBIT.ACCT.NO> = Y.DEBIT.ACC
    END ELSE
        FFTT.REQUEST<FT.DEBIT.ACCT.NO> = R.PARAM<PAY.PAR.FHA.INT.AC>
    END
    FFTT.REQUEST<FT.DEBIT.CURRENCY> = 'DOP'         ;* POR EL MOMENTO VA DOP, hay que preguntar a REINALDO como obtener la moneda
    FFTT.REQUEST<FT.DEBIT.AMOUNT> = Y.PAY.VAL
    FFTT.REQUEST<FT.DEBIT.VALUE.DATE> = TODAY
    FFTT.REQUEST<FT.CREDIT.ACCT.NO> = Y.DEBIT.ACC.N
    FFTT.REQUEST<FT.ORDERING.BANK> = 'BANK'
    FFTT.REQUEST<FT.LOCAL.REF,Y.L.COM> = 'POL NUM: ': R.NEW(PAY.DAT.SEN.POLICY.NUMBER)
*  FFTT.REQUEST<FT.CHARGE.CODE> = 'CREDIT LESS CHARGE'

    CALL F.READ(FN.ACC,Y.DEBIT.ACC,R.ACC,F.ACC,YERR)
    FFTT.REQUEST<FT.PROFIT.CENTRE.DEPT> = R.ACC<AC.ACCOUNT.OFFICER>

    FFTT.REQUEST<FT.LOCAL.REF,Y.POLICY.TYPE.POS> = R.NEW(PAY.DAT.INS.POLICY.TYPE)
    FFTT.REQUEST<FT.LOCAL.REF,Y.CLS.FT> = R.NEW(PAY.DAT.INS.CLAS.POLICY)
    FFTT.REQUEST<FT.LOCAL.REF,Y.INS.COMPANY.POS> = R.NEW(PAY.DAT.INS.COMPANY)
    FFTT.REQUEST<FT.LOCAL.REF,Y.CLOSE.BAL.DATE.POS> = R.NEW(PAY.DAT.CLOSING.DATE)
* FFTT.REQUEST<FT.CREDIT.THEIR.REF> = R.NEW(PAY.DAT.INS.POLICY.TYPE):'-':R.NEW(PAY.DAT.INS.CLAS.POLICY):'-':R.NEW(PAY.DAT.INS.COMPANY):'-':R.NEW(PAY.DAT.CLOSING.DATE)

RETURN
***</region>

***<region name= PROCESS.OFS>
*-----------------------------------------------------------------------------------------------------------
PROCESS.OFS:
    Y.OFS.SOURCE.ID = 'B2.FT'
    Y.APPLICATION   = 'FUNDS.TRANSFER'
*  Y.VERSION       = 'FUNDS.TRANSFER,B2'
    Y.OFS.MSG.VAL   = ''
    Y.OFS.INFO      = ''
    OFS.MSG.ID = ''
    OFS.ERR = ''


    CALL OFS.BUILD.RECORD(Y.APPLICATION,'I','PROCESS',Y.VERSION,'',0,'',FFTT.REQUEST,Y.OFS.MSG.VAL)


    CALL OFS.POST.MESSAGE(Y.OFS.MSG.VAL,OFS.MSG.ID,Y.OFS.SOURCE.ID,OFS.ERR)

RETURN
***</region>

***<region name= UPDATE.PARAM>
*-----------------------------------------------------------------------------------------------------------
UPDATE.PARAM:

    Y.NEWCLOSDATE = R.NEW(PAY.DAT.CLOSING.DATE)

*  CALL F.READ(FN.PARAM,'SYSTEM',R.PARAM,F.PARAM,YERR) ;*Tus Start
    CALL CACHE.READ(FN.PARAM,'SYSTEM',R.PARAM,YERR) ; * Tus End
    R.PARAM<PAY.PAR.FECHA.CORTE> = Y.NEWCLOSDATE
    CALL F.WRITE(FN.PARAM,'SYSTEM',R.PARAM)
RETURN
***</region>

***<region name= GET.LOCAL.FIELDS>
*-----------------------------------------------------------------------------------------------------------
GET.LOCAL.FIELDS:
    LOC.REF.POS=0
    LOC.REF.APPL="FUNDS.TRANSFER"
    LOC.REF.FIELDS = "INS.POLICY.TYPE": @VM :"INS.COMPANY": @VM :"CLOSE.BAL.DATE": @VM : "L.COMMENTS" : @VM : "L.FT.CONCEPT"
    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,LOC.REF.POS)
    Y.POLICY.TYPE.POS     = LOC.REF.POS<1,1>
    Y.INS.COMPANY.POS     = LOC.REF.POS<1,2>
    Y.CLOSE.BAL.DATE.POS  = LOC.REF.POS<1,3>
    Y.L.COM = LOC.REF.POS<1,4>
    Y.CLS.FT = LOC.REF.POS<1,5>
RETURN
*** </region>


END
