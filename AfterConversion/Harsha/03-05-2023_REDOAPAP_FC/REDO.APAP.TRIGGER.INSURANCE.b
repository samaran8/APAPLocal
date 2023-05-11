* @ValidationCode : MjotMTQxODQxMDcwMzpDcDEyNTI6MTY4MjUwMjY3MTY5NDpJVFNTOi0xOi0xOjEzMjA6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 26 Apr 2023 15:21:11
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 1320
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.TRIGGER.INSURANCE

*-----------------------------------------------------------------------------------------------------------
* Description
*
* This routine reads the values from the application APAP.H.INSURANCE.DETAILS and triggers an
* AA Activity 'LENDING-RENEGOTIATE-ARRANGEMENT' which will update the Insurance details given as part of this
* Application
*
* This routine is attached to as a AUTH routine to the version used by APAP.H.INSURANCE.DETAILS so that the AA
* activity message is posted in OFS.MESSAGE.QUEUE
*
*-----------------------------------------------------------------------------------------------------------
*
* Modification History
* BY: Santiago Jijon
* DATE: 2011/11/09
* PACS00867267     - GOPALA KRISHNAN R      - System should not duplicate the property under the payment schedule tab.
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*18-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  FM to @FM , VM to @VM , SM to @SM, CHAR to CHARX
*18-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------



*-----------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.APAP.H.INSURANCE.DETAILS
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.AA.PAYMENT.SCHEDULE
    $INSERT I_F.AA.CHARGE

*-----------------------------------------------------------------------------------------------------------
MAIN.LOGIC:

    GOSUB INITIALISE

    GOSUB PROCESS.OFS

RETURN
*-----------------------------------------------------------------------------------------------------------

INITIALISE:

    AAA.REQUEST = ''

    PROPERTY.CLASS = ''

    FN.AA.ACCOUNT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS = ''
    R.AA.ACCOUNT.DETAILS = ''
    CALL OPF (FN.AA.ACCOUNT.DETAILS, F.AA.ACCOUNT.DETAILS)

    ARR.ID = R.NEW(INS.DET.ASSOCIATED.LOAN)

    CALL F.READ(FN.AA.ACCOUNT.DETAILS, ARR.ID, R.AA.ACCOUNT.DETAILS, F.AA.ACCOUNT.DETAILS, READ.ERR)

    FN.CHARGE = 'F.AA.ARR.CHARGE'
    F.CHARGE  = ''
    R.CHARGE  = ''
    Y.CHARGE  = ''
    CALL OPF(FN.CHARGE,F.CHARGE)

    FN.PAY.SCH = 'F.AA.ARR.PAYMENT.SCHEDULE'
    F.PAY.SCH  = ''
    R.PAY.SCH  = ''
    Y.PAY.SCH  = ''
    CALL OPF(FN.PAY.SCH,F.PAY.SCH)


RETURN
*-----------------------------------------------------------------------------------------------------------
PROCESS.OFS:

    AAA.REQUEST<AA.ARR.ACT.ARRANGEMENT> = R.NEW(INS.DET.ASSOCIATED.LOAN)
    AAA.REQUEST<AA.ARR.ACT.ACTIVITY> = 'LENDING-RENEGOTIATE-OPEN'
    AAA.REQUEST<AA.ARR.ACT.EFFECTIVE.DATE> = TODAY

    GOSUB FORM.CHARGE
    GOSUB INCLUDE.CHARGE

    APP.NAME = 'AA.ARRANGEMENT.ACTIVITY'
    IN.FUNCTION = 'I'
    VERSION.NAME = 'AA.ARRANGEMENT.ACTIVITY,ZERO.AUTH'


    CALL OFS.BUILD.RECORD(APP.NAME, IN.FUNCTION, "PROCESS", VERSION.NAME, "", "0", AAA.ID, AAA.REQUEST, PROCESS.MSG)      ;*Form the OFS Message

    GOSUB TRIGGER.OFS ;* Post the OFS in the queue

*    CALL System.setVariable("CURRENT.SCH",Y.SCHEDULE)
*
*    NEW.TASK = 'AA.ARRANGEMENT.ACTIVITY,AA.B02 I F3'
*    CALL EB.SET.NEW.TASK(NEW.TASK)

RETURN

*-----------------------------------------------------------------------------------------------------------

FORM.CHARGE:

* This forms the OFS message request for CHARGE property

    Y.ID.CMD = ARR.ID:'-': R.NEW(INS.DET.CHARGE)<1,1>

    PROPERTY.CLASS = "CHARGE"
    GOSUB GET.PROPERTY

    AA.PROPERTY = R.NEW(INS.DET.CHARGE)<1,1>
    AAA.REQUEST<AA.ARR.ACT.PROPERTY,-1> = AA.PROPERTY         ;* Charge property to be updated as part of Insurance

*    CALL F.READ(FN.CHARGE, ARR.ID, R.CHARGE, F.CHARGE, Y.ERR)
*    SEL.CMD = 'SELECT ': FN.CHARGE :' WITH @ID LIKE ': Y.ID.CMD : '... AND ACTIVITY EQ LENDING-RENEGOTIATE-OPEN BY-DSND DATE.TIME'
*    CALL EB.READLIST(SEL.CMD,Y.LIST,'',NO.OF.REG,RET.CODE)
*    IF NO.OF.REG GT 0 THEN
*        GOSUB GET.LOCAL.FIELDS
*        LOOP
*            REMOVE Y.CHARGE FROM Y.LIST SETTING POS
*        WHILE Y.CHARGE:POS
*            CALL F.READ(FN.CHARGE,Y.CHARGE,R.CHARGE,F.CHARGE,Y.ERR)
*            GOSUB GET.VALUES.LOCAL.FIELDS
*            Y.TOT.POL.NUMBER = DCOUNT(Y.POL.NUMBER, SM)
*            FOR II = 1 TO Y.TOT.POL.NUMBER
*                GOSUB ARMA.OFS
*            NEXT
*            II = Y.TOT.POL.NUMBER + 1
*            GOSUB ARMA.OFS.NUEVO
*            BREAK
*        REPEAT
*    END ELSE
*        II = 1
*        GOSUB ARMA.OFS.NUEVO
*    END
    idArrangementComp = ARR.ID
    idPropertyClass = PROPERTY.CLASS
    idProperty = AA.PROPERTY
    effectiveDate = ''
    returnIds = ''
    returnConditions = ''
    returnError = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(idArrangementComp, idPropertyClass, idProperty, effectiveDate, returnIds, returnConditions, returnError)

    Y.POLICYTYPE = returnConditions<1,31,16>
    MMARK = CHARX(251) ;*R22 AUTO CODE CONVERSION
    Y.POLICYTYPE1 = CHANGE(Y.POLICYTYPE, MMARK , @SM )
    II = DCOUNT(Y.POLICYTYPE1, @SM) + 1

    GOSUB ARMA.OFS.NUEVO
RETURN

*-----------------------------------------------------------------------------------------------------------
GET.LOCAL.FIELDS:
    LOC.REF.POS=0
    LOC.REF.APPL="AA.PRD.DES.CHARGE"
    LOC.REF.FIELDS = "POLICY.TYPE": @VM :"POLICY.CLASS": @VM :"INS.COMPANY": @VM :"POL.NUMBER": @VM :"SNR.PLY.NUMBER": @VM :"MANAG.TYPE": @VM
    LOC.REF.FIELDS := "MNTY.POLICY.AMT": @VM :"EXTRA.POLICY": @VM :"TOT.MNT.PLY.AMT": @VM :"TOT.PLY.AMT": @VM :"STATUS.POLICY": @VM : "EXPIRY.DATE"
    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,LOC.REF.POS)
    Y.POLICY.TYPE.POS     = LOC.REF.POS<1,1>
    Y.POLICY.CLASS.POS    = LOC.REF.POS<1,2>
    Y.INS.COMPANY.POS     = LOC.REF.POS<1,3>
    Y.POL.NUMBER.POS      = LOC.REF.POS<1,4>
    Y.SNR.PLY.NUMBER.POS  = LOC.REF.POS<1,5>
    Y.MANAG.TYPE.POS      = LOC.REF.POS<1,6>
    Y.MNTY.POLICY.AMT.POS = LOC.REF.POS<1,7>
    Y.EXTRA.POLICY.POS    = LOC.REF.POS<1,8>
    Y.TOT.MNT.PLY.AMT.POS = LOC.REF.POS<1,9>
    Y.TOT.PLY.AMT.POS     = LOC.REF.POS<1,10>
    Y.STATUS.POLICY.POS   = LOC.REF.POS<1,11>
    Y.EXPIRY.DATE.POS     = LOC.REF.POS<1,12>
RETURN

*-----------------------------------------------------------------------------------------------------------
GET.VALUES.LOCAL.FIELDS:

    Y.POLICY.TYPE     = R.CHARGE<AA.CHG.LOCAL.REF,Y.POLICY.TYPE.POS>
    Y.POLICY.CLASS    = R.CHARGE<AA.CHG.LOCAL.REF,Y.POLICY.CLASS.POS>
    Y.INS.COMPANY     = R.CHARGE<AA.CHG.LOCAL.REF,Y.INS.COMPANY.POS>
    Y.POL.NUMBER      = R.CHARGE<AA.CHG.LOCAL.REF,Y.POL.NUMBER.POS>
    Y.SNR.PLY.NUMBER  = R.CHARGE<AA.CHG.LOCAL.REF,Y.SNR.PLY.NUMBER.POS>
    Y.MANAG.TYPE      = R.CHARGE<AA.CHG.LOCAL.REF,Y.MANAG.TYPE.POS>
    Y.MNTY.POLICY.AMT = R.CHARGE<AA.CHG.LOCAL.REF,Y.MNTY.POLICY.AMT.POS>
    Y.EXTRA.POLICY    = R.CHARGE<AA.CHG.LOCAL.REF,Y.EXTRA.POLICY.POS>
    Y.TOT.MNT.PLY.AMT = R.CHARGE<AA.CHG.LOCAL.REF,Y.TOT.MNT.PLY.AMT.POS>
    Y.TOT.PLY.AMT     = R.CHARGE<AA.CHG.LOCAL.REF,Y.TOT.PLY.AMT.POS>
    Y.STATUS.POLICY   = R.CHARGE<AA.CHG.LOCAL.REF,Y.STATUS.POLICY.POS>
    Y.EXPIRY.DATE     = R.CHARGE<AA.CHG.LOCAL.REF,Y.EXPIRY.DATE.POS>

RETURN

*-----------------------------------------------------------------------------------------------------------
ARMA.OFS:
    IF Y.POL.NUMBER<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'POL.NUMBER:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.POL.NUMBER<1,1,II>
    END

    IF Y.SNR.PLY.NUMBER<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'SNR.PLY.NUMBER:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.SNR.PLY.NUMBER<1,1,II>
    END

    IF Y.POLICY.TYPE<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'POLICY.TYPE:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.POLICY.TYPE<1,1,II>
    END

    IF Y.POLICY.CLASS<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'POLICY.CLASS:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.POLICY.CLASS<1,1,II>
    END

    IF Y.MANAG.TYPE<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'MANAG.TYPE:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.MANAG.TYPE<1,1,II>
    END

    IF Y.MNTY.POLICY.AMT<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'MNTY.POLICY.AMT:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.MNTY.POLICY.AMT<1,1,II>
    END

    IF Y.EXTRA.POLICY<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'EXTRA.POLICY:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.EXTRA.POLICY<1,1,II>
    END

    IF Y.TOT.MNT.PLY.AMT<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'TOT.MNT.PLY.AMT:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.TOT.MNT.PLY.AMT<1,1,II>
    END

    IF Y.TOT.PLY.AMT<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'TOT.PLY.AMT:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.TOT.PLY.AMT<1,1,II>
    END

    IF Y.STATUS.POLICY<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'STATUS.POLICY:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.STATUS.POLICY<1,1,II>
    END

    IF Y.INS.COMPANY<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'INS.COMPANY:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.INS.COMPANY<1,1,II>
    END

    IF Y.EXPIRY.DATE<1,1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'EXPIRY.DATE:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = Y.EXPIRY.DATE<1,1,II>
    END

RETURN
*-----------------------------------------------------------------------------------------------------------
ARMA.OFS.NUEVO:


    IF R.NEW(INS.DET.POLICY.NUMBER) NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'POL.NUMBER:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.POLICY.NUMBER)
    END

    IF R.NEW(INS.DET.SEN.POLICY.NUMBER) NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'SNR.PLY.NUMBER:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.SEN.POLICY.NUMBER)
    END

    IF R.NEW(INS.DET.INS.POLICY.TYPE) NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'POLICY.TYPE:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.INS.POLICY.TYPE)
    END

    IF R.NEW(INS.DET.CLASS.POLICY) NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'POLICY.CLASS:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.CLASS.POLICY)
    END

    IF R.NEW(INS.DET.MANAGEMENT.TYPE) NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'MANAG.TYPE:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.MANAGEMENT.TYPE)
    END

    IF R.NEW(INS.DET.MON.POL.AMT)<1,1> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'MNTY.POLICY.AMT:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.MON.POL.AMT)<1,1>
    END

    IF R.NEW(INS.DET.EXTRA.AMT)<1,1> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'EXTRA.POLICY:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.EXTRA.AMT)<1,1>
    END

    IF R.NEW(INS.DET.MON.TOT.PRE.AMT)<1,1> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'TOT.MNT.PLY.AMT:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.MON.TOT.PRE.AMT)<1,1>
    END

    IF R.NEW(INS.DET.TOTAL.PRE.AMT) NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'TOT.PLY.AMT:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.TOTAL.PRE.AMT)
    END

    IF R.NEW(INS.DET.POLICY.STATUS) NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'STATUS.POLICY:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.POLICY.STATUS)
    END

    IF R.NEW(INS.DET.INS.COMPANY) NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'INS.COMPANY:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.INS.COMPANY)
    END

    IF R.NEW(INS.DET.POL.EXP.DATE) NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,1,-1> = 'EXPIRY.DATE:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,1,-1> = R.NEW(INS.DET.POL.EXP.DATE)
    END

RETURN


*-----------------------------------------------------------------------------------------------------------
INCLUDE.CHARGE:

    IF R.NEW(INS.DET.MANAGEMENT.TYPE) EQ 'INCLUIR EN CUOTA' THEN
        PROPERTY.CLASS = 'PAYMENT.SCHEDULE'
        GOSUB GET.PROPERTY

        AAA.REQUEST<AA.ARR.ACT.PROPERTY,-1> = AA.PROPERTY

        Y.ID.CMD = ARR.ID
*        SEL.CMD = 'SELECT ': FN.PAY.SCH :' WITH ID.COMP.1 EQ ': Y.ID.CMD :' BY-DSND DATE.TIME'
*        CALL EB.READLIST(SEL.CMD,Y.LIST,'',NO.OF.REG,RET.CODE)
*        IF NO.OF.REG GT 0 THEN
*            LOOP
*                REMOVE Y.PAY.SCH FROM Y.LIST SETTING POS
*            WHILE Y.PAY.SCH:POS
*                CALL F.READ(FN.PAY.SCH,Y.PAY.SCH,R.PAY.SCH,F.PAY.SCH,Y.ERR)
*                Y.TOT.PAY.TYPE = DCOUNT(R.PAY.SCH<AA.PS.PAYMENT.TYPE>, VM)
*                FOR II = 1 TO Y.TOT.PAY.TYPE
*                    GOSUB ARMA.OFS.SCH
*                NEXT
*                II = Y.TOT.PAY.TYPE + 1
*                BREAK
*            REPEAT
*        END ELSE
*            II = 1
*        END
*        GOSUB ARMA.OFS.NUEVO.SCH
        idArrangementComp = ARR.ID
        idPropertyClass = PROPERTY.CLASS
        idProperty = AA.PROPERTY
        effectiveDate = ''
        returnIds = ''
        returnConditions = ''
        returnError = ''
        CALL AA.GET.ARRANGEMENT.CONDITIONS(idArrangementComp, idPropertyClass, idProperty, effectiveDate, returnIds, returnConditions, returnError)
        Y.PAYMENTS = returnConditions<1,10>
        II = DCOUNT(Y.PAYMENTS,@SM) + 1
* PACS00867267 - START
        PAY.TYPE.VAR = 0
        R.PAY.SCHEDULE = RAISE(returnConditions)
        Y.AA.PAY.TYPE = RAISE(R.PAY.SCHEDULE<AA.PS.PAYMENT.TYPE>)
        Y.AA.PAY.PROPERTY = RAISE(R.PAY.SCHEDULE<AA.PS.PROPERTY>)
        CHANGE @VM TO @FM IN Y.AA.PAY.TYPE
* PACS00867267 - END
        GOSUB ARMA.OFS.NUEVO.SCH

    END

RETURN
*-----------------------------------------------------------------------------------------------------------
ARMA.OFS.SCH:

    IF R.PAY.SCH<AA.PS.PAYMENT.TYPE><1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.TYPE:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.PAY.SCH<AA.PS.PAYMENT.TYPE><1,II>
    END ELSE
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.TYPE:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = ''
    END

    IF R.PAY.SCH<AA.PS.PAYMENT.METHOD><1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.METHOD:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.PAY.SCH<AA.PS.PAYMENT.METHOD><1,II>
    END ELSE
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.METHOD:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = ''
    END

    IF R.PAY.SCH<AA.PS.PAYMENT.FREQ><1,II> NE '' THEN
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.FREQ:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.PAY.SCH<AA.PS.PAYMENT.FREQ><1,II>
    END ELSE
        AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.FREQ:':II:':1'
        AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = ''
    END

    Y.TOT.PROPERTY = DCOUNT(R.PAY.SCH<AA.PS.PROPERTY><1,II>, @SM)

    FOR JJ = 1 TO Y.TOT.PROPERTY
        IF R.PAY.SCH<AA.PS.PROPERTY><1,II,JJ> NE '' THEN
            AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PROPERTY:':II:':':JJ:':1'
            AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.PAY.SCH<AA.PS.PROPERTY><1,II,JJ>
        END

        IF R.PAY.SCH<AA.PS.DUE.FREQ><1,II,JJ> NE '' THEN
            AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'DUE.FREQ:':II:':':JJ:':1'
            AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.PAY.SCH<AA.PS.DUE.FREQ><1,II,JJ>
        END

        IF R.PAY.SCH<AA.PS.PERCENTAGE><1,II,JJ> NE '' THEN
            AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PERCENTAGE:':II:':':JJ:':1'
            AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.PAY.SCH<AA.PS.PERCENTAGE><1,II,JJ>
        END

    NEXT JJ

    Y.TOT.ST.DATE = DCOUNT(R.PAY.SCH<AA.PS.START.DATE>, @VM)

    FOR JJ = 1 TO Y.TOT.ST.DATE

        IF R.PAY.SCH<AA.PS.START.DATE><1,II,JJ> NE '' THEN
            AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'START.DATE:':II:':':JJ:':1'
            AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.PAY.SCH<AA.PS.START.DATE><1,II,JJ>
        END

        IF R.PAY.SCH<AA.PS.END.DATE><1,II,JJ> NE '' THEN
            AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'END.DATE:':II:':':JJ:':1'
            AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.PAY.SCH<AA.PS.END.DATE><1,II,JJ>
        END

        IF R.PAY.SCH<AA.PS.NUM.PAYMENTS><1,II,JJ> NE '' THEN
            AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'NUM.PAYMENTS:':II:':':JJ:':1'
            AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.PAY.SCH<AA.PS.NUM.PAYMENTS><1,II,JJ>
        END

        IF R.PAY.SCH<AA.PS.ACTUAL.AMT><1,II,JJ> NE '' THEN
            AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'ACTUAL.AMT:':II:':':JJ:':1'
            AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.PAY.SCH<AA.PS.ACTUAL.AMT><1,II,JJ>
        END

    NEXT JJ

RETURN

*-----------------------------------------------------------------------------------------------------------
ARMA.OFS.NUEVO.SCH:

    Y.TOT.PROPERTY = DCOUNT(R.NEW(INS.DET.MON.POL.AMT), @VM)

    FOR JJ = 1 TO Y.TOT.PROPERTY
        IF JJ EQ 1 THEN
            AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.TYPE:':II:':1'
            AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.PAYMENT.TYPE)<1,JJ>
* PACS00867267 - START
            Y.AA.DET.PAY.TYPE = R.NEW(INS.DET.PAYMENT.TYPE)<1,JJ>
            LOCATE Y.AA.DET.PAY.TYPE IN Y.AA.PAY.TYPE<1> SETTING PAY.TYPE.POS THEN
                PAY.TYPE.VAR = 1
            END
            Y.FINAL.DET.CHARGES = R.NEW(INS.DET.CHARGE)
            Y.VAR.FINAL.DET.CHARGES = Y.FINAL.DET.CHARGES
            IF PAY.TYPE.VAR EQ 1 THEN
                Y.INS.DET.CHARGE = DCOUNT(R.NEW(INS.DET.CHARGE), @VM)
                FOR KK = 1 TO Y.INS.DET.CHARGE
                    Y.AA.DET.CHARGE = Y.VAR.FINAL.DET.CHARGES<1,KK>
                    LOCATE Y.AA.DET.CHARGE IN Y.AA.PAY.PROPERTY<1> SETTING DET.CHARGE.POS THEN
                        DEL Y.FINAL.DET.CHARGES<1,KK>
                        Y.TOT.PROPERTY -= 1
                    END
                NEXT KK
                Y.FINAL.DET.CHARGES.CNT = DCOUNT(Y.FINAL.DET.CHARGES, @VM)
                IF Y.FINAL.DET.CHARGES.CNT EQ 0 THEN
                    AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2> = ''
                    AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2> = ''
                    CONTINUE
                END
            END
* PACS00867267 - END

            AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.METHOD:':II:':1'
            AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = 'DUE'

            AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.FREQ:':II:':1'
            AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = 'e0Y e1M e0W e0D e0F'

            IF R.NEW(INS.DET.CHARGE)<1,JJ> NE '' THEN
                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PROPERTY:':II:':':JJ:':1'
*       AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.CHARGE)<1,JJ>    ;*PACS00867267
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = Y.FINAL.DET.CHARGES<1,JJ>      ;*PACS00867267
            END


            IF R.NEW(INS.DET.INS.START.DATE)<1,JJ> NE '' THEN
                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'START.DATE:':II:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.INS.START.DATE)<1,JJ>
            END

            IF R.NEW(INS.DET.INS.END.DATE)<1,JJ> NE '' THEN
                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'END.DATE:':II:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.INS.END.DATE)<1,JJ>
            END

            IF R.NEW(INS.DET.MON.POL.AMT)<1,JJ> NE '' THEN
                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'ACTUAL.AMT:':II:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.MON.POL.AMT)<1,JJ>
            END

        END ELSE

            IF R.NEW(INS.DET.INS.START.DATE)<1,JJ> NE '' THEN
                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'START.DATE:':II:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.INS.START.DATE)<1,JJ>
            END

            IF R.NEW(INS.DET.INS.END.DATE)<1,JJ> NE '' THEN
                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'END.DATE:':II:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.INS.END.DATE)<1,JJ>
            END

            IF R.NEW(INS.DET.MON.POL.AMT)<1,JJ> NE '' THEN
                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'ACTUAL.AMT:':II:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.MON.POL.AMT)<1,JJ>
            END
        END

    NEXT JJ

*** ESTE PROCEDIMIENTO ES SOLO PARA CUANDO EXISTEN EXTRA PRIMAS
    JJ = 1
    IF R.NEW(INS.DET.EXTRA.AMT)<1,JJ> NE '' THEN
        FOR JJ = 1 TO Y.TOT.PROPERTY
            IF JJ EQ 1 THEN
                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.TYPE:':II+1:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.PAYMENT.TYPE)<1,JJ>

                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.METHOD:':II+1:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = 'DUE'

                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.FREQ:':II+1:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = 'e0Y e1M e0W e0D e0F'

                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PROPERTY:':II+1:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.CHARGE.EXTRA.AMT)<1,JJ>

                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'START.DATE:':II+1:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.INS.START.DATE)<1,JJ>

                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'END.DATE:':II+1:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.INS.END.DATE)<1,JJ>

                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'ACTUAL.AMT:':II+1:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.EXTRA.AMT)<1,JJ>
            END ELSE
                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'START.DATE:':II+1:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.INS.START.DATE)<1,JJ>

                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'END.DATE:':II+1:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.INS.END.DATE)<1,JJ>

                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'ACTUAL.AMT:':II+1:':':JJ:':1'
                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.EXTRA.AMT)<1,JJ>
            END
        NEXT JJ
    END
*   IF R.NEW(INS.DET.EXTRA.AMT)<1,JJ> NE '' THEN
*                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.TYPE:':II+1:':1'
*                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.PAYMENT.TYPE)<1,JJ>
*
*                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.METHOD:':II+1:':1'
*                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = 'DUE'
*
*                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PAYMENT.FREQ:':II+1:':1'
*                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = 'e0Y e1M e0W e0D e0F'
*
*                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'PROPERTY:':II+1:':':JJ:':1'
*                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.CHARGE.EXTRA.AMT)<1,JJ>
*
*                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'START.DATE:':II+1:':':JJ:':1'
*                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.INS.START.DATE)<1,JJ>
*
*                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'END.DATE:':II+1:':':JJ:':1'
*                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.INS.END.DATE)<1,JJ>
*
*                AAA.REQUEST<AA.ARR.ACT.FIELD.NAME,2,-1> = 'ACTUAL.AMT:':II+1:':':JJ:':1'
*                AAA.REQUEST<AA.ARR.ACT.FIELD.VALUE,2,-1> = R.NEW(INS.DET.EXTRA.AMT)<1,JJ>
*            END

RETURN

*-----------------------------------------------------------------------------------------------------------

GET.PROPERTY:
* Get the property Name for the property class

    ARR.INFO = ''
    R.ARRANGEMENT = ''
    AA.PROPERTY = ''

    ARR.INFO<1> = R.NEW(INS.DET.ASSOCIATED.LOAN)
    ARR.DATE = R.AA.ACCOUNT.DETAILS<AA.AD.BASE.DATE>

    CALL AA.GET.ARRANGEMENT.PROPERTIES(ARR.INFO, ARR.DATE, R.ARRANGEMENT, PROP.LIST)

    CLASS.LIST = ''
    CALL AA.GET.PROPERTY.CLASS (PROP.LIST, CLASS.LIST)

    CLASS.LIST = RAISE(CLASS.LIST)
    PROP.LIST = RAISE(PROP.LIST)

    CLASS.CTR = ''
    LOOP
        REMOVE Y.CLASS FROM CLASS.LIST SETTING CLASS.POS
        CLASS.CTR +=1
    WHILE Y.CLASS:CLASS.POS
        IF Y.CLASS EQ PROPERTY.CLASS THEN
            AA.PROPERTY = PROP.LIST<CLASS.CTR>
            BREAK
        END
    REPEAT


RETURN
*-----------------------------------------------------------------------------------------------------------

TRIGGER.OFS:

* This posts the OFS message formed to the OFS.MESSAGE.QUEUE for processing
*

    OFS.MSG.ID = ''
    OFS.SOURCE = 'TRIGGER.INSURANCE'
    OFS.ERR = ''

    CALL OFS.POST.MESSAGE(PROCESS.MSG,OFS.MSG.ID,OFS.SOURCE,OFS.ERR)

RETURN

*-----------------------------------------------------------------------------------------------------------


END
