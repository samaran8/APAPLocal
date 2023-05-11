* @ValidationCode : MjotOTU4MzgzNTc6Q3AxMjUyOjE2ODExMDk1NDMyODU6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 10 Apr 2023 12:22:23
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
$PACKAGE APAP.REDOCHNLS
SUBROUTINE AI.REDO.STMT.LOAN(Y.ID.LIST)

*-------------------------------------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : AI.REDO.STMT.LOAN
*-------------------------------------------------------------------------------------------------------
*Description  : This is a conversion routine used to display From.date and Until.date
*In Parameter : N/A
*Out Parameter: O.DATA
*Linked File  : AI.REDO.STMT.LOAN
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*     Date            Who                              Reference               Description
*    ------          ------                            -----------             --------------
*  19-09-2011       PRABHUN                            PACS00125978                MODIFICATION
*
* 10-APR-2023     Conversion tool   R22 Auto conversion   FM TO @FM, VM to @VM, SM to @SM, ++ to +=
* 10-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*--------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.PAYMENT.STOP
    $INSERT I_F.REDO.APAP.PROPERTY.PARAM
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.AA.REFERENCE.DETAILS
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AA.PROPERTY
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.ACTIVITY.BALANCES
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_System
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.AI.REDO.ARCIB.PARAMETER

    GOSUB OPENFILES
    GOSUB PROCESS
    GOSUB PGM.END
RETURN
*-----------------------------------------
OPENFILES:
*-----------------------------------------

    FN.ACCOUNT  = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.STMT.ENTRY = 'F.STMT.ENTRY'
    F.STMT.ENTRY = ''
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)

    FN.AA.REFERENCE.DETAILS = 'F.AA.REFERENCE.DETAILS'
    F.AA.REFERENCE.DETAILS  = ''
    CALL OPF(FN.AA.REFERENCE.DETAILS,F.AA.REFERENCE.DETAILS)

    FN.REDO.APAP.PROPERTY.PARAM = 'F.REDO.APAP.PROPERTY.PARAM'
    F.REDO.APAP.PROPERTY.PARAM  = ''
    CALL OPF(FN.REDO.APAP.PROPERTY.PARAM,F.REDO.APAP.PROPERTY.PARAM)

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT  = ''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.AA.PROPERTY = 'F.AA.PROPERTY'
    F.AA.PROPERTY  = ''
    CALL OPF(FN.AA.PROPERTY,F.AA.PROPERTY)

    FN.AA.ARRANGEMENT.ACTIVITY = 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AA.ARRANGEMENT.ACTIVITY  = ''
    CALL OPF(FN.AA.ARRANGEMENT.ACTIVITY,F.AA.ARRANGEMENT.ACTIVITY)

    FN.AA.ARRANGEMENT.ACTIVITY$NAU = 'F.AA.ARRANGEMENT.ACTIVITY$NAU'
    F.AA.ARRANGEMENT.ACTIVITY$NAU  = ''
    CALL OPF(FN.AA.ARRANGEMENT.ACTIVITY$NAU,F.AA.ARRANGEMENT.ACTIVITY$NAU)

    FN.AA.ACTIVITY.BALANCES = 'F.AA.ACTIVITY.BALANCES'
    F.AA.ACTIVITY.BALANCES  = ''
    CALL OPF(FN.AA.ACTIVITY.BALANCES,F.AA.ACTIVITY.BALANCES)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT  = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.AI.REDO.ARCIB.PARAMETER = 'F.AI.REDO.ARCIB.PARAMETER'
    F.AI.REDO.ARCIB.PARAMETER = ''
    CALL OPF(FN.AI.REDO.ARCIB.PARAMETER,F.AI.REDO.ARCIB.PARAMETER)

    CALL CACHE.READ(FN.AI.REDO.ARCIB.PARAMETER,'SYSTEM',R.AI.REDO.ARCIB.PARAMETER,ARCIB.ERR)
    Y.MIG.PARAM.CODE = R.AI.REDO.ARCIB.PARAMETER<AI.PARAM.MIGRATION.CODE>
    CHANGE @VM TO @FM IN Y.MIG.PARAM.CODE

    LREF.APP = 'ACCOUNT':@FM:'AZ.ACCOUNT'
    LREF.FIELDS = 'L.AC.AV.BAL':@FM:'L.TYPE.INT.PAY'
    LREF.POS=''
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    POS.L.AC.AV.BAL        = LREF.POS<1,1>
    POS.L.TYPE.INT.PAY.POS = LREF.POS<2,1>


    ACCT.ID = System.getVariable("CURRENT.ACCT.NO")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto conversion - START
        ACCT.ID = ""
    END					;*R22 Auto conversion - END

    CALL F.READ(FN.ACCOUNT,ACCT.ID,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    Y.ARRANGEMENT.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>

    LOCATE "BOOKING.DATE" IN ENQ.SELECTION<2,1> SETTING ST.DT.POS THEN
        ST.RG.DATE = ENQ.SELECTION<4,ST.DT.POS>
    END
    CHANGE @SM TO @VM IN ST.RG.DATE

RETURN
*-----------------------------------------
PROCESS:
*-----------------------------------------
    Y.AAA.ID.LIST = ''
    Y.CHARGE.AMT  = ''
    D.FIELDS<1> = 'ACCOUNT'
    D.FIELDS<2> = 'BOOKING.DATE'
    D.LOGICAL.OPERANDS = 1:@FM:2
    D.RANGE.AND.VALUE<1> = ACCT.ID
    D.RANGE.AND.VALUE<2> = ST.RG.DATE
    CALL E.STMT.ENQ.BY.CONCAT(Y.ID.LIST)

    CALL F.READ(FN.AZ.ACCOUNT,ACCT.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,AZ.ACCOUNT.ERR)
    IF R.AZ.ACCOUNT THEN
        Y.DEP.TYPE       = R.AZ.ACCOUNT<AZ.LOCAL.REF,POS.L.TYPE.INT.PAY.POS>
        IF Y.DEP.TYPE EQ 'Reinvested' THEN
            Y.INTEREST.LIQU.ACCT = R.AZ.ACCOUNT<AZ.INTEREST.LIQU.ACCT>
            D.FIELDS<1> = 'ACCOUNT'
            D.FIELDS<2> = 'BOOKING.DATE'
            D.LOGICAL.OPERANDS = 1:@FM:2
            D.RANGE.AND.VALUE<1> = Y.INTEREST.LIQU.ACCT
            D.RANGE.AND.VALUE<2> = ST.RG.DATE
            CALL E.STMT.ENQ.BY.CONCAT(Y.AZ.ID.LIST)
            Y.ID.LIST<-1> = Y.AZ.ID.LIST
        END
    END

    GOSUB MIG.SORT.PARA

    IF NOT(Y.ARRANGEMENT.ID) THEN
        RETURN
    END
    Y.STMT.TOTAL.CNT = DCOUNT(Y.ID.LIST,@FM)
    Y.STMT.CNT = 1
    LOOP
    WHILE Y.STMT.CNT LE Y.STMT.TOTAL.CNT
        Y.INS.FLAG = ''
        AMT.TO.CHECK = FIELD(Y.ID.LIST<Y.STMT.CNT>,'*',6)
        Y.SE.ID    = FIELD(Y.ID.LIST<Y.STMT.CNT>,'*',2)
        Y.SE.NEXT.ID  = FIELD(Y.ID.LIST<Y.STMT.CNT+1>,'*',2)
        GOSUB GET.TXN.DETAILS
        IF Y.INS.FLAG THEN
            Y.STMT.CNT += 2
        END ELSE
            Y.STMT.CNT += 1
        END
    REPEAT

RETURN
**************
MIG.SORT.PARA:
**************
    Y.MIG.TOT.CNT = DCOUNT(Y.ID.LIST,@FM)

    Y.MIG.INT = 1
    LOOP
    WHILE Y.MIG.INT LE Y.MIG.TOT.CNT
        Y.MIG.STMT.ID = FIELD(Y.ID.LIST<Y.MIG.INT>,'*',2)
        CALL F.READ(FN.STMT.ENTRY,Y.MIG.STMT.ID,R.STMT.ENTRY,F.STMT.ENTRY,SE.ERR)
        Y.MIG.TXN.CODE = R.STMT.ENTRY<AC.STE.TRANSACTION.CODE>
        LOCATE  Y.MIG.TXN.CODE IN Y.MIG.PARAM.CODE SETTING Y.MIG.POS THEN
            DEL Y.ID.LIST<Y.MIG.INT>
        END
        Y.MIG.INT += 1
    REPEAT

RETURN
*******************
GET.TXN.DETAILS:
*******************

    CALL F.READ(FN.STMT.ENTRY,Y.SE.ID,R.STMT.ENTRY,F.STMT.ENTRY,STMT.ENTRY.ERR)
    Y.SE.TRANS.REFERENCE = R.STMT.ENTRY<AC.STE.TRANS.REFERENCE>
    CALL F.READ(FN.STMT.ENTRY,Y.SE.NEXT.ID,R.STMT.ENTRY,F.STMT.ENTRY,STMT.ENTRY.ERR)
    Y.SE.NEXT.REFERENCE  = R.STMT.ENTRY<AC.STE.TRANS.REFERENCE>

    LOCATE Y.SE.TRANS.REFERENCE IN Y.AAA.ID.LIST SETTING REF.POS THEN
    END ELSE
        Y.AAA.ID.LIST<-1> = Y.SE.TRANS.REFERENCE
    END

    IF Y.SE.TRANS.REFERENCE NE Y.SE.NEXT.REFERENCE AND Y.CHARGE.AMT THEN
        Y.MANUAL.ARR  = FIELD(Y.ID.LIST<Y.STMT.CNT>,'*',1,5):"*":Y.CHARGE.AMT
        Y.FINAL.LIST = Y.ID.LIST
        INS Y.MANUAL.ARR BEFORE Y.ID.LIST<Y.STMT.CNT+1>
        Y.INS.FLAG = 1
    END


    ARR.ID = R.STMT.ENTRY<AC.STE.THEIR.REFERENCE>
    CALL F.READ(FN.AA.ARRANGEMENT,ARR.ID,R.AA.ARRANGEMENT,F.AA.ARRANGEMENT,ARR.ERR)
    Y.PRODUCT.GROUP = R.AA.ARRANGEMENT<AA.ARR.PRODUCT.GROUP>
    CALL CACHE.READ(FN.REDO.APAP.PROPERTY.PARAM,Y.PRODUCT.GROUP,R.REDO.APAP.PROPERTY.PARAM,PAR.ERR)
    Y.PENALTY.CHARGE.PROP = R.REDO.APAP.PROPERTY.PARAM<PROP.PARAM.PENALTY.ARREAR>
    GOSUB GET.TRANSACTION.DETAILS
RETURN

*-----------------------------------------
GET.TRANSACTION.DETAILS:
*-----------------------------------------
    Y.AAA.ID  = Y.SE.TRANS.REFERENCE
    GOSUB GET.LIST.OF.AAA       ;* Including child activity
    CALL F.READ(FN.AA.ACTIVITY.BALANCES,ARR.ID,R.AA.ACTIVITY.BALANCES,F.AA.ACTIVITY.BALANCES,AA.ACT.ERR)
    Y.ACT.CNT = DCOUNT(Y.FINAL.AAA.ID,@FM)
    Y.LOOP1 = 1
    LOOP
    WHILE Y.LOOP1 LE Y.ACT.CNT
        Y.AAA.ID.INDV = Y.FINAL.AAA.ID<Y.LOOP1>
        LOCATE Y.AAA.ID.INDV IN R.AA.ACTIVITY.BALANCES<AA.ACT.BAL.ACTIVITY.REF,1> SETTING POS2 THEN
            Y.PROPERTY     = R.AA.ACTIVITY.BALANCES<AA.ACT.BAL.PROPERTY,POS2>
            Y.PROPERTY.BAL.TYPE = FIELDS(Y.PROPERTY,'.',1)
            Y.PROPERTY.AMT = R.AA.ACTIVITY.BALANCES<AA.ACT.BAL.PROPERTY.AMT,POS2>
            GOSUB CALC.AMT
        END
        Y.LOOP1 += 1
    REPEAT
RETURN
*-----------------------------------------
GET.LIST.OF.AAA:
*-----------------------------------------
    Y.FINAL.AAA.ID = ''
    Y.LOOP.BRK = 1
    LOOP
    WHILE Y.LOOP.BRK
        Y.FINAL.AAA.ID<-1> = Y.AAA.ID
        R.AAA = ''
        CALL F.READ(FN.AA.ARRANGEMENT.ACTIVITY$NAU,Y.AAA.ID,R.AAA,F.AA.ARRANGEMENT.ACTIVITY$NAU,AAA.ERR)
        IF R.AAA ELSE
            CALL F.READ(FN.AA.ARRANGEMENT.ACTIVITY,Y.AAA.ID,R.AAA,F.AA.ARRANGEMENT.ACTIVITY,AAA.ERR)
        END
        Y.CHILD.ACTIVITY = R.AAA<AA.ARR.ACT.CHILD.ACTIVITY>
        IF R.AAA<AA.ARR.ACT.ACTIVITY.CLASS> EQ 'LENDING-CREDIT-ARRANGEMENT' THEN
            Y.ADVANCE.AMT += R.AAA<AA.ARR.ACT.ORIG.TXN.AMT>
        END
        Y.AAA.ID = Y.CHILD.ACTIVITY
        IF Y.CHILD.ACTIVITY ELSE
            Y.LOOP.BRK = 0
        END
    REPEAT
RETURN
*-----------------------------------------
CALC.AMT:
*-----------------------------------------
    Y.CHARGE.AMT = ''
    Y.PROP.CNT = DCOUNT(Y.PROPERTY,@SM)
    Y.CNT1 = 1
    LOOP
    WHILE Y.CNT1 LE Y.PROP.CNT
        Y.PROP = Y.PROPERTY.BAL.TYPE<1,1,Y.CNT1>
        CALL CACHE.READ(FN.AA.PROPERTY, Y.PROP, R.PROP.REC, PP.ERR) ;*R22 Auto conversion
        Y.PROP.CLASS = R.PROP.REC<AA.PROP.PROPERTY.CLASS>
        BEGIN CASE
            CASE Y.PROP EQ Y.PENALTY.CHARGE.PROP AND Y.PROP.CLASS EQ 'CHARGE'
                Y.CHARGE.AMT    = Y.PROPERTY.AMT<1,1,Y.CNT1>
        END CASE
        Y.CNT1 += 1
    REPEAT
RETURN
*******************************************
PGM.END:
END
