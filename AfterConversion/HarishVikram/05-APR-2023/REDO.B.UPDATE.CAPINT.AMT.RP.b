* @ValidationCode : MjotMTA4MjMyNjMzNTpDcDEyNTI6MTY4MDY4NTc5NzkyMDpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 05 Apr 2023 14:39:57
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
SUBROUTINE REDO.B.UPDATE.CAPINT.AMT.RP
*-----------------------------------------------------------------------------
* Company Name  : APAP DEV2
* Developed By  : Marimuthu S
* Program Name  : REDO.B.UPDATE.CAPINT.AMT.RP
*-----------------------------------------------------------------
* Description : This single threaded routine used to group the loans whenever income has happened
*-----------------------------------------------------------------
* Linked With   : -NA-
* In Parameter  : -NA-
* Out Parameter : -NA-
*-----------------------------------------------------------------
* Modification History :
*-----------------------
* Reference              Date                Description
* ODR-2011-12-0017      21-Nov-2011          Initial draft
* 04-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.ACCT.MRKWOF.HIST
    $INSERT I_F.REDO.WORK.INT.CAP.AMT.RP


MAIN:

    GOSUB OPENFILES
    GOSUB PROCESS
    GOSUB PGM.END


OPENFILES:

    FN.REDO.ACCT.MRKWOF.HIST = 'F.REDO.ACCT.MRKWOF.HIST'
    F.REDO.ACCT.MRKWOF.HIST = ''
    CALL OPF(FN.REDO.ACCT.MRKWOF.HIST,F.REDO.ACCT.MRKWOF.HIST)

    FN.REDO.WORK.PARTIAL.AMT = 'F.REDO.WORK.PARTIAL.AMT'
    F.REDO.WORK.PARTIAL.AMT = ''
    CALL OPF(FN.REDO.WORK.PARTIAL.AMT,F.REDO.WORK.PARTIAL.AMT)

    FN.REDO.WORK.INT.CAP.AMT.RP = 'F.REDO.WORK.INT.CAP.AMT.RP'
    F.REDO.WORK.INT.CAP.AMT.RP = ''
    CALL OPF(FN.REDO.WORK.INT.CAP.AMT.RP,F.REDO.WORK.INT.CAP.AMT.RP)

RETURN

PROCESS:


    SEL.CMD = 'SELECT ':FN.REDO.ACCT.MRKWOF.HIST:' WITH STATUS EQ INITIATED'
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.RECS,SEL.ERR)

    FLG = ''
    LOOP
    WHILE NO.RECS GT 0 DO
        FLG += 1
        Y.AA.ID = SEL.LIST<FLG>
        CALL F.READ(FN.REDO.ACCT.MRKWOF.HIST,Y.AA.ID,R.REDO.ACCT.MRKWOF.HIST,F.REDO.ACCT.MRKWOF.HIST,WOF.ERR)
        CALL F.READ(FN.REDO.WORK.PARTIAL.AMT,Y.AA.ID,R.REDO.WORK.PARTIAL.AMT,F.REDO.WORK.PARTIAL.AMT,WOR.RK.ERR)
        IF NOT(R.REDO.WORK.PARTIAL.AMT) THEN
            R.REDO.WORK.PARTIAL.AMT = R.REDO.ACCT.MRKWOF.HIST<REDO.WH.TOT.INT.PAID>:'*':R.REDO.ACCT.MRKWOF.HIST<REDO.WH.TOT.PRINCIPAL.PAID>
        END ELSE
            R.REDO.WORK.PARTIAL.AMT = (R.REDO.ACCT.MRKWOF.HIST<REDO.WH.TOT.INT.PAID> - FIELD(R.REDO.WORK.PARTIAL.AMT,'*',1)):'*':(R.REDO.ACCT.MRKWOF.HIST<REDO.WH.TOT.PRINCIPAL.PAID> - FIELD(R.REDO.WORK.PARTIAL.AMT,'*',2))
        END
        CALL F.WRITE(FN.REDO.WORK.PARTIAL.AMT,Y.AA.ID,R.REDO.WORK.PARTIAL.AMT)

        Y.CURR = R.REDO.ACCT.MRKWOF.HIST<REDO.WH.CURRENCY>
        Y.CATEG = R.REDO.ACCT.MRKWOF.HIST<REDO.WH.CATEGORY>
        Y.AGE.ST = R.REDO.ACCT.MRKWOF.HIST<REDO.WH.ARR.AGE.STATUS>
        Y.TEMP.ID = Y.CURR:'*':Y.CATEG:'*':Y.AGE.ST
        CALL F.READ(FN.REDO.WORK.INT.CAP.AMT.RP,Y.TEMP.ID,R.REDO.WORK.INT.CAP.AMT.RP,F.REDO.WORK.INT.CAP.AMT.RP,WORK.ERR)
        IF NOT(R.REDO.WORK.INT.CAP.AMT.RP) THEN
            Y.TM.INT.VAL = FIELD(R.REDO.WORK.PARTIAL.AMT,'*',1)
            IF Y.TM.INT.VAL GT 0 THEN
                R.REDO.WORK.INT.CAP.AMT.RP<REDO.RP.AMT.INTEREST.TEXT> = Y.AA.ID:'*':FIELD(R.REDO.WORK.PARTIAL.AMT,'*',1):'*':TODAY
            END
            Y.TM.PR.VAL = FIELD(R.REDO.WORK.PARTIAL.AMT,'*',2)
            IF Y.TM.PR.VAL GT 0 THEN
                R.REDO.WORK.INT.CAP.AMT.RP<REDO.RP.AMT.CAPITAL.TEXT> = Y.AA.ID :'*':FIELD(R.REDO.WORK.PARTIAL.AMT,'*',2):'*':TODAY
            END
        END ELSE
            Y.TM.INT.VAL = FIELD(R.REDO.WORK.PARTIAL.AMT,'*',1)
            IF Y.TM.INT.VAL GT 0 THEN
                R.REDO.WORK.INT.CAP.AMT.RP<REDO.RP.AMT.INTEREST.TEXT,-1> = Y.AA.ID:'*':FIELD(R.REDO.WORK.PARTIAL.AMT,'*',1):'*':TODAY
            END
            Y.TM.PR.VAL = FIELD(R.REDO.WORK.PARTIAL.AMT,'*',2)
            IF Y.TM.PR.VAL GT 0 THEN
                R.REDO.WORK.INT.CAP.AMT.RP<REDO.RP.AMT.CAPITAL.TEXT,-1> = Y.AA.ID :'*':FIELD(R.REDO.WORK.PARTIAL.AMT,'*',2):'*':TODAY
            END
        END
        CALL F.WRITE(FN.REDO.WORK.INT.CAP.AMT.RP,Y.TEMP.ID,R.REDO.WORK.INT.CAP.AMT.RP)
        NO.RECS -= 1
    REPEAT

RETURN

PGM.END:

END
