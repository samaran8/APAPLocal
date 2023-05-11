* @ValidationCode : MjotMTI1MDU5NjAyNjpDcDEyNTI6MTY4MTcwMjc3NTg0ODpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 09:09:35
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
$PACKAGE APAP.TAM
SUBROUTINE REDO.SMS.CHECK.ACTIVITY

** 17-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 17-04-2023 Skanda R22 Manual Conversion - added APAP.TAM

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.USER
    $INSERT I_F.AA.ACTIVITY
    $INSERT I_F.AA.PROPERTY
    $INSERT I_AA.LOCAL.COMMON
MAIN:

    IF APPLICATION NE 'AA.ARRANGEMENT.ACTIVITY' THEN
        GOSUB PGM.END
    END

    FN.USER = 'F.USER'
    F.USER = ''
    CALL OPF(FN.USER,F.USER)

    FN.AA.ACTIVITY = 'F.AA.ACTIVITY'
    F.AA.ACTIVITY = ''
    CALL OPF(FN.AA.ACTIVITY,F.AA.ACTIVITY)

    FN.AA.PROPERTY = 'F.AA.PROPERTY'
    F.AA.PROPERTY = ''
    CALL OPF(FN.AA.PROPERTY,F.AA.PROPERTY)

    FN.AAA.NAU = 'F.AA.ARRANGEMENT.ACTIVITY$NAU'
    F.AAA.NAU = ''
    CALL OPF(FN.AAA.NAU,F.AAA.NAU)

    POS.SS = ''
    Y.FLDS = 'L.TELR.LOAN':@VM:'L.ALLOW.ACTS'
    Y.APPLNS = 'USER'
    CALL MULTI.GET.LOC.REF(Y.APPLNS,Y.FLDS,POS.SS)
    Y.POS.TRL = POS.SS<1,1>
    Y.POS.ALW.ACT = POS.SS<1,2>



    Y.USR = OPERATOR
    CALL CACHE.READ(FN.USER, Y.USR, R.USR, ERR.US) ;* R22 Auto conversion

    IF R.USR<EB.USE.LOCAL.REF,Y.POS.TRL> EQ 'TELLER' THEN
        IF V$FUNCTION EQ 'S' THEN
            E = 'EB-ACTIVITY.NOT.ALLOW'
            CALL STORE.END.ERROR
        END ELSE
            GOSUB PROCESS
        END
    END ELSE
        IF R.USR<EB.USE.LOCAL.REF,Y.POS.TRL> EQ 'OTHERS' THEN
            IF V$FUNCTION EQ 'S' THEN
                E = 'EB-ACTIVITY.NOT.ALLOW'
                CALL STORE.END.ERROR
            END ELSE
                AF = AA.ARR.ACT.ACTIVITY
                E = 'EB-ACTIVITY.NOT.ALLOW'
                CALL STORE.END.ERROR
            END
        END
    END

RETURN

PROCESS:

    Y.ALLW.ACTS = R.USR<EB.USE.LOCAL.REF,Y.POS.ALW.ACT> ; Y.ALLW.ACTS = CHANGE(Y.ALLW.ACTS,@SM,@VM)
    Y.ACT = R.NEW(AA.ARR.ACT.ACTIVITY)
    IF Y.ACT EQ '' THEN         ;* Warning message raised during repayment which doesnt allow to commit the transaction.
*                                                We have R.NEW value as NULL. So if the R.NEW is NULL then return.
        RETURN
    END
    CALL CACHE.READ(FN.AA.ACTIVITY, Y.ACT, R.ACT, AA.ACT.ER) ;* R22 Auto conversion
    Y.PRP.CLS = R.ACT<AA.ACT.PROPERTY>

    IF Y.PRP.CLS NE 'ARRANGEMENT' THEN
        CALL CACHE.READ(FN.AA.PROPERTY, Y.PRP.CLS, R.AA.PROP, PROP.ERR) ;* R22 Auto conversion
        Y.ACT.CLASS = R.ACT<AA.ACT.PRODUCT.LINE>:'-':R.ACT<AA.ACT.PROCESS.ID>:'-':R.AA.PROP<AA.PROP.PROPERTY.CLASS>
    END ELSE
        Y.ACT.CLASS = R.ACT<AA.ACT.PRODUCT.LINE>:'-':R.ACT<AA.ACT.PROCESS.ID>:'-':Y.PRP.CLS
    END

    LOCATE Y.ACT.CLASS IN Y.ALLW.ACTS<1,1> SETTING POS.M ELSE
        Y.MASTER.ACT = R.NEW(AA.ARR.ACT.MASTER.AAA)
        IF ID.NEW EQ Y.MASTER.ACT THEN

            IF V$FUNCTION NE 'R' AND R.NEW(AA.ARR.ACT.REV.MASTER.AAA) EQ '' THEN      ;* Ref - PACS00405487, After the payoff txn, we will post OFS message for lending change charge to update insurance as cancelled. So during reversal of payoff txn, first it will reverse first change-charge activity via OFS(in this case both master & current has same id) but teller user will not have permission to do that activity which will end in error. so we will not validate SMS check during reversal of txn.
                AF = AA.ARR.ACT.ACTIVITY
                ETEXT = 'EB-ACTIVITY.NOT.ALLOW' ;* Both E & ETEXT used. so that error message displayed in screen.
                E = 'EB-ACTIVITY.NOT.ALLOW'
                CALL STORE.END.ERROR
            END
        END ELSE
            CALL F.READ(FN.AAA.NAU,Y.MASTER.ACT,R.AAA.NAU,F.AAA.NAU,AAA.NAU.ERR)
            Y.MS.ACT.CLASS = R.AAA.NAU<AA.ARR.ACT.ACTIVITY.CLASS>

            LOCATE Y.MS.ACT.CLASS IN Y.ALLW.ACTS<1,1> SETTING POS.MM ELSE
                AF = AA.ARR.ACT.ACTIVITY
                ETEXT = 'EB-ACTIVITY.NOT.ALLOW'
                E = 'EB-ACTIVITY.NOT.ALLOW'
                CALL STORE.END.ERROR
            END
        END
    END

RETURN

PGM.END:

END
