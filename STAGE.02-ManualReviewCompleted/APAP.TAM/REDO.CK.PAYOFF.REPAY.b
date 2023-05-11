* @ValidationCode : MjotMjcwNjI3NTE5OkNwMTI1MjoxNjgwNjc0NzUxMjAyOklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 11:35:51
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM

SUBROUTINE REDO.CK.PAYOFF.REPAY
    
*-----------------------------------------------------------------------------------------------------
* Modification History:
*
* Date             Who                   Reference      Description
* 05.04.2023       Conversion Tool       R22            Auto Conversion     - VM TO @VM, SM TO @SM
* 05.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.BILL.DETAILS
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.REFERENCE.DETAILS
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.REDO.APAP.PROPERTY.PARAM
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.ACCOUNT

    FN.AA.AC = 'F.AA.ACCOUNT.DETAILS'
    F.AA.AC = ''
    CALL OPF(FN.AA.AC,F.AA.AC)

    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''
    CALL OPF(FN.FT,F.FT)

    FN.FT.NAU = 'F.FUNDS.TRANSFER$NAU'
    F.FT.NAU = ''
    CALL OPF(FN.FT.NAU,F.FT.NAU)

    FN.AA.REF = 'F.AA.REFERENCE.DETAILS'
    F.AA.REF = ''
    CALL OPF(FN.AA.REF,F.AA.REF)

    FN.AAA = 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AAA = ''
    CALL OPF(FN.AAA,F.AAA)

    FN.AAA.NAU = 'F.AA.ARRANGEMENT.ACTIVITY$NAU'
    F.AAA.NAU = ''
    CALL OPF(FN.AAA.NAU,F.AAA.NAU)

    FN.REDO.APAP.PROPERTY.PARAM = 'F.REDO.APAP.PROPERTY.PARAM'
    F.REDO.APAP.PROPERTY.PARAM = ''
    CALL OPF(FN.REDO.APAP.PROPERTY.PARAM,F.REDO.APAP.PROPERTY.PARAM)

    FN.AA = 'F.AA.ARRANGEMENT'
    F.AA = ''
    CALL OPF(FN.AA,F.AA)

    FN.AC = 'F.ACCOUNT'
    F.AC = ''
    CALL OPF(FN.AC,F.AC)


    Y.AC.ID = R.NEW(FT.CREDIT.ACCT.NO)
    CALL F.READ(FN.AC,Y.AC.ID,R.AC,F.AC,AC.ERR)
    Y.AA.ID = R.AC<AC.ARRANGEMENT.ID>

    CALL F.READ(FN.AA,Y.AA.ID,R.AA,F.AA,AA.ERR)
    Y.PRD = R.AA<AA.ARR.PRODUCT.GROUP>

    CALL F.READ(FN.REDO.APAP.PROPERTY.PARAM,Y.PRD,R.PARAM,F.REDO.APAP.PROPERTY.PARAM,PAR.ERR)
    Y.PAYOFF.ACT = R.PARAM<PROP.PARAM.PAYOFF.ACTIVITY>

    CALL F.READ(FN.AA.AC,Y.AA.ID,R.AA.AC,F.AA.AC,AA.AC.ERR)

    Y.PAY.MET = R.AA.AC<AA.AD.PAY.METHOD>
    Y.PAY.MET = CHANGE(Y.PAY.MET,@SM,@VM)

    LOCATE 'INFO' IN Y.PAY.MET<1,1> SETTING POS.INF THEN
        Y.VAL = 'PAYOFF'
    END ELSE
        Y.VAL = 'NO'
    END

    Y.FIN = ''
    IF Y.VAL EQ 'PAYOFF' THEN
        CALL F.READ(FN.AA.REF,Y.AA.ID,R.AA.REF,F.AA.REF,REF.AA.ER)
        Y.AAA.ID = R.AA.REF<AA.REF.AAA.ID>
        Y.CNT = DCOUNT(Y.AAA.ID,@VM) ; FLG = ''
        LOOP
        WHILE Y.CNT GT 0 DO
            FLG += 1
            Y.AAA = Y.AAA.ID<1,FLG>
            CALL F.READ(FN.AAA,Y.AAA,R.AAA,F.AAA,AAA.ERR)
            IF NOT(R.AAA) THEN
                CALL F.READ(FN.AAA.NAU,Y.AAA,R.AAA,F.AAA.NAU,AA.ERR.N)
            END
            Y.ACT.ID = R.AAA<AA.ARR.ACT.ACTIVITY>
            GOSUB LOC.COMM
            Y.CNT -= 1
        REPEAT
    END

    IF Y.VAL EQ 'PAYOFF' AND Y.FIN EQ 'YES' THEN
        ETEXT = 'EB-PAYOFF.NOT.ALLOWED'
        AF = FT.CREDIT.ACCT.NO
        CALL STORE.END.ERROR
    END

RETURN

LOC.COMM:

    LOCATE Y.ACT.ID IN Y.PAYOFF.ACT<1,1> SETTING STR.P THEN
        Y.FT.ID = R.AA.REF<AA.REF.TRANS.REF,FLG>
        CALL F.READ(FN.FT,Y.FT.ID,R.FT,F.FT,FT.ERR)
        IF NOT(R.FT) THEN
            CALL F.READ(FN.FT.NAU,Y.FT.ID,R.FT,F.FT.NAU,FT.ERR)
        END
        IF R.FT<FT.CREDIT.THEIR.REF>[1,2] NE 'FT' THEN
            Y.FIN = 'YES'
            Y.CNT = 0
        END
    END

RETURN

END
