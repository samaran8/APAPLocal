* @ValidationCode : MjotMTc2MzIzODE5MTpDcDEyNTI6MTY4MjQxMjM1NzI4NjpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:57
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
SUBROUTINE REDO.V.VAL.CHARGE.INITIAL
*----------------------------------------------------------------------------------------------------
* DESCRIPTION : This routine is a pre-routine attached to property CHARGE in ACTIVITY.API
*-----------------------------------------------------------------------------------------------------
*  Input / Output
* --------------
* IN Parameter    : NA
* OUT Parameter   : NA
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* PROGRAM NAME : REDO.V.VAL.CHARGE.INITIAL
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* Date             Author             Reference                   Description
* 2-Aug-2010      SUJITHA.S       ODR-2009100340               Inital creation
*Modification history
*Date                Who               Reference                  Description
*19-04-2023      conversion tool     R22 Auto code conversion    VM TO @VM
*19-04-2023      Mohanraj R          R22 Manual code conversion   No changes
*------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.CHARGE
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_F.REDO.APAP.H.COMP.NAME
    $INSERT I_F.REDO.H.POLICY.NUMBER



    GOSUB INIT
    GOSUB PROCESS

RETURN

INIT:

    FN.REDO.H.POLICY.NUMBER = 'F.REDO.H.POLICY.NUMBER'
    F.REDO.H.POLICY.NUMBER = ''
    CALL OPF(FN.REDO.H.POLICY.NUMBER,F.REDO.H.POLICY.NUMBER)

    LOC.REF.APPL="AA.PRD.DES.CHARGE"
    LOC.REF.FIELDS="CLASS.POLICY":@VM:"INS.POLICY.TYPE":@VM:"POLICY.NUMBER":@VM:"SEN.POL.NUMBER":@VM:"MON.POL.AMT":@VM:"EXTRA.AMT":@VM:"MON.TOT.PRE.AMT":@VM:"MANAGEMENT.TYPE":@VM:"MON.TOT.PRE.AMT":@VM:"TOT.PREMIUM.AMT"
    FIELD.POS=""

    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,FIELD.POS)

    CLASS.POLICY.POS = FIELD.POS<1,1>
    INS.POLICY.TYPE.POS = FIELD.POS<1,2>
    POLICY.NUMBER.POS = FIELD.POS<1,3>
    SEN.POL.NUMBER.POS = FIELD.POS<1,4>
    MON.POL.AMT.POS = FIELD.POS<1,5>
    EXTRA.AMT.POS = FIELD.POS<1,6>
    MON.TOT.PRE.AMT.POS = FIELD.POS<1,7>
    MANAGEMENT.TYPE.POS = FIELD.POS<1,8>
    MON.TOT.PRE.AMT.POS = FIELD.POS<1,9>
    TOT.PREMIUM.AMT.POS = FIELD.POS<1,10>

    VAR.INS.POL.TYPE1 = R.NEW(AA.CHG.LOCAL.REF)<1,INS.POLICY.TYPE.POS>

    VAR.CLASS.POLICY1 = R.NEW(AA.CHG.LOCAL.REF)<1,CLASS.POLICY.POS>

    EXTRA.AMT = R.NEW(AA.CHG.LOCAL.REF)<1,EXTRA.AMT.POS>

RETURN

PROCESS:

    VAR.INS.POL.TYPE1 = R.NEW(AA.CHG.LOCAL.REF)<1,INS.POLICY.TYPE.POS>
    VAR.CLASS.POLICY1 = R.NEW(AA.CHG.LOCAL.REF)<1,CLASS.POLICY.POS>
    Y.SEN.POLICY.NUMBER = R.NEW(AA.CHG.LOCAL.REF)<1,SEN.POL.NUMBER.POS>
    Y.IND.POLICY.NUMBER = R.NEW(AA.CHG.LOCAL.REF)<1,POLICY.NUMBER.POS>
    MON.POL.AMT1 = R.NEW(AA.CHG.LOCAL.REF)<1,MON.POL.AMT.POS>
    MGMT.VAL     = R.NEW(AA.CHG.LOCAL.REF)<1,MANAGEMENT.TYPE.POS>

    IF NOT(MGMT.VAL) THEN
        AF = AA.CHG.LOCAL.REF
        AV = INS.POLICY.TYPE.POS
        ETEXT = 'EB-MGMT.TYPE.MISSING'
        CALL STORE.END.ERROR
    END

    IF NOT(VAR.INS.POL.TYPE1) THEN
        AF = AA.CHG.LOCAL.REF
        AV = INS.POLICY.TYPE.POS
        ETEXT = 'EB-INS.POL.TYPE.MISSING'
        CALL STORE.END.ERROR
    END

    IF NOT(VAR.CLASS.POLICY1) THEN
        AF = AA.CHG.LOCAL.REF
        AV = CLASS.POLICY.POS
        ETEXT = 'EB-CLASS.POLICY.MISSING'
        CALL STORE.END.ERROR
    END

    IF NOT(Y.SEN.POLICY.NUMBER) THEN
        AF = AA.CHG.LOCAL.REF
        AV = SEN.POL.NUMBER.POS
        ETEXT = 'EB-SEN.POLICY.NUMBER.MISSING'
        CALL STORE.END.ERROR
    END

    IF NOT(Y.IND.POLICY.NUMBER) THEN
        AF = AA.CHG.LOCAL.REF
        AV = POLICY.NUMBER.POS
        ETEXT = 'EB-IND.POLICY.NUMBER.MISSING'
        CALL STORE.END.ERROR
    END

    IF NOT(MON.POL.AMT1) THEN
        AF = AA.CHG.LOCAL.REF
        AV = MON.POL.AMT.POS
        ETEXT = 'EB-MON.POL.AMT.MISSING'
        CALL STORE.END.ERROR
    END

    IF MGMT.VAL EQ 'Included on Fee' THEN
        IF EXTRA.AMT EQ '' THEN
            MON.POL.AMT =  R.NEW(AA.CHG.LOCAL.REF)<1,MON.POL.AMT.POS>
            R.NEW(AA.CHG.CHG.AMOUNT) = MON.POL.AMT
        END ELSE
            MON.POL.PRE.AMT = R.NEW(AA.CHG.LOCAL.REF)<1,MON.TOT.PRE.AMT.POS>
            R.NEW(AA.CHG.CHG.AMOUNT) = MON.POL.PRE.AMT
        END

    END
RETURN
END
