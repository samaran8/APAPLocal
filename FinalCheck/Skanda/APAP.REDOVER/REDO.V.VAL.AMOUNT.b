* @ValidationCode : MjotMTkwOTY2Mjg0MDpDcDEyNTI6MTY4MTM3MjYzMDA5NjpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 13 Apr 2023 13:27:10
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
*TSR-337930   Rajasoundarya S    11-Aug-2022    Facing this error : MONTO ML EB-SALDO CADENA EN CHEQUE
*-----------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*13-04-2023            Conversion Tool             R22 Auto Code conversion                      VM TO @VM,++ to +=1
*13-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*-------------------------------------------------------------------------------------------------------------------------
SUBROUTINE REDO.V.VAL.AMOUNT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER

    GOSUB INIT
    GOSUB PROCESS
RETURN

INIT:

    FN.TELLER.NAU = "F.TELLER$NAU"
    F.TELLER.NAU = ""
    CALL OPF(FN.TELLER.NAU,F.TELLER.NAU)

    Y.APP = "TELLER"
    Y.FIELDS = "L.DEBIT.AMOUNT":@VM:"L.INITIAL.ID":@VM:"L.ACTUAL.VERSIO":@VM:"L.NEXT.VERSION"
    Y.POS = ""

    CALL MULTI.GET.LOC.REF(Y.APP,Y.FIELDS,Y.POS)
    Y.DEB.POS = Y.POS<1,1>
    Y.INIT.POS = Y.POS<1,2>
    Y.ACT.POS = Y.POS<1,3>
    Y.NXT.VER.POS = Y.POS<1,4>
    Y.DEBIT.AMT = ""
    Y.AMOUNT = ""
    R.TT.REC = ""

RETURN


PROCESS:

    Y.TT.ID = R.NEW(TT.TE.LOCAL.REF)<1,Y.INIT.POS>
*TSR-337930
*Y.NEXT.VERSION = R.NEW(TT.TE.LOCAL.REF)<1,Y.NXT.VER.POS>
    Y.CURR.ID = ID.NEW

    Y.AMOUNT = R.NEW(TT.TE.AMOUNT.LOCAL.1)
    CALL F.READ(FN.TELLER.NAU,Y.TT.ID,R.TT.REC,F.TELLER.NAU,ERR.TEE)
    Y.NEXT.VERSION = R.TT.REC<TT.TE.LOCAL.REF,Y.NXT.VER.POS>

    IF R.TT.REC AND R.TT.REC<TT.TE.LOCAL.REF,Y.ACT.POS> EQ "TELLER,REDO.ENV.TRANSF.INTL" THEN
        Y.DEBIT.AMT = R.TT.REC<TT.TE.LOCAL.REF,Y.DEB.POS>

        SEL.CMD = "SELECT ":FN.TELLER.NAU:" WITH L.INITIAL.ID EQ ":Y.TT.ID
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',Y.TOT.LIST,CUST.ERR)

        IF CUST.ERR NE "" AND Y.TOT.LIST GT 0 THEN
            CNT = 1
            LOOP
            WHILE CNT LE Y.TOT.LIST
                REC.ID = SEL.LIST<CNT>
                IF Y.TT.ID NE REC.ID THEN
                    CALL F.READ(FN.TELLER.NAU,REC.ID,R.TT.REC.PREV,F.TELLER.NAU,ERR.TEE.PREV)
                    REC.STATUS = R.TT.REC.PREV<TT.TE.RECORD.STATUS>
                    REC.ACT.VERSION = R.TT.REC.PREV<TT.TE.LOCAL.REF,Y.ACT.POS>
                    IF REC.STATUS EQ 'INAU' AND REC.ACT.VERSION NE "TELLER,REDO.ENV.TRANSF.INTL" THEN
                        Y.AMOUNT = Y.AMOUNT + R.TT.REC.PREV<TT.TE.AMOUNT.LOCAL.1>
                    END
                END
                CNT += 1
            REPEAT
        END

        IF (Y.NEXT.VERSION EQ "") AND Y.DEBIT.AMT AND (Y.DEBIT.AMT NE Y.AMOUNT) THEN
            AF = TT.TE.AMOUNT.LOCAL.1
            AV = "1"
            ETEXT = 'EB-INCORRECT.AMT'
            CALL STORE.END.ERROR
        END

    END

RETURN


END
