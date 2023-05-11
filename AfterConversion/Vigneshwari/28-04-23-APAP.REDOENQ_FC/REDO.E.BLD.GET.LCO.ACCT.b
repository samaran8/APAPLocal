$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.BLD.GET.LCO.ACCT(ENQ.DATA)
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 10-APRIL-2023      Conversion Tool       R22 Auto Conversion - No changes
* 10-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.REDO.CARD.BIN
    $INSERT I_F.LATAM.CARD.ORDER

    GOSUB INIT
    GOSUB PROCESS
RETURN

*------------------------------------------------------------
INIT:
*------------------------------------------------------------
    FN.REDO.CARD.BIN = 'F.REDO.CARD.BIN'
    F.REDO.CARD.BIN = ''
    CALL OPF(FN.REDO.CARD.BIN,F.REDO.CARD.BIN)

    FN.LATAM.CARD.ORDER='F.LATAM.CARD.ORDER'
    F.LATAM.CARD.ORDER=''
    CALL OPF(FN.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER)
RETURN

*------------------------------------------------------------
PROCESS:
*------------------------------------------------------------

    Y.LCO.VAL=''
    LOCATE '@ID' IN ENQ.DATA<2,1> SETTING Y.ACCT.POS THEN
        Y.LCO.VAL = ENQ.DATA<4,Y.ACCT.POS>
    END
    Y.LCO.VAL.LEN=LEN(Y.LCO.VAL)
    IF Y.LCO.VAL.LEN NE '16' THEN
        RETURN
    END
    Y.CARD.BIN = Y.LCO.VAL[1,6]
    CALL F.READ(FN.REDO.CARD.BIN,Y.CARD.BIN,R.REDO.CARD.BIN,F.REDO.CARD.BIN,Y.ERR.BIN)
    Y.CRD.TYPE= R.REDO.CARD.BIN<REDO.CARD.BIN.CARD.TYPE>

    LOOP
        REMOVE Y.CRD.TYP FROM Y.CRD.TYPE SETTING POS.CRD

    WHILE Y.CRD.TYP:POS.CRD

        CRD.NUMBER=Y.CRD.TYP:".":Y.LCO.VAL
        CALL F.READ(FN.LATAM.CARD.ORDER,CRD.NUMBER,R.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER,ERR.CRD)
        IF R.LATAM.CARD.ORDER THEN
            ENQ.DATA<4,Y.ACCT.POS>=R.LATAM.CARD.ORDER<CARD.IS.ACCOUNT>
        END
    REPEAT

RETURN
*------------------------------------------------------------
END
