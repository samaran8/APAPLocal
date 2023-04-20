SUBROUTINE L.APAP.V.ACCT.TD.NO.CLOSE.RT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.LATAM.CARD.ORDER
    $INSERT I_F.REDO.CARD.RENEWAL
*------------------------------------------------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------------------------------------------------
    Y.ACCOUNT = COMI
*------------------------------------------------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------------------------------------------------
    SEL.LIST = ''
    NO.OF.RECS = ''
    SEL.ERR = ''
    FN.LCO = "F.LATAM.CARD.ORDER"
    FV.LCO = ""
    R.LCO = ""
    LCO.ERR = ""
    CALL OPF(FN.LCO,FV.LCO)
    FN.CR = "F.REDO.CARD.RENEWAL"
    FV.CR = ""
    R.CR = ""
    CR.ERR = ""
    CALL OPF(FN.CR,FV.CR)
*------------------------------------------------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------------------------------------------------
    SEL.CMD = "SELECT F.REDO.CARD.RENEWAL WITH @ID LIKE ..." : Y.ACCOUNT
*------------------------------------------------------------------------------------------------------------------------------------
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.RECS,SEL.ERR)
*------------------------------------------------------------------------------------------------------------------------------------
    LOOP REMOVE Y.TD.ACC.ID FROM SEL.LIST SETTING CR.POS
    WHILE Y.TD.ACC.ID DO
        CALL F.READ(FN.CR,Y.TD.ACC.ID,R.CR, FV.CR, CR.ERR)
        Y.CS = R.CR<REDO.RENEW.STATUS>
        Y.CAN.CS = DCOUNT(Y.CS,@VM)
        FOR A = 1 TO Y.CAN.CS STEP 1
            Y.TMP.CS = R.CR<REDO.RENEW.STATUS, A>
            IF (Y.TMP.CS EQ 51 OR Y.TMP.CS EQ 74 OR Y.TMP.CS EQ 75 OR Y.TMP.CS EQ 90 OR Y.TMP.CS EQ 94 OR Y.TMP.CS EQ 96) THEN
                MESSAGE = "DEBE CANCELAR TD RELACIONADA A ESTA CUENTA."
                E = MESSAGE
                ETEXT = E
                CALL ERR
            END
        NEXT A


    REPEAT


END
