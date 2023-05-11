$PACKAGE APAP.LAPAP
*---------------------------------------------------------------------------------------
*MODIFICATION HISTORY:
*DATE           WHO                 REFERENCE               DESCRIPTION
*21-APR-2023    CONVERSION TOOL     R22 AUTO CONVERSION     INSERT FILE MODIFIED
*21-APR-2023    VICTORIA S          R22 MANUAL CONVERSION   CALL ROUTINE ADDED
*----------------------------------------------------------------------------------------
SUBROUTINE REDO.V.VAL.PRIMARY.ACC.EXTENDED
    $INSERT I_COMMON ;*R22 AUTO CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT ;*R22 AUTO CONVERSION END

    IF VAL.TEXT EQ '' THEN
        GOSUB CHECK.FIDUCIA.ACCT
    END

*CALL REDO.V.VAL.PRIMARY.ACC
*R22 MANUAL CONVERSION
    CALL APAP.REDOVER.REDO.V.VAL.PRIMARY.ACC

RETURN

********************
CHECK.FIDUCIA.ACCT:
********************
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    Y.NUMERO.CTA = COMI

    CALL F.READ(FN.ACCOUNT,Y.NUMERO.CTA,R.ACCOUNT,F.ACCOUNT,R.AC.ERR)

    IF R.ACCOUNT THEN
        Y.AC.CATEG = R.ACCOUNT<AC.CATEGORY>
        IF Y.AC.CATEG EQ '6023' THEN
            APP.ID="ENQ L.APAP.ENQ.CTA.FIDUCIA ACCOUNT.NUMBER EQ " :Y.NUMERO.CTA
            CALL EB.SET.NEW.TASK(APP.ID)
        END
    END

RETURN

END
