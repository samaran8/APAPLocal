$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.SEP.CQ.CASH.AMTS
*-----------------------------------------------------------------------------
* Marimuthus
* MANTIS  4798
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 12-APRIL-2023      Conversion Tool       R22 Auto Conversion - VM to @VM
* 12-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.TRANSACTION.CHAIN
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.TELLER.ID
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER


MAIN:


    Y.ID = O.DATA
    FN.RTC = 'F.REDO.TRANSACTION.CHAIN'
    F.RTC = ''
    CALL OPF(FN.RTC,F.RTC)

    FN.TELLER.ID = 'F.TELLER.ID'
    F.TELLER.ID = ''
    CALL OPF(FN.TELLER.ID,F.TELLER.ID)

    Y.APLS = 'TELLER.ID'
    Y.FIELDS = 'L.CH.CCY'
    Y.PSS = ''
    CALL MULTI.GET.LOC.REF(Y.APLS,Y.FIELDS,Y.PSS)
    Y.CCY.POS = Y.PSS<1,1>

    CALL F.READ(FN.RTC,Y.ID,R.RTC,F.RTC,RTC.ERR)
    Y.TEL.ID = R.RTC<RTC.TELLER.ID>
    Y.TYPES = R.RTC<RTC.TRANS.TYPE>
    Y.TRNS.ID = R.RTC<RTC.TRANS.ID>
    Y.CNT = DCOUNT(Y.TYPES,@VM)
    FLG = '' ; Y.CK.AMT = ''; Y.CQ.AMT = ''
    LOOP
    WHILE Y.CNT GT 0 DO
        FLG += 1
        Y.TP = Y.TYPES<1,FLG>
        Y.TRNS = Y.TRNS.ID<1,FLG>
        IF Y.TRNS[1,2] NE 'FT' THEN
            IF Y.TP EQ 'CHECK' THEN
                Y.CK.AMT += R.RTC<RTC.TRANS.AMOUNT,FLG>
            END
            GOSUB SEP.CASH
        END ELSE
            Y.FT = 'Y'
            Y.TOT.AMT.FT += R.RTC<RTC.TRANS.AMOUNT,FLG>
        END
        Y.CNT -= 1
    REPEAT

    CALL F.READ(FN.TELLER.ID,Y.TEL.ID,R.TELLER.ID,F.TELLER.ID,TEL.ERR)
    Y.CCY = R.TELLER.ID<TT.TID.LOCAL.REF,Y.CCY.POS>

    IF Y.CCY EQ '' THEN
        Y.CCY = R.RTC<RTC.TRANS.CCY,1>
    END

    IF Y.CK.AMT OR Y.CQ.AMT THEN
* Y.TOT.AMT = Y.CK.AMT + Y.CQ.AMT
        O.DATA = Y.CCY:'*':Y.CQ.AMT:'*':Y.CK.AMT:'*':Y.TOT.AMT.FT
    END ELSE
        IF Y.FT EQ 'Y' OR Y.TT EQ 'Y' THEN
            O.DATA = Y.CCY:'*':'':'*':'':'*':Y.TOT.AMT.FT
        END ELSE
            O.DATA = ''
        END
    END

    GOSUB PGM.END

RETURN

SEP.CASH:

    IF Y.TP EQ 'CASH' OR Y.TP EQ 'NON.AA' OR Y.TP EQ 'ACCOUNT.DEBIT' THEN
        IF Y.ID[1,2] EQ 'TT' AND R.RTC<RTC.TRANS.AMOUNT,FLG> LT 0 THEN
            Y.TOT.AMT.FT += R.RTC<RTC.TRANS.AMOUNT,FLG>
            Y.TT = 'Y'
        END ELSE
            Y.CQ.AMT += R.RTC<RTC.TRANS.AMOUNT,FLG>
        END
    END

RETURN

PGM.END:

END
