*========================================================================
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE LAPAP.WS.BENEFICIARY.UPDATE(Y.INFO)
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.WS.BENEFICIARY.UPDATE
* Date           : 2018-12-18
* Item ID        : ----
*========================================================================
* Brief description :
* -------------------
* This a program allow reverse beneficiary through ENQ no file
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-12-18     Richard HC        Initial Development
*========================================================================
* Content summary :
* =================
* Table name     : FBNK.BENEFICIARY
* Auto Increment : N/A
* Views/versions :(VERSION)BENEFICIARY,DMR |(ENQ)LAPAP.WS.BENEFICIARY.UPDATE
* EB record      : LAPAP.WS.BENEFICIARY.UPDATE
*========================================================================

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BENEFICIARY
    $INSERT I_ENQUIRY.COMMON

    FN.BEN = "F.BENEFICIARY"
    F.BEN = ""
    CALL OPF(FN.BEN,F.BEN)

    LOCATE "BEN" IN D.FIELDS<1> SETTING BEN.POS THEN
        ID = D.RANGE.AND.VALUE<BEN.POS>
    END
    LOCATE "NICK" IN D.FIELDS<1> SETTING NICK.POS THEN
        NICKNAME = D.RANGE.AND.VALUE<NICK.POS>
    END
    LOCATE "EMAIL" IN D.FIELDS<1> SETTING EMAIL.POS THEN
        BEN.EMAIL = D.RANGE.AND.VALUE<EMAIL.POS>
    END
    LOCATE "ACCT" IN D.FIELDS<1> SETTING ACCT.POS THEN
        ACCT.NO = D.RANGE.AND.VALUE<ACCT.POS>
    END
    LOCATE "PROD" IN D.FIELDS<1> SETTING PROD.POS THEN
        PROD.TYPE = D.RANGE.AND.VALUE<PROD.POS>
    END

    APPL.NAME = "BENEFICIARY"
    VERS.NAME = "BENEFICIARY,RAD"
    Y.FUNC = "I"
    Y.PRO.VAL = "PROCESS"
    Y.ID  = ID

    CALL CACHE.READ(FN.BEN, ID, R.BEN, BEN.ERR)
    CALL GET.LOC.REF("BENEFICIARY","L.BEN.EMAIL",POS1)
    CALL GET.LOC.REF("BENEFICIARY","L.BEN.PROD.TYPE",POS2)

    RSS<ARC.BEN.NICKNAME> = EREPLACE(NICKNAME, "~", " ")
    RSS<ARC.BEN.LOCAL.REF,POS1> = EREPLACE(BEN.EMAIL, "~", " ")


    IF ACCT.NO NE "" THEN
        RSS<ARC.BEN.BEN.ACCT.NO> = ACCT.NO
    END

    IF PROD.TYPE NE "" THEN
        RSS<ARC.BEN.LOCAL.REF,POS2> = PROD.TYPE
    END

    CALL OFS.BUILD.RECORD(APPL.NAME,Y.FUNC,Y.PRO.VAL,VERS.NAME,Y.GTS.CONTROL,Y.NO.OF.AUTH,Y.ID,RSS,FINAL.OFS)
    CALL OFS.GLOBUS.MANAGER("DM.OFS.SRC.VAL", FINAL.OFS)
    CALL JOURNAL.UPDATE('')

    Y.INFO<-1> = FINAL.OFS    ;* "Mensaje ..."

RETURN

END
