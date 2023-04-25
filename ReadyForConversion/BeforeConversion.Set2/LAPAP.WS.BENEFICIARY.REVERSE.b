*========================================================================
*-----------------------------------------------------------------------------
* <Rating>-7</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.WS.BENEFICIARY.REVERSE(Y.INFO)
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.WS.BENEFICIARY.REVERSE
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
* Views/versions :(VERSION)BENEFICIARY,DMR |(ENQ)LAPAP.WS.BENEFICIARY.REVERSE
* EB record      : LAPAP.WS.BENEFICIARY.REVERSE
*========================================================================
    
    
    
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.BENEFICIARY
    $INSERT T24.BP I_ENQUIRY.COMMON
    
    FN.BEN = "F.BENEFICIARY"
    F.BEN = ""
    CALL OPF(FN.BEN,F.BEN)
    
    LOCATE "BEN" IN D.FIELDS<1> SETTING CUS.POS THEN
    ID = D.RANGE.AND.VALUE<CUS.POS>
    END
    
    APPL.NAME = "BENEFICIARY"
    VERS.NAME = "BENEFICIARY,DMR"
    Y.FUNC = "R"
    Y.PRO.VAL = "PROCESS"
    Y.ID  = ID
    RSS = ""
    
    CALL OFS.BUILD.RECORD(APPL.NAME,Y.FUNC,Y.PRO.VAL,VERS.NAME,Y.GTS.CONTROL,Y.NO.OF.AUTH,Y.ID,RSS,FINAL.OFS)
*   CALL OFS.POST.MESSAGE(FINAL.OFS,'',"DM.OFS.SRC.VAL",'')
    CALL OFS.GLOBUS.MANAGER("DM.OFS.SRC.VAL", FINAL.OFS)
    
    REQ = FINAL.OFS
    RES = ""
    OPTIONS<1> = "FT.BULK"
    
*   CALL OFS.CALL.BULK.MANAGER(OPTIONS, REQ, RES, TXNCOMMITTED)
    CALL JOURNAL.UPDATE('')
    
*    Y.INFO<-1> = RES:"*":RES:"*"
    
*    IF TXNCOMMITTED EQ 0 THEN
*        Y.INFO<-1> = "NO EXISTE EL BENEFICIARIO"
*    END
*    IF TXNCOMMITTED EQ 1 THEN
*        Y.INFO<-1> = "OK"
*    END
    
    
    Y.INFO<-1> = FINAL.OFS    ;* "Mensaje ..."
    
    RETURN
    
    END
