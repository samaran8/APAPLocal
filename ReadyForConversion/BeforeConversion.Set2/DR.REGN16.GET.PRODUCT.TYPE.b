*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.REGN16.GET.PRODUCT.TYPE

*-------------------------------------------------------------------------
* Date              Author                    Description
* ==========        ====================      ============
* 31-07-2014        Ashokkumar                PACS00366332- Initial revision
* 28-11-2017        Ashokkumar                CN006499 - Fix to get the credit card details through parameter.
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.AA.PRODUCT.GROUP
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.AA.ACCOUNT
    $INCLUDE TAM.BP I_F.REDO.H.REPORTS.PARAM
    $INCLUDE TAM.BP I_F.REDO.ISSUE.CLAIMS
    $INCLUDE LAPAP.BP I_DR.REG.REGN16.EXTRACT.COMMON


    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT = ''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.AA.PRODUCT.GROUP = 'F.AA.PRODUCT.GROUP'
    F.AA.PRODUCT.GROUP = ''
    CALL OPF(FN.AA.PRODUCT.GROUP,F.AA.PRODUCT.GROUP)

    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM"
    F.REDO.H.REPORTS.PARAM  = ""
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)

    FN.AA.ARR.TERM.AMOUNT = 'F.AA.ARR.TERM.AMOUNT'
    F.AA.ARR.TERM.AMOUNT = ''
    CALL OPF(FN.AA.ARR.TERM.AMOUNT,F.AA.ARR.TERM.AMOUNT)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.AA.ARR.ACCOUNT = 'F.AA.ARR.ACCOUNT'
    F.AA.ARR.ACCOUNT = ''
    CALL OPF(FN.AA.ARR.ACCOUNT,F.AA.ARR.ACCOUNT)

    FN.ACCOUNT.H = 'F.ACCOUNT$HIS'; F.ACCOUNT.H = ''
    CALL OPF(FN.ACCOUNT.H,F.ACCOUNT.H)

    FN.REDO.ISSUE.CLAIMS = 'F.REDO.ISSUE.CLAIMS'
    F.REDO.ISSUE.CLAIMS = ''
    CALL OPF(FN.REDO.ISSUE.CLAIMS,F.REDO.ISSUE.CLAIMS)

    FLD.POS = ''
    APP.VAL = 'AA.ARR.ACCOUNT':FM:'AZ.ACCOUNT'
    LOC.FLD = 'L.CR.FACILITY':FM:'L.AZ.DEP.NAME'
    CALL MULTI.GET.LOC.REF(APP.VAL,LOC.FLD,FLD.POS)
    L.CR.FACILITY.POS = FLD.POS<1,1>
    L.AZ.DEP.NAME.POS = FLD.POS<2,1>
*
    Y.REPORT.PARAM.ID = "PU01-REGN16"
    PARAM.ERR = ''; R.REDO.H.REPORTS.PARAM = ''
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,Y.REPORT.PARAM.ID,R.REDO.H.REPORTS.PARAM,PARAM.ERR)


    REDO.ISSUE.CLAIMS.ID = COMI
    R.REDO.ISSUE.CLAIMS = ''; ERR.REDO.ISSUE.CLAIMS = ''
    CALL F.READ(FN.REDO.ISSUE.CLAIMS,REDO.ISSUE.CLAIMS.ID,R.REDO.ISSUE.CLAIMS,F.REDO.ISSUE.CLAIMS,ERR.REDO.ISSUE.CLAIMS)
    ACC.ID = R.REDO.ISSUE.CLAIMS<ISS.CL.ACCOUNT.ID>
    YCARD.NO = R.REDO.ISSUE.CLAIMS<ISS.CL.CARD.NO>

    COMI = ''; R.ACCOUNT = ''; ACCOUNT.ERR = ''
    CALL F.READ(FN.ACCOUNT,ACC.ID,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    IF NOT(R.ACCOUNT) THEN
        FT.HIST.ID = ACC.ID; ERRH.ACCOUNT = ''
        CALL EB.READ.HISTORY.REC(F.ACCOUNT.H,FT.HIST.ID,R.ACCOUNT,ERRH.ACCOUNT)
    END
    IF R.ACCOUNT THEN
        AC.CATEG = R.ACCOUNT<AC.CATEGORY>
*
        IF AC.CATEG GE '3000' AND AC.CATEG LE '3999' THEN
            ARR.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>
            IF NOT(ARR.ID) THEN
                FT.HIST.ID = ACC.ID:";1"; ERRH.ACCOUNT = ''
                CALL F.READ(FN.ACCOUNT.H,FT.HIST.ID,R.ACCOUNT,F.ACCOUNT.H,ACCOUNT.ERR)
                ARR.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>
            END
            IF ARR.ID THEN
                R.AA.ARRANGEMENT = ''; AA.ARRANGEMENT.ERR = ''
                CALL F.READ(FN.AA.ARRANGEMENT,ARR.ID,R.AA.ARRANGEMENT,F.AA.ARRANGEMENT,AA.ARRANGEMENT.ERR)
                GOSUB GET.AA.ARR
            END
        END

        LOCATE AC.CATEG IN Y.TXNCDE.VAL.ARR<1,1> SETTING TXNTE.POS THEN
            COMI = Y.TXNCDE.DIS.ARR<1,TXNTE.POS>
        END
    END

    IF R.ACCOUNT EQ '' AND YCARD.NO NE '' THEN
        AC.CATEG = YCARD.NO[1,6]
        LOCATE AC.CATEG IN Y.TXNTYE.VAL.ARR<1,1> SETTING TXNCC.POS THEN
            COMI = Y.TXNTYE.DIS.ARR<1,TXNCC.POS>
        END
    END
    RETURN

GET.AA.ARR:
**********
    EFF.DATE = TODAY; PROP.CLASS = 'ACCOUNT'
    PROPERTY = ''; R.AA.ARR.ACCOUNT = ''; ERR.MSG = ''
    CALL REDO.CRR.GET.CONDITIONS(ARR.ID,EFF.DATE,PROP.CLASS,PROPERTY,R.AA.ARR.ACCOUNT,ERR.MSG)
    IF R.AA.ARR.ACCOUNT THEN
        COMI = R.AA.ARR.ACCOUNT<AA.AC.LOCAL.REF,L.CR.FACILITY.POS>
    END
    RETURN

END