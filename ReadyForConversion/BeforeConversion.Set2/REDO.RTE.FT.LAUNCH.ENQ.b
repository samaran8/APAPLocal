*-----------------------------------------------------------------------------
* <Rating>-24</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.RTE.FT.LAUNCH.ENQ
*----------------------------------------------------------------------------------------------------------------------
* Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By      : APAP
* Program   Name    : REDO.RTE.FT.LAUNCH.ENQ
*----------------------------------------------------------------------------------------------------------------------
* Description       : Routine to refresh all the enquiries displayed in CASHOFF.REDO.COS cos screen
* Linked With       : VERSION.CONTROL FT
* In  Parameter     : N/A
* Out Parameter     : N/A
* Files  Used       : FUNDS.TRANSFER
*----------------------------------------------------------------------------------------------------------------------
* Modification Details:
* =====================
* Date         Who                  Reference      Description
* ------       -----                ------------   -------------
* 20-01-2017   APAP                 RTE FIXES      Initial Version
*----------------------------------------------------------------------------------------------------------------------
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_S.COMMON
    $INSERT I_GTS.COMMON
    $INSERT I_F.ENQUIRY
    $INSERT I_BROWSER.TAGS
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT TAM.BP I_F.REDO.H.REPORTS.PARAM

    GOSUB INITIALISE
    GOSUB PROCESS
*
    RETURN
*
*-------
PROCESS:
*-------
*
    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM"
    F.REDO.H.REPORTS.PARAM  = ""
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)
    R.REDO.H.REPORTS.PARAM = ''
    RTE.PARAM.ERR = ''
    RTE.PARAM.ID = 'REDO.RTE.FORM'
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,RTE.PARAM.ID,R.REDO.H.REPORTS.PARAM,RTE.PARAM.ERR)

    IF R.REDO.H.REPORTS.PARAM THEN
        Y.FIELD.NME.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.NAME>
        Y.FIELD.VAL.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE>
        Y.DISP.TEXT.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.DISPLAY.TEXT>
    END
    LOCATE "RTE.VERSIONS" IN Y.FIELD.NME.ARR<1,1> SETTING RTE.VER.POS THEN
        Y.RTE.VERSIONS = Y.FIELD.VAL.ARR<1,RTE.VER.POS>
    END
    Y.RTE.VERSIONS = CHANGE(Y.RTE.VERSIONS,SM,VM)
    IF COMI[1,2] EQ 'FT' THEN
        GET.APPLICATION = 'FUNDS.TRANSFER'
    END

    Y.CURRENT.VERSION = GET.APPLICATION:PGM.VERSION

    CALL F.READ(FN.FUNDS.TRANSFER.NAU,ID.NEW,R.FUNDS.TRANSFER.NAU,F.FUNDS.TRANSFER.NAU,FT.ERR)
    LOCATE Y.CURRENT.VERSION IN Y.RTE.VERSIONS<1,1> SETTING RTE.VER.POS ELSE
        IF R.FUNDS.TRANSFER.NAU AND V$FUNCTION EQ 'I' AND OFS$SOURCE.ID NE 'FASTPATH' THEN
            E = 'EB-CANNOT.AMEND.EXISTING.RECORD'
            CALL ERR
        END
    END

    GET.FT.CRD.ACCT = R.NEW(FT.CREDIT.ACCT.NO)

    Y.GET.AUTHORISOR = R.NEW(FT.AUTHORISER)
    Y.GET.RECORD.STATUS = R.NEW(FT.RECORD.STATUS)

    IF Y.COS.NAME[1,4] EQ Y.CASHIER.COS OR Y.COS.NAME[1,11] EQ Y.SUPER.COS OR Y.COS.NAME[1,11] EQ Y.BUSS.COS ELSE
        RETURN
    END

    IF ALPHA(GET.FT.CRD.ACCT[1,3]) THEN
        GET.FT.CRD.ACCT = R.NEW(FT.DEBIT.ACCT.NO)
        IF ALPHA(GET.FT.CRD.ACCT[1,3]) THEN
            RETURN
        END
    END


*    GET.ENQ.LIST = 'ENQ REDO.ACCT.JHOLDER @ID EQ ':GET.FT.CRD.ACCT
*    GET.ENQ.LIST<-1> = 'ENQ REDO.ENQ.RBHP.PADRONE ACCOUNT.NO EQ ':GET.FT.CRD.ACCT
*    GET.ENQ.LIST<-1> = 'ENQ REDO.IM.CONSULTA.FIRMAS IMAGE.REFERENCE EQ ':GET.FT.CRD.ACCT

    IF Y.COS.NAME[1,11] EQ Y.SUPER.COS THEN
*        GET.ENQ.LIST<-1> = 'ENQ REDO.CUST.ACCT.FULL.CASH @ID EQ ':GET.FT.CRD.ACCT
    END

    I = 1 ; Y.ENQ.CNT = DCOUNT(GET.ENQ.LIST,FM) ;
    LOOP
    WHILE I LE Y.ENQ.CNT
        Y.NEXT.TASK = ''
        Y.NEXT.TASK = GET.ENQ.LIST<I>
        CALL EB.SET.NEW.TASK(Y.NEXT.TASK)
        I += 1
    REPEAT

    RETURN
*
* ---------
INITIALISE:
* ---------
*
    Y.COS.NAME = OFS$WINDOW.NAME

    Y.CASHIER.COS = 'TXNS'
    Y.SUPER.COS = 'OFFICERTXNS'
    Y.BUSS.COS = 'BUSINESSTXN'
*
    FN.FUNDS.TRANSFER.NAU = 'F.FUNDS.TRANSFER$NAU'
    F.FUNDS.TRANSFER.NAU = ''
    CALL OPF(FN.FUNDS.TRANSFER.NAU,F.FUNDS.TRANSFER.NAU)
*
    RETURN
*
END
