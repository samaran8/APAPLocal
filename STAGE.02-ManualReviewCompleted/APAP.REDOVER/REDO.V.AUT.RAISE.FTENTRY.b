* @ValidationCode : MjotMTk4NDI4MDAwNDpDcDEyNTI6MTY4MTI4Njc5ODc2MTo5MTYzODotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 12 Apr 2023 13:36:38
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 91638
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE  REDO.V.AUT.RAISE.FTENTRY
*-------------------------------------------------------------------------
*DESCRIPTION:
*~~~~~~~~~~~~
* This routine is attached as the authorisation routine for the versions following:
*
* FUNDS.TRANSFER, CHQ.TAX
* FUNDS.TRANSFER, CHQ.OTHERS
* FUNDS.TRANSFER, REVERSE.CHQ
* FUNDS.TRANSFER, REINSTATE

* This routine is to raise statement and categ entries wheh the field L.FT.WAIVE.TAX
* is set to YES
*-------------------------------------------------------------------------
*DEVELOPMENT DETAILS:
*~~~~~~~~~~~~~~~~~~~~
*
*   Date               who           Reference            Description
*   ~~~~               ~~~           ~~~~~~~~~            ~~~~~~~~~~~
* 22-APR-2010     SHANKAR RAJU     ODR-2010-03-0447     Initial Creation
* 15-03-2011       SUDHARSANAN S   PACS00023910      Check the WAIVE.TAX Value
*Modification history
*Date                Who               Reference                  Description
*12-04-2023      conversion tool     R22 Auto code conversion     VM TO @VM
*12-04-2023      Mohanraj R          R22 Manual code conversion   No changes
*-------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.FT.COMMISSION.TYPE
    $INSERT I_F.FT.CHARGE.TYPE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.USER
    $INSERT I_F.REDO.ADMIN.CHQ.PARAM
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.CATEG.ENTRY
*   $INSERT I_F.FT.COMMISSION.TYPE
    GOSUB INITIALISE
    GOSUB PROCESS

RETURN
*-------------------------------------------------------------------------
INITIALISE:
*~~~~~~~~~~
*Initialise the Variables

    EB.ACCT.ERR.MSG = ''

    FN.FT.COMMISSION.TYPE = 'F.FT.COMMISSION.TYPE'
    F.FT.COMMISSION.TYPE = ''
    R.FT.COMMISSION.TYPE = ''
    CALL OPF(FN.FT.COMMISSION.TYPE,F.FT.COMMISSION.TYPE)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    R.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.FT.COMMISSION.TYPE = 'F.FT.COMMISSION.TYPE'
    F.FT.COMMISSION.TYPE = ''
    R.FT.COMMISSION.TYPE = ''
    CALL OPF(FN.FT.COMMISSION.TYPE,F.FT.COMMISSION.TYPE)

    FN.FT.CHARGE.TYPE = 'F.FT.CHARGE.TYPE'
    F.FT.CHARGE.TYPE = ''
    R.FT.CHARGE.TYPE = ''
    CALL OPF(FN.FT.CHARGE.TYPE,F.FT.CHARGE.TYPE)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    R.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.ADMIN.CHQ.PARAM = 'F.REDO.ADMIN.CHQ.PARAM'
    F.REDO.ADMIN.CHQ.PARAM = ''
    R.REDO.ADMIN.CHQ.PARAM = ''
    CALL OPF(FN.REDO.ADMIN.CHQ.PARAM,F.REDO.ADMIN.CHQ.PARAM)

    LREF.APPLN = 'FUNDS.TRANSFER'
    LREF.FLDS = 'WAIVE.TAX':@VM:'L.FT.WAI.TAXAMT'
    LREF.POS = ''

    CALL MULTI.GET.LOC.REF(LREF.APPLN,LREF.FLDS,LREF.POS)
    POS.WAIVE.TAX = LREF.POS<1,1>
    POS.WAIVE.AMT = LREF.POS<1,2>

RETURN
*-------------------------------------------------------------------------
PROCESS:
*~~~~~~~


    Y.DEBIT.AC.NO = R.NEW(FT.DEBIT.ACCT.NO)
    Y.DEBIT.CURRENCY = R.NEW(FT.DEBIT.CURRENCY)

    Y.CREDIT.AC = R.NEW(FT.CREDIT.ACCT.NO)

    CALL CACHE.READ(FN.REDO.ADMIN.CHQ.PARAM,'SYSTEM',R.REDO.ADMIN.CHQ.PARAM,ERR.PAR)
    Y.ALL.AC.NOS = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.ACCOUNT>
    Y.ALL.TAX.KEY = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.TAX.KEY>
    Y.ALL.CATEG = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.CATEGORY>

    LOCATE Y.CREDIT.AC IN Y.ALL.AC.NOS<1,1> SETTING POS3 THEN

        Y.TAX.KEY = Y.ALL.TAX.KEY<1,POS3>

    END

    Y.TEMP.AMT = R.NEW(FT.LOCAL.REF)<1,POS.WAIVE.AMT>

    Y.TAX.AMT = Y.TEMP.AMT[4,10]

    IF Y.TAX.AMT LE 0 OR Y.TAX.AMT EQ '' THEN
        RETURN

    END

    CALL F.READ(FN.FT.COMMISSION.TYPE,Y.TAX.KEY,R.FT.COMMISSION.TYPE,F.FT.COMMISSION.TYPE,ERR.FTCT)
    Y.PL.CATEG.CREDIT = R.FT.COMMISSION.TYPE<FT4.CATEGORY.ACCOUNT>

    CALL F.READ(FN.ACCOUNT,Y.DEBIT.AC.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACC)
    Y.DEBIT.CUSTOMER = R.ACCOUNT<AC.CUSTOMER>
    Y.ACCT.OFFICER = R.ACCOUNT<AC.ACCOUNT.OFFICER>
    Y.DB.AC.CATEG = R.ACCOUNT<AC.CATEGORY>

    LOCATE Y.CREDIT.AC IN Y.ALL.AC.NOS<1,1> SETTING POS3 THEN

        Y.PL.CATEG.DEBIT = Y.ALL.CATEG<1,POS3>

    END

    VAR.REC.STAT = R.NEW(FT.RECORD.STATUS)

    IF V$FUNCTION EQ 'A' OR V$FUNCTION EQ 'I' THEN
*PACS00023910 - S

        IF R.NEW(FT.LOCAL.REF)<1,POS.WAIVE.TAX> EQ 'YES' THEN

            GOSUB RAISE.CATG.ENTRIES

        END
*PACS00023910 - E
    END
RETURN
*-------------------------------------------------------------------------
RAISE.CATG.ENTRIES:
*~~~~~~~~~~~~~~~~~~
*Raising CATEG ENTRY

    R.CATEG.ENT = ''
    R.CATEG.ENT<AC.CAT.ACCOUNT.NUMBER> = ''
    R.CATEG.ENT<AC.CAT.COMPANY.CODE> = ID.COMPANY

    IF (V$FUNCTION EQ 'A'  OR V$FUNCTION EQ 'I') AND VAR.REC.STAT NE 'RNAU' THEN
        IF Y.DEBIT.CURRENCY EQ LCCY THEN
            R.CATEG.ENT<AC.CAT.AMOUNT.LCY> = Y.TAX.AMT
            R.CATEG.ENT<AC.CAT.AMOUNT.FCY> = ''
        END ELSE
            Y.CCY = Y.DEBIT.CURRENCY
            Y.AMT = Y.TAX.AMT
            TARGET.CCY = LCCY
            CALL EXCHRATE('1',Y.CCY,Y.AMT,TARGET.CCY,LCCY.TOT.AMT,'','','','','')
            R.CATEG.ENT<AC.CAT.AMOUNT.LCY> = LCCY.TOT.AMT
            R.CATEG.ENT<AC.CAT.AMOUNT.FCY> = Y.TAX.AMT
        END
    END ELSE
        IF (VAR.REC.STAT EQ 'RNAU'  OR V$FUNCTION EQ 'R') OR (PGM.VERSION EQ ',REINSTATE' OR PGM.VERSION EQ ',REVERSE.CHQ') THEN
            IF Y.DEBIT.CURRENCY EQ LCCY THEN
                R.CATEG.ENT<AC.CAT.AMOUNT.LCY> = -1 * Y.TAX.AMT
                R.CATEG.ENT<AC.CAT.AMOUNT.FCY> = ''
            END ELSE
                Y.CCY = Y.DEBIT.CURRENCY
                Y.AMT = Y.TAX.AMT
                TARGET.CCY = LCCY
                CALL EXCHRATE('1',Y.CCY,Y.AMT,TARGET.CCY,LCCY.TOT.AMT,'','','','','')
                R.CATEG.ENT<AC.CAT.AMOUNT.LCY> = -1 * LCCY.TOT.AMT
                R.CATEG.ENT<AC.CAT.AMOUNT.FCY> = -1 * Y.TAX.AMT
            END
        END
    END
    R.CATEG.ENT<AC.CAT.TRANSACTION.CODE> = 213
* R.CATEG.ENT<AC.CAT.CUSTOMER.ID> = Y.DEBIT.CUSTOMER
    R.CATEG.ENT<AC.CAT.DEPARTMENT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>
    R.CATEG.ENT<AC.CAT.ACCOUNT.OFFICER> = Y.ACCT.OFFICER
    R.CATEG.ENT<AC.CAT.PL.CATEGORY> = Y.PL.CATEG.CREDIT
    R.CATEG.ENT<AC.CAT.PRODUCT.CATEGORY> = Y.DB.AC.CATEG
    R.CATEG.ENT<AC.CAT.VALUE.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.CURRENCY> = Y.DEBIT.CURRENCY
    R.CATEG.ENT<AC.CAT.OUR.REFERENCE> = ID.NEW
    R.CATEG.ENT<AC.CAT.TRANS.REFERENCE> = ID.NEW
    R.CATEG.ENT<AC.CAT.SYSTEM.ID> = "FT"
    R.CATEG.ENT<AC.CAT.BOOKING.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.EXPOSURE.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.CURRENCY.MARKET> = "1"

    MULTI.STMT=''
    MULTI.STMT<-1> = LOWER(R.CATEG.ENT)
*******************************************
    R.CATEG.ENT = ''
    R.CATEG.ENT<AC.CAT.ACCOUNT.NUMBER> = ''
    R.CATEG.ENT<AC.CAT.COMPANY.CODE> = ID.COMPANY

    IF (V$FUNCTION EQ 'A'  OR V$FUNCTION EQ 'I') AND VAR.REC.STAT NE 'RNAU' THEN
        IF Y.DEBIT.CURRENCY EQ LCCY THEN
            R.CATEG.ENT<AC.CAT.AMOUNT.LCY> = -1 * Y.TAX.AMT
            R.CATEG.ENT<AC.CAT.AMOUNT.FCY> = ''
        END ELSE
            Y.CCY = Y.DEBIT.CURRENCY
            Y.AMT = Y.TAX.AMT
            TARGET.CCY = LCCY
            CALL EXCHRATE('1',Y.CCY,Y.AMT,TARGET.CCY,LCCY.TOT.AMT,'','','','','')
            R.CATEG.ENT<AC.CAT.AMOUNT.LCY> = -1* LCCY.TOT.AMT
            R.CATEG.ENT<AC.CAT.AMOUNT.FCY> = -1 * Y.TAX.AMT
        END
    END ELSE
        IF (VAR.REC.STAT EQ 'RNAU'  OR V$FUNCTION EQ 'R') OR (PGM.VERSION EQ ',REINSTATE' OR PGM.VERSION EQ ',REVERSE.CHQ') THEN
            IF Y.DEBIT.CURRENCY EQ LCCY THEN
                R.CATEG.ENT<AC.CAT.AMOUNT.LCY> = Y.TAX.AMT
                R.CATEG.ENT<AC.CAT.AMOUNT.FCY> = ''
            END ELSE
                Y.CCY = Y.DEBIT.CURRENCY
                Y.AMT = Y.TAX.AMT
                TARGET.CCY = LCCY
                CALL EXCHRATE('1',Y.CCY,Y.AMT,TARGET.CCY,LCCY.TOT.AMT,'','','','','')
                R.CATEG.ENT<AC.CAT.AMOUNT.LCY> = LCCY.TOT.AMT
                R.CATEG.ENT<AC.CAT.AMOUNT.FCY> = Y.TAX.AMT
            END
        END
    END

    R.CATEG.ENT<AC.CAT.TRANSACTION.CODE> = 213
* R.CATEG.ENT<AC.CAT.CUSTOMER.ID> = Y.DEBIT.CUSTOMER
    R.CATEG.ENT<AC.CAT.DEPARTMENT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>
    R.CATEG.ENT<AC.CAT.ACCOUNT.OFFICER> = Y.ACCT.OFFICER
    R.CATEG.ENT<AC.CAT.PL.CATEGORY> = Y.PL.CATEG.DEBIT
    R.CATEG.ENT<AC.CAT.PRODUCT.CATEGORY> = Y.DB.AC.CATEG
    R.CATEG.ENT<AC.CAT.VALUE.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.CURRENCY> = Y.DEBIT.CURRENCY
    R.CATEG.ENT<AC.CAT.OUR.REFERENCE> = ID.NEW
    R.CATEG.ENT<AC.CAT.TRANS.REFERENCE> = ID.NEW
    R.CATEG.ENT<AC.CAT.SYSTEM.ID> = "FT"
    R.CATEG.ENT<AC.CAT.BOOKING.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.EXPOSURE.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.CURRENCY.MARKET> = "1"

    MULTI.STMT<-1> = LOWER(R.CATEG.ENT)

    V = FT.AUDIT.DATE.TIME
*******UPDATE STMT.NOS**********************
    IF APPLICATION EQ 'FUNDS.TRANSFER' THEN
        Y.TEMP.STMT.ARRAY = R.NEW(FT.STMT.NOS)
    END

    CALL EB.ACCOUNTING.WRAPPER("FT","SAO",MULTI.STMT,'',EB.ACCT.ERR.MSG)

    IF APPLICATION EQ 'FUNDS.TRANSFER' THEN
        IF Y.TEMP.STMT.ARRAY<1,3> NE '' THEN
            Y.TEMP.STMT.ARRAY<1,4> = ID.COMPANY
            Y.TEMP.STMT.ARRAY<1,5> = R.NEW(FT.STMT.NOS)
        END ELSE
            Y.TEMP.STMT.ARRAY<1,3> = ID.COMPANY
            Y.TEMP.STMT.ARRAY<1,4> = R.NEW(FT.STMT.NOS)
        END
        R.NEW(FT.STMT.NOS) = Y.TEMP.STMT.ARRAY
    END

*******UPDATE STMT.NOS**********************


RETURN
*-------------------------------------------------------------------------
END
