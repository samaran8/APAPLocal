* @ValidationCode : MjoxOTI2NjQ2NzU2OkNwMTI1MjoxNjgxMTA0MDk2ODQ5OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 10 Apr 2023 10:51:36
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
SUBROUTINE  REDO.V.AUT.RAISE.TTENTRY
*-------------------------------------------------------------------------
*DESCRIPTION:
*~~~~~~~~~~~~
* This routine is attached as the authorisation routine for the versions following:
*TELLER, CHQ.TAX
*TELLER, CHQ.OTHERS
*TELLER, CASH.CHQ
*TELLER, REINSTATE

* This routine is to raise statement and categ entries wheh the field WAIVE.TAX
* is set to YES
*-------------------------------------------------------------------------
*DEVELOPMENT DETAILS:
*~~~~~~~~~~~~~~~~~~~~
*
*   Date               who           Reference            Description
*   ~~~~               ~~~           ~~~~~~~~~            ~~~~~~~~~~~
* 22-APR-2010     SHANKAR RAJU     ODR-2010-03-0447     Initial Creation
*-------------------------------------------------------------------------

*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*10-04-2023            Conversion Tool             R22 Auto Code conversion                VM TO @VM, F.READ TO CACHE.READ,START.LOOP+1 TO +=1
*10-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*----------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.TELLER
    $INSERT I_F.FT.COMMISSION.TYPE
    $INSERT I_F.FT.CHARGE.TYPE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.USER
    $INSERT I_F.REDO.ADMIN.CHQ.PARAM
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.CATEG.ENTRY
    $INSERT I_F.TAX
    $INSERT I_F.TELLER.TRANSACTION

    GOSUB INITIALISE
    GOSUB PROCESS

RETURN
*-------------------------------------------------------------------------
INITIALISE:
*~~~~~~~~~~
*Initialise the Variables
    EB.ACCT.ERR.MSG = ''

    FN.TELLER = 'F.TELLER'
    F.TELLER = ''
    R.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.TELLER.TRANSACTION = 'F.TELLER.TRANSACTION'
    F.TELLER.TRANSACTION = ''
    R.TELLER.TRANSACTION = ''
    CALL OPF(FN.TELLER.TRANSACTION,F.TELLER.TRANSACTION)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    R.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.TAX = 'F.TAX'
    F.TAX = ''
    R.TAX = ''
    CALL OPF(FN.TAX,F.TAX)

    FN.REDO.ADMIN.CHQ.PARAM = 'F.REDO.ADMIN.CHQ.PARAM'
    F.REDO.ADMIN.CHQ.PARAM = ''
    R.REDO.ADMIN.CHQ.PARAM = ''
    CALL OPF(FN.REDO.ADMIN.CHQ.PARAM,F.REDO.ADMIN.CHQ.PARAM)
    CALL CACHE.READ(FN.REDO.ADMIN.CHQ.PARAM,'SYSTEM',R.REDO.ADMIN.CHQ.PARAM,ERR.PAR)

    FN.FT.COMMISSION.TYPE = 'F.FT.COMMISSION.TYPE'
    F.FT.COMMISSION.TYPE = ''
    R.FT.COMMISSION.TYPE = ''
    CALL OPF(FN.FT.COMMISSION.TYPE,F.FT.COMMISSION.TYPE)

    LOC.REF.APPLICATION="TELLER"
    LOC.REF.FIELDS='WAIVE.TAX':@VM:'L.TT.WAI.CHARGE'
    LOC.REF.POS=''

    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LREF.POS)
    POS.WAIVE.TAX = LREF.POS<1,1>
    POS.WAIVE.CHARGE = LREF.POS<1,2>

RETURN
*-------------------------------------------------------------------------
PROCESS:
*~~~~~~~

    Y.TELLER.TRANSACTION = R.NEW(TT.TE.TRANSACTION.CODE)
    Y.DEBIT.AC.NO = R.NEW(TT.TE.ACCOUNT.1)<1,1>

    CALL F.READ(FN.ACCOUNT,Y.DEBIT.AC.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACC)
    Y.DEBIT.CUSTOMER = R.ACCOUNT<AC.CUSTOMER>
    Y.ACCT.OFFICER = R.ACCOUNT<AC.ACCOUNT.OFFICER>
    Y.DB.AC.CATEG = R.ACCOUNT<AC.CATEGORY>

    Y.ACCOUNT.2 = R.NEW(TT.TE.ACCOUNT.2)
    Y.ALL.AC.NOS = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.ACCOUNT>
    Y.ALL.TAX.KEYS = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.TAX.KEY>
    LOCATE Y.ACCOUNT.2 IN Y.ALL.AC.NOS<1,1> SETTING POS1 THEN

        Y.TAX.KEY = Y.ALL.TAX.KEYS<1,POS1>

    END

    CALL CACHE.READ(FN.FT.COMMISSION.TYPE, Y.TAX.KEY, R.FT.COMMISSION.TYPE, ERR.FTCT)    ;*R22 AUTO CODE CONVERSION

    Y.PL.CATEG.CREDIT = R.FT.COMMISSION.TYPE<FT4.CATEGORY.ACCOUNT>

    CALL CACHE.READ(FN.TELLER.TRANSACTION, Y.TELLER.TRANSACTION, R.TELLER.TRANSACTION, ERR.TT)     ;*R22 AUTO CODE CONVERSION

    Y.CHARGE.CODES = R.TELLER.TRANSACTION<TT.TR.CHARGE.CODE>

    Y.WAIVE.TAX = R.NEW(TT.TE.LOCAL.REF)<1,POS.WAIVE.TAX>
    Y.WAIVE.CHARGE = R.NEW(TT.TE.LOCAL.REF)<1,POS.WAIVE.CHARGE>

    BEGIN CASE

        CASE (Y.WAIVE.CHARGE EQ 'NO' OR Y.WAIVE.CHARGE EQ '') AND Y.WAIVE.TAX EQ 'YES'

            IF V$FUNCTION EQ 'I' OR V$FUNCTION EQ 'A' THEN

                GOSUB PROCESS.CHARGE.ENTRIES
                GOSUB PROCESS.WAIVE.TAX

            END

        CASE Y.WAIVE.CHARGE EQ 'YES' AND Y.WAIVE.TAX EQ 'YES'
            IF V$FUNCTION EQ 'I' OR V$FUNCTION EQ 'A' THEN

                GOSUB PROCESS.WAIVE.TAX

            END

        CASE Y.WAIVE.CHARGE EQ 'YES' AND Y.WAIVE.TAX EQ 'NO'
            IF V$FUNCTION EQ 'I' OR V$FUNCTION EQ 'A' THEN

                GOSUB PROCESS.TAX.ENTRIES

            END

        CASE Y.WAIVE.CHARGE EQ 'NO' AND Y.WAIVE.TAX EQ 'NO'
            IF V$FUNCTION EQ 'I' OR V$FUNCTION EQ 'A' THEN

                GOSUB PROCESS.CHARGE.ENTRIES
                GOSUB PROCESS.TAX.ENTRIES

            END


    END CASE

RETURN
*-------------------------------------------------------------------------
PROCESS.CHARGE.ENTRIES:
*~~~~~~~~~~~~~~~~~~~~~~

    Y.NO.CHARGE = DCOUNT(Y.CHARGE.CODES,@VM)

    START.LOOP = 1
    LOOP
    WHILE START.LOOP LE Y.NO.CHARGE

        IF R.TELLER.TRANSACTION<TT.TR.CHARGE.CODE,START.LOOP> NE Y.TAX.KEY THEN

            Y.CHARGE.CODE = R.TELLER.TRANSACTION<TT.TR.CHARGE.CODE,START.LOOP>

            GOSUB PROCESS.RAISE.CHARGE

        END

        START.LOOP += 1   ;*R22 AUTO CODE CONVERSION

    REPEAT

RETURN
*-------------------------------------------------------------------------
PROCESS.TAX.ENTRIES:
*~~~~~~~~~~~~~~~~~~~

    Y.NO.CHARGE = DCOUNT(Y.CHARGE.CODES,@VM)

    START.LOOP = 1
    LOOP
    WHILE START.LOOP LE Y.NO.CHARGE

        IF R.TELLER.TRANSACTION<TT.TR.CHARGE.CODE,START.LOOP> EQ Y.TAX.KEY THEN

            GOSUB PROCESS.RAISE.TAX

        END

        START.LOOP += 1    ;*R22 AUTO CODE CONVERSION

    REPEAT
RETURN
*-------------------------------------------------------------------------
PROCESS.RAISE.CHARGE:
*~~~~~~~~~~~~~~~~~~~~

    CALL CACHE.READ(FN.FT.COMMISSION.TYPE, Y.CHARGE.CODE, R.FT.COMMISSION.TYPE, ERR.FTCT)      ;*R22 AUTO CODE CONVERSION

    Y.PL.CATEG.CREDIT = R.FT.COMMISSION.TYPE<FT4.CATEGORY.ACCOUNT>

RETURN
*-------------------------------------------------------------------------
PROCESS.RAISE.TAX:
*~~~~~~~~~~~~~~~~~

RETURN
*-------------------------------------------------------------------------
PROCESS.WAIVE.TAX:
*~~~~~~~~~~~~~~~~~

    Y.DEBIT.CURRENCY = R.NEW(TT.TE.CURRENCY.1)
    Y.TAX.AMT = R.NEW(TT.TE.NARRATIVE.2)

    Y.ACCOUNT.2 = R.NEW(TT.TE.ACCOUNT.2)
    Y.ALL.AC.NOS = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.ACCOUNT>
    Y.ALL.CATEG = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.CATEGORY>
    LOCATE Y.ACCOUNT.2 IN Y.ALL.AC.NOS<1,1> SETTING POS3 THEN

        Y.PL.CATEG.DEBIT = Y.ALL.CATEG<1,POS3>

    END

    R.CATEG.ENT = ''
    R.CATEG.ENT<AC.CAT.ACCOUNT.NUMBER> = ''
    R.CATEG.ENT<AC.CAT.COMPANY.CODE> = ID.COMPANY

    IF V$FUNCTION EQ 'I' OR V$FUNCTION EQ 'A' THEN
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

    R.CATEG.ENT<AC.CAT.TRANSACTION.CODE> = 213
    R.CATEG.ENT<AC.CAT.CUSTOMER.ID> = Y.DEBIT.CUSTOMER
    R.CATEG.ENT<AC.CAT.DEPARTMENT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>
    R.CATEG.ENT<AC.CAT.PL.CATEGORY> = Y.PL.CATEG.DEBIT
    R.CATEG.ENT<AC.CAT.ACCOUNT.OFFICER> = Y.ACCT.OFFICER
    R.CATEG.ENT<AC.CAT.PRODUCT.CATEGORY> = Y.DB.AC.CATEG
    R.CATEG.ENT<AC.CAT.VALUE.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.CURRENCY> = Y.DEBIT.CURRENCY
    R.CATEG.ENT<AC.CAT.OUR.REFERENCE> = ID.NEW
    R.CATEG.ENT<AC.CAT.TRANS.REFERENCE> = ID.NEW
    R.CATEG.ENT<AC.CAT.SYSTEM.ID> = "TT"
    R.CATEG.ENT<AC.CAT.BOOKING.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.EXPOSURE.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.CURRENCY.MARKET> = "1"

    MULTI.STMT = ''
    MULTI.STMT<-1> = LOWER(R.CATEG.ENT)

    R.CATEG.ENT = ''
    R.CATEG.ENT<AC.CAT.ACCOUNT.NUMBER> = ''
    R.CATEG.ENT<AC.CAT.COMPANY.CODE> = ID.COMPANY

    IF V$FUNCTION EQ 'I' OR V$FUNCTION EQ 'A' THEN
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

    R.CATEG.ENT<AC.CAT.TRANSACTION.CODE> = 213
    R.CATEG.ENT<AC.CAT.CUSTOMER.ID> = Y.DEBIT.CUSTOMER
    R.CATEG.ENT<AC.CAT.DEPARTMENT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>
    R.CATEG.ENT<AC.CAT.PL.CATEGORY> = Y.PL.CATEG.CREDIT
    R.CATEG.ENT<AC.CAT.ACCOUNT.OFFICER> = Y.ACCT.OFFICER
    R.CATEG.ENT<AC.CAT.PRODUCT.CATEGORY> = Y.DB.AC.CATEG
    R.CATEG.ENT<AC.CAT.VALUE.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.CURRENCY> = Y.DEBIT.CURRENCY
    R.CATEG.ENT<AC.CAT.OUR.REFERENCE> = ID.NEW
    R.CATEG.ENT<AC.CAT.TRANS.REFERENCE> = ID.NEW
    R.CATEG.ENT<AC.CAT.SYSTEM.ID> = "TT"
    R.CATEG.ENT<AC.CAT.BOOKING.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.EXPOSURE.DATE> = TODAY
    R.CATEG.ENT<AC.CAT.CURRENCY.MARKET> = "1"

    MULTI.STMT<-1> = LOWER(R.CATEG.ENT)

    V = TT.TE.AUDIT.DATE.TIME
    CALL EB.ACCOUNTING.WRAPPER("TT","SAO",MULTI.STMT,'',EB.ACCT.ERR.MSG)

RETURN
*-------------------------------------------------------------------------
END
