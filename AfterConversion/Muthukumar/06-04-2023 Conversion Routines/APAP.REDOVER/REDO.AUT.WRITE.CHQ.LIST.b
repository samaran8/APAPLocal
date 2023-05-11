* @ValidationCode : Mjo2MjI1NjE2NjA6Q3AxMjUyOjE2ODA3NzU4MDg4NjM6bXV0aHU6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 06 Apr 2023 15:40:08
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : muthu
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.AUT.WRITE.CHQ.LIST
*---------------------------------------------------------------------------------
* This is an authorisation routine for the Cheque version, the detail of issued cheques will
* be written to REDO.PRINT.CHQ.LIST for printing purpose
*----------------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHOROS Y PRESTAMOS
* Developed By  : SHANKAR RAJU
* ODR NUMBER    : ODR-2010-03-0447
*----------------------------------------------------------------------
*MODIFICATION DETAILS:
*---------------------
*   DATE           RESOURCE           REFERENCE             DESCRIPTION

* 09-03-2011     SHANKAR RAJU      ODR-2010-03-0447     Printing of Cheques
* 11-04-2011     Bharath G         PACS00032271         Update user and concept in REDO.PRINT.CHQ.LIST table
*19-sep-2011     JEEVA T           PACS00127058
* 09-APR-2012    NAVA V.    PACS00172913         Changing REDO.CONVERT.NUM.TO.WORDS instead of DE.O.PRINT.WORDS
*                                                       on "amount to letters" converting service routine.
* 06-04-2023	CONVERSION TOOL		AUTO R22 CODE CONVERSION	 FM to @FM, VM to @VM, SM to @SM, TNO to C$T24.SESSION.NO
* 06-04-2023	MUTHUKUMAR M		MANUAL R22 CODE CONVERSION	 NO CHANGE
*----------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
*
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.USER
*
    $INSERT I_F.REDO.PRINT.CHQ.LIST
    $INSERT I_F.REDO.CHQ.PRNT.VERSIONS

    $INSERT I_F.REDO.MANAGER.CHQ.PARAM
    $INSERT I_F.REDO.ADMIN.CHQ.PARAM


    GOSUB INITIALISE
    GOSUB CHK.PRINT
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END
*
RETURN
*
*----------------------------------------------------------------------------------
INITIALISE:
*----------

    PROCESS.GOAHEAD = 1

    FN.REDO.PRINT.CHQ.LIST = 'F.REDO.PRINT.CHQ.LIST'
    F.REDO.PRINT.CHQ.LIST  = ''
    CALL OPF(FN.REDO.PRINT.CHQ.LIST,F.REDO.PRINT.CHQ.LIST)

    FN.REDO.CHQ.PRNT.VERSIONS = 'F.REDO.CHQ.PRNT.VERSIONS'
    F.REDO.CHQ.PRNT.VERSIONS  = ''
    CALL OPF(FN.REDO.CHQ.PRNT.VERSIONS,F.REDO.CHQ.PRNT.VERSIONS)


    FN.REDO.MANAGER.CHQ.PARAM  = 'F.REDO.MANAGER.CHQ.PARAM'
    F.REDO.MANAGER.CHQ.PARAM   = ''
    CALL OPF(FN.REDO.MANAGER.CHQ.PARAM,F.REDO.MANAGER.CHQ.PARAM)

    FN.REDO.ADMIN.CHQ.PARAM   = 'F.REDO.ADMIN.CHQ.PARAM'
    F.REDO.ADMIN.CHQ.PARAM    =  ''
    CALL OPF(FN.REDO.ADMIN.CHQ.PARAM,F.REDO.ADMIN.CHQ.PARAM)

    CALL CACHE.READ(FN.REDO.MANAGER.CHQ.PARAM, 'SYSTEM',R.REDO.MANAGER.CHQ.PARAM, ADMIN.CHQ.ERR)
    CALL CACHE.READ(FN.REDO.ADMIN.CHQ.PARAM,'SYSTEM',R.REDO.ADMIN.CHQ.PARAM,MGR.CHQ.ERR)

    Y.MGR.ACCTS    = R.REDO.MANAGER.CHQ.PARAM<MAN.CHQ.PRM.ACCOUNT>
    Y.ADMIN.ACCTS  = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.ACCOUNT>
*
    LOC.REF.FIELDS      = 'L.TT.BENEFICIAR':@VM:'L.TT.BASE.AMT':@VM:'L.TT.CONCEPT':@FM:'L.FT.CONCEPT':@VM:'BENEFIC.NAME'
    LOC.REF.POS         = ''
    LOC.REF.APPLICATION = 'TELLER':@FM:'FUNDS.TRANSFER'

    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.TT.BENEFICIAR = LOC.REF.POS<1,1>
    POS.L.TT.BASE.AMT   = LOC.REF.POS<1,2>
    POS.L.TT.CONCEPT    = LOC.REF.POS<1,3>
*
    POS.L.FT.CONCEPT    = LOC.REF.POS<2,1>
    POS.BENEFIC.NAME    = LOC.REF.POS<2,2>
*
RETURN
*
*----------------------------------------------------------------------------------
CHK.PRINT:
*---------
*
    R.REDO.CHQ.PRNT.VERSIONS = ""
    PROCESS.GOAHEAD          = ""
*
    CALL CACHE.READ(FN.REDO.CHQ.PRNT.VERSIONS,"SYSTEM",R.REDO.CHQ.PRNT.VERSIONS,ERR.PR.VRSN)
    IF R.REDO.CHQ.PRNT.VERSIONS THEN
        Y.THIS.VERSION = APPLICATION:PGM.VERSION
        Y.ALL.VERSIONS = RAISE(R.REDO.CHQ.PRNT.VERSIONS<PRINT.CHQ.LIST.VERSIONS>)
        LOCATE Y.THIS.VERSION IN Y.ALL.VERSIONS<1> SETTING VER.POS THEN
            PROCESS.GOAHEAD = 1
        END
    END

RETURN
*----------------------------------------------------------------------------------
INIT.TELLER:
*-----------
*
    FN.TELLER = 'F.TELLER'
    F.TELLER  = ''
    CALL OPF(FN.TELLER,F.TELLER)
*
    Y.CHEQUE.AMOUNT = R.NEW(TT.TE.LOCAL.REF)<1,POS.L.TT.BASE.AMT>
*
RETURN
*
*----------------------------------------------------------------------------------
INIT.FT:
*-------
*
    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER  = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)
*
    Y.CHEQUE.AMOUNT = R.NEW(FT.DEBIT.AMOUNT)
*
RETURN
*
*----------------------------------------------------------------------------------
PROCESS:
*-------
*
    Y.REDO.PRINT.CHQ.LIST.ID = ID.NEW
*
    CON.DATE = OCONV(DATE(),"D-")
    Y.DATE.TIME = CON.DATE[9,2]:CON.DATE[1,2]:CON.DATE[4,2]:TIME.STAMP[1,2]:TIME.STAMP[4,2]
*
    IF APPLICATION EQ 'TELLER' THEN
        GOSUB INIT.TELLER
        GOSUB ASIGN.TELLER
    END ELSE
        GOSUB INIT.FT
        GOSUB ASSIGN.FT
    END
*
    GOSUB WRITE.PRINT.CHQ
*
RETURN
*----------------------------------------------------------------------------------
ASSIGN.FT:
*---------
*-----------Assign values if Application EQ 'FUNDS.TRANSFER'----------

    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CHEQUE.NO>     = R.NEW(FT.CREDIT.THEIR.REF)<1,1>
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.ISSUE.ACCOUNT> = R.NEW(FT.DEBIT.ACCT.NO)
    IF R.NEW(FT.BEN.CUSTOMER) THEN
        R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.BENEFICIARY>   = R.NEW(FT.BEN.CUSTOMER)
    END ELSE
        Y.BEN.NAME.LIST = R.NEW(FT.LOCAL.REF)<1,POS.BENEFIC.NAME>
        CHANGE @SM TO @FM IN Y.BEN.NAME.LIST
        R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.BENEFICIARY,1> = Y.BEN.NAME.LIST<1>
        R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.BENEFICIARY,2> = Y.BEN.NAME.LIST<2>
    END
    Y.FT.CR.ACCT.NUM = R.NEW(FT.CREDIT.ACCT.NO)
    LOCATE Y.FT.CR.ACCT.NUM IN Y.MGR.ACCTS<1,1> SETTING Y.MGR.POS THEN
        R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CHQ.TYPE> = 'MANAGER'
    END

    LOCATE Y.FT.CR.ACCT.NUM IN Y.ADMIN.ACCTS<1,1> SETTING Y.ADM.POS THEN
        R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CHQ.TYPE> = 'ADMIN'
    END


*   PACS00032271 - S

*    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CONCEPT,1>     = R.NEW(FT.LOCAL.REF)<1,POS.L.FT.CONCEPT>[1,35]
*    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CONCEPT,2>     = R.NEW(FT.LOCAL.REF)<1,POS.L.FT.CONCEPT>[36,35]

    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CONCEPT> = R.NEW(FT.LOCAL.REF)<1,POS.L.FT.CONCEPT>

*   PACS00032271 - E

RETURN
*
*----------------------------------------------------------------------------------
ASIGN.TELLER:
*------------
*-----------Assign values if Application EQ 'TELLER'------------------

    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CHEQUE.NO>     = R.NEW(TT.TE.CHEQUE.NUMBER)
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.ISSUE.ACCOUNT> = R.NEW(TT.TE.ACCOUNT.2)
    Y.BEN.NAME.LIST = R.NEW(TT.TE.LOCAL.REF)<1,POS.L.TT.BENEFICIAR>
    CHANGE @SM TO @FM IN Y.BEN.NAME.LIST
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.BENEFICIARY,1>   = Y.BEN.NAME.LIST<1>
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.BENEFICIARY,2>   = Y.BEN.NAME.LIST<2>

*   PACS00032271 - S

*    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CONCEPT,1>     = R.NEW(TT.TE.LOCAL.REF)<1,POS.L.TT.CONCEPT>[1,35]
*    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CONCEPT,2>     = R.NEW(TT.TE.LOCAL.REF)<1,POS.L.TT.CONCEPT>[36,35]
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CONCEPT> = R.NEW(TT.TE.LOCAL.REF)<1,POS.L.TT.CONCEPT>

*   PACS00032271 - E
*
    Y.TT.ACCT.1 = R.NEW(TT.TE.ACCOUNT.1)
    Y.TT.ACCT.2 = R.NEW(TT.TE.ACCOUNT.2)

    LOCATE Y.TT.ACCT.1 IN Y.MGR.ACCTS<1,1> SETTING Y.MGR.POS THEN
        R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CHQ.TYPE> = 'MANAGER'
    END

    LOCATE Y.TT.ACCT.2 IN Y.MGR.ACCTS<1,1> SETTING Y.MGR.POS THEN
        R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CHQ.TYPE> = 'MANAGER'
    END

    LOCATE Y.TT.ACCT.1 IN Y.ADMIN.ACCTS<1,1> SETTING Y.ADM.POS THEN
        R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CHQ.TYPE> = 'ADMIN'
    END

    LOCATE Y.TT.ACCT.2 IN Y.ADMIN.ACCTS<1,1> SETTING Y.ADM.POS THEN
        R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CHQ.TYPE> = 'ADMIN'
    END

RETURN
*
*----------------------------------------------------------------------------------
FIGURE.TO.WORDS:
*---------------
* In : Y.CHEQUE.AMOUNT
* Out: Y.AMT.IN.WRDS
*
    Y.DEBIT.AMT = Y.CHEQUE.AMOUNT
*
    OUT.AMT      = ''
    LANGUAGE     = 'ES'
    LINE.LENGTH  = 100
    NO.OF.LINES  = 1
    ERR.MSG      = ''
    IN.AMT = Y.DEBIT.AMT
    CALL REDO.CONVERT.NUM.TO.WORDS(IN.AMT, OUT.AMT, LINE.LENGTH, NO.OF.LINES, ERR.MSG)
    OUT.AMT = UPCASE(OUT.AMT)
*
    Y.AMT.IN.WRDS = OUT.AMT

RETURN
*----------------------------------------------------------------------------------
WRITE.PRINT.CHQ:
*---------------
*
    GOSUB FIGURE.TO.WORDS
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.AMOUNT>       = Y.CHEQUE.AMOUNT
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.AMOUNT.WORDS> = Y.AMT.IN.WRDS
*
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.PRINT>        = 'N'
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.USER>         = OPERATOR
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.ISSUE.DATE>   = TODAY
*
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CURR.NO>      = 1
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.INPUTTER>     = C$T24.SESSION.NO:'_':OPERATOR
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.AUTHORISER>   = C$T24.SESSION.NO:'_':OPERATOR
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.DATE.TIME>    = Y.DATE.TIME
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.CO.CODE>      = ID.COMPANY
    R.REDO.PRINT.CHQ.LIST<PRINT.CHQ.LIST.DEPT.CODE>    = R.USER<EB.USE.DEPARTMENT.CODE>

    CALL F.WRITE(FN.REDO.PRINT.CHQ.LIST,Y.REDO.PRINT.CHQ.LIST.ID,R.REDO.PRINT.CHQ.LIST)

RETURN
*----------------------------------------------------------------------------------
END
