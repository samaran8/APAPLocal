* @ValidationCode : MjotODAyMDYxNTE0OkNwMTI1MjoxNjgxMzcxOTU0ODQwOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 13:15:54
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*12-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*12-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
SUBROUTINE REDO.CUS.CASH.WTH(CASH.WTH)
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This development is for ODR Reference ODR-2009-10-0547
* This is field format routine for the deal slip to return the cash withdrawn
* Input/Output:
*--------------
* IN :CUSTOMER.ID
* OUT : CASH.WTH
*
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------------------------------------------------------------------------------
*   Date               who           Reference            Description
* 28-DEC-2009      SHANKAR RAJU                            Initial Creation
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.DEAL.SLIP.FORMAT
    $INSERT I_F.TELLER

    GOSUB INIT
    GOSUB PROCESS

RETURN

*****
INIT:
*****
    FLAG  = 1
    CNT = 0
    PROD.ACCT.NO = ''
    AMT.TRANS = ''

    FN.ACCOUNT = 'F.ACCOUNT'
    FN.DEAL.SLIP.FORMAT = 'F.DEAL.SLIP.FORMAT'
    FN.TELLER = 'F.TELLER'
    FN.TELLER.HIS = 'F.TELLER$HIS'
    F.TELLER.HIS = ''

    F.DEAL.SLIP.FORMAT = ''
    F.ACCOUNT = ''
    F.TELLER = ''

    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.DEAL.SLIP.FORMAT,F.DEAL.SLIP.FORMAT)
    CALL OPF(FN.TELLER,F.TELLER)
    CALL OPF(FN.TELLER.HIS,F.TELLER.HIS)

    CUS.ID = ID.NEW
    NO.OF.ACC = ''
    COUNT1=''
    COUNT2=''
    COUNT3=''
    COUNT4=''
    COUNT5=''

RETURN
**********

********
PROCESS:
********

    GOSUB TELLER.CHECK

    CASH.WTH = FMT(COUNT1,"L#13"):FMT(COUNT2,"L#20")
    CASH.WTH := FMT(COUNT3,"L#20"):FMT(COUNT4,"L#20"):FMT(COUNT5,"L#20")

RETURN
**********

*************
TELLER.CHECK:
*************
*CHECK FOR TELLER TRANSACTIONS

    SEL.CMD = "SELECT ":FN.ACCOUNT:" WITH CUSTOMER EQ ":CUS.ID
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NOR,ERR)

    LOOP
        REMOVE ACC.ID FROM SEL.LIST SETTING POS3
    WHILE ACC.ID:POS3

        GOSUB FIND.TRANS

    REPEAT
RETURN
***********


***********
FIND.TRANS:
***********
*FIND TELLER TRANSACTIONS

    SELECT.CMD="SELECT ":FN.TELLER.HIS:" WITH CHEQUE.NUMBER EQ ''"
    CALL EB.READLIST(SELECT.CMD,SELECT.LIST1,'',NOR,ERR1)

    LOOP
        REMOVE TRANS.ID FROM SELECT.LIST1 SETTING POS4
    WHILE TRANS.ID:POS4
        CALL F.READ(FN.TELLER.HIS,TRANS.ID,R.TELLER.HIS,F.TELLER.HIS,TLR.ERR)
        DC.MARKR=R.TELLER.HIS<TT.TE.DR.CR.MARKER>

        IF DC.MARKR EQ 'CREDIT' THEN
            IF R.TELLER.HIS<TT.TE.ACCOUNT.2> EQ ACC.ID THEN
                AMT.TRANS=R.TELLER.HIS<TT.TE.AMOUNT.LOCAL.1>
                GOSUB COUNT.VAR
            END
        END ELSE
            IF R.TELLER.HIS<TT.TE.ACCOUNT.1> EQ ACC.ID THEN
                AMT.TRANS=R.TELLER.HIS<TT.TE.AMOUNT.LOCAL.1>
                GOSUB COUNT.VAR
            END
        END
    REPEAT

    SELECT.CMD="SELECT ":FN.TELLER:" WITH CHEQUE.NUMBER EQ ''"
    CALL EB.READLIST(SELECT.CMD,SELECT.LIST2,'',NOR,ERR1)

    LOOP
        REMOVE TRANS.ID FROM SELECT.LIST2 SETTING POS5
    WHILE TRANS.ID:POS5
        CALL F.READ(FN.TELLER,TRANS.ID,R.TELLER,F.TELLER,TLR.ERR)
        DC.MARKR=R.TELLER<TT.TE.DR.CR.MARKER>

        IF DC.MARKR EQ 'CREDIT' THEN
            IF R.TELLER<TT.TE.ACCOUNT.2> EQ ACC.ID THEN
                AMT.TRANS=R.TELLER<TT.TE.AMOUNT.LOCAL.1>
                GOSUB COUNT.VAR
            END
        END ELSE
            IF R.TELLER<TT.TE.ACCOUNT.1> EQ ACC.ID THEN
                AMT.TRANS=R.TELLER<TT.TE.AMOUNT.LOCAL.1>
                GOSUB COUNT.VAR
            END
        END
    REPEAT
RETURN
***********

**********
COUNT.VAR:
**********
*INCREMENT THE APPROPRIATE COUNTER VARIABLE

    BEGIN CASE

        CASE AMT.TRANS GT 0 AND AMT.TRANS LE 10000

            COUNT1+=1

        CASE AMT.TRANS GT 10000 AND AMT.TRANS LE 100000

            COUNT2+=1

        CASE AMT.TRANS GT 100001 AND AMT.TRANS LE 250000

            COUNT3+=1

        CASE AMT.TRANS GT 250001 AND AMT.TRANS LE 500000

            COUNT4+=1

        CASE AMT.TRANS GT 500001

            COUNT5+=1

        CASE 1

    END CASE

RETURN
**********
END
