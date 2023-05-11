$PACKAGE APAP.TAM
SUBROUTINE REDO.RE.CHK.ACC.CATEG(CUSTOMER.IDENTITY)
*-------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This routine is attached to EB.CONTEXT to get the loan paid percentage.
*-------------------------------------------------------------------------
* HISTORY:
*---------
*   Date               who           Reference            Description

* 24-AUG-2011     SHANKAR RAJU     ODR-2011-07-0162     Initial Creation
** 13-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 13-04-2023 Skanda R22 Manual Conversion - No changes
*-------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.EB.RULES
    $INSERT I_F.EB.RULES.VERSION
    $INSERT I_F.CUSTOMER.ACCOUNT

    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN

OPEN.FILES:
*----------

    FN.ACCOUNT  = 'F.ACCOUNT'
    F.ACCOUNT   = ''
    R.ACCOUNT   = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER.ACCOUNT = 'F.CUSTOMER.ACCOUNT'
    F.CUSTOMER.ACCOUNT  = ''
    R.CUSTOMER.ACCOUNT  = ''
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)

    FN.EB.RULES = 'F.EB.RULES'
    F.EB.RULES  = ''
    R.EB.RULES  = ''
    CALL OPF(FN.EB.RULES,F.EB.RULES)

    FN.EB.RULES.VERSION = 'F.EB.RULES.VERSION'
    F.EB.RULES.VERSION  = ''
    R.EB.RULES.VERSION  = ''
    CALL OPF(FN.EB.RULES.VERSION,F.EB.RULES.VERSION)

RETURN

PROCESS:
*-------
    Y.RULE = 'RULE.ACCOUNT.CAMP'

    CALL CACHE.READ(FN.EB.RULES, Y.RULE, R.EB.RULES, ERR.EB.RULES) ;* R22 Auto conversion

    Y.VERSION = R.EB.RULES<EB.RLE.VERSIONS,1>
    Y.RULE.VERSION = Y.RULE:"*":Y.VERSION

    CALL CACHE.READ(FN.EB.RULES.VERSION, Y.RULE.VERSION, R.EB.RULES.VERSION, ERR.RV) ;* R22 Auto conversion
    Y.RULE.TEXT = R.EB.RULES.VERSION<EB.RLE.VER.TEXT>

    FINDSTR "CATEGORY.1" IN Y.RULE.TEXT SETTING POS.VAR,POS.CAT THEN

        Y.CATEG.1 = FIELD(R.EB.RULES.VERSION<EB.RLE.VER.TEXT,POS.CAT>,"=",2)
        Y.CATEG.2 = FIELD(R.EB.RULES.VERSION<EB.RLE.VER.TEXT,POS.CAT+1>,"=",2)
        Y.CATEG.3 = FIELD(R.EB.RULES.VERSION<EB.RLE.VER.TEXT,POS.CAT+2>,"=",2)
        Y.CATEG.4 = FIELD(R.EB.RULES.VERSION<EB.RLE.VER.TEXT,POS.CAT+3>,"=",2)

    END

    CALL F.READ(FN.CUSTOMER.ACCOUNT,CUSTOMER.IDENTITY,R.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT,ERR.CUS.ACC)

    IF R.CUSTOMER.ACCOUNT THEN
        TOT.ACC = DCOUNT(R.CUSTOMER.ACCOUNT,@FM)

        LOOP.VAR = 1
        LOOP

        WHILE LOOP.VAR LE TOT.ACC
            Y.ACCT.NO = R.CUSTOMER.ACCOUNT<LOOP.VAR>
            CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ERR)
            IF R.ACCOUNT THEN
                AC.CATEG = R.ACCOUNT<AC.CATEGORY>

                IF ((AC.CATEG LT Y.CATEG.1) OR (AC.CATEG GT Y.CATEG.2)) AND ((AC.CATEG LT Y.CATEG.3) OR (AC.CATEG GT Y.CATEG.4)) THEN
                    CUSTOMER.IDENTITY = 1
                END ELSE
                    CUSTOMER.IDENTITY = 0
                    RETURN
                END

            END ELSE
                CUSTOMER.IDENTITY = 1
            END
            LOOP.VAR += 1 ;* R22 Auto conversion
        REPEAT
    END ELSE
        CUSTOMER.IDENTITY = 0
        RETURN
    END

RETURN

END
