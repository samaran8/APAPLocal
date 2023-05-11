$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOFILE.IVR.DRCRACCTS(REC.IDS)
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
** 13-APRIL-2023      Conversion Tool       R22 Auto Conversion - VM to @VM and ++ to +=
* 13-APRIL-2023      Harsha                R22 Manual Conversion - No changes                                
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT

    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN

*----------
OPEN.FILES:
*----------

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

RETURN

*-------
PROCESS:
*-------

    LOCATE "DR.ACCT" IN D.FIELDS<1> SETTING Y.DR.ACCT.POS  THEN
        Y.DR.ACCT= D.RANGE.AND.VALUE<Y.DR.ACCT.POS>
        COMI = Y.DR.ACCT
        CALL IN2POSANT(19,'')
        Y.DR.ACCT = COMI
    END

    LOCATE "CR.ACCT" IN D.FIELDS<1> SETTING Y.CR.ACCT.POS  THEN
        Y.CR.ACCT= D.RANGE.AND.VALUE<Y.CR.ACCT.POS>
        COMI = Y.CR.ACCT
        CALL IN2POSANT(19,'')
        Y.CR.ACCT = COMI
    END

    IF Y.DR.ACCT EQ '' THEN
        REC.IDS = 3
        RETURN
    END

    IF Y.CR.ACCT EQ '' THEN
        REC.IDS = 3
        RETURN
    END

    Y.ACCT.ID = Y.DR.ACCT
    GOSUB GET.CUSTOMER
    Y.DR.CUS = CUS.ID
    IF REC.IDS EQ 3 THEN
        RETURN
    END

    CUS.IDS.REL.DR = ''
    GOSUB VAL.CUS.REL
    IF REC.IDS EQ 3 THEN
        RETURN
    END

    Y.ACCT.ID = Y.CR.ACCT
    GOSUB GET.CUSTOMER
    Y.CR.CUS = CUS.ID
    IF REC.IDS EQ 3 THEN
        RETURN
    END

    GOSUB VAL.CUS.REL2
    IF REC.IDS EQ '' THEN
        REC.IDS = 2
    END

RETURN

*------------
GET.CUSTOMER:
*------------

    CUS.ID = ''; CUS.IDS.REL = ''
    CUS.IDS.TYPE = ''
    R.ACCOUNT = ''; ACC.ERR = ''
    CALL F.READ(FN.ACCOUNT,Y.ACCT.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    IF R.ACCOUNT THEN
        CUS.ID = R.ACCOUNT<AC.CUSTOMER>
        CUS.IDS.REL = R.ACCOUNT<AC.JOINT.HOLDER>
        CUS.IDS.TYPE = R.ACCOUNT<AC.RELATION.CODE>
    END ELSE
        REC.IDS = 3
    END

RETURN

*-----------
VAL.CUS.REL:
*-----------

    IF CUS.IDS.REL NE '' THEN
        Y.TOT.CUS.IDS.REL = DCOUNT(CUS.IDS.REL,@VM)
        Y.CNT.CUS.IDS.REL = 1
        LOOP
        WHILE Y.CNT.CUS.IDS.REL LE Y.TOT.CUS.IDS.REL
            Y.CUS.ID.TYPE = FIELD(CUS.IDS.TYPE,@VM,Y.CNT.CUS.IDS.REL)
            IF Y.CUS.ID.TYPE EQ '500' THEN
                REC.IDS = 3
                Y.CNT.CUS.IDS.REL = Y.TOT.CUS.IDS.REL
            END
            Y.CNT.CUS.IDS.REL += 1
        REPEAT
    END

    CUS.IDS.REL.DR = CUS.IDS.REL

RETURN

*------------
VAL.CUS.REL2:
*------------

    REC.IDS = ''
    Y.NEXT = ''

    IF Y.DR.CUS EQ Y.CR.CUS THEN
        REC.IDS = 1
        RETURN
    END

    IF CUS.IDS.REL NE '' THEN
        Y.TOT.CUS.IDS.REL = DCOUNT(CUS.IDS.REL,@VM)
        Y.CNT.CUS.IDS.REL = 1
        LOOP
        WHILE Y.CNT.CUS.IDS.REL LE Y.TOT.CUS.IDS.REL
            Y.CUS.ID.REL = FIELD(CUS.IDS.REL,@VM,Y.CNT.CUS.IDS.REL)
            IF Y.CUS.ID.REL EQ Y.DR.CUS THEN
                REC.IDS = 1
                Y.NEXT = 'Y'
                Y.CNT.CUS.IDS.REL = Y.TOT.CUS.IDS.REL
            END
            Y.CNT.CUS.IDS.REL += 1
        REPEAT
    END

    IF Y.NEXT EQ '' AND CUS.IDS.REL.DR NE '' THEN
        Y.TOT.CUS.IDS.REL = DCOUNT(CUS.IDS.REL.DR,@VM)
        Y.CNT.CUS.IDS.REL = 1
        LOOP
        WHILE Y.CNT.CUS.IDS.REL LE Y.TOT.CUS.IDS.REL
            Y.CUS.ID.REL = FIELD(CUS.IDS.REL.DR,@VM,Y.CNT.CUS.IDS.REL)
            IF Y.CUS.ID.REL EQ Y.CR.CUS THEN
                REC.IDS = 1
                Y.NEXT = 'Y'
                Y.CNT.CUS.IDS.REL = Y.TOT.CUS.IDS.REL
            END
            Y.CNT.CUS.IDS.REL += 1
        REPEAT
    END

    IF Y.NEXT EQ '' AND CUS.IDS.REL.DR NE '' AND CUS.IDS.REL NE '' THEN
        Y.TOT.CUS.IDS.REL.DR = DCOUNT(CUS.IDS.REL.DR,@VM)
        Y.TOT.CUS.IDS.REL.CR = DCOUNT(CUS.IDS.REL,@VM)
        Y.CNT.CUS.IDS.REL.DR = 1
        LOOP
        WHILE Y.CNT.CUS.IDS.REL.DR LE Y.TOT.CUS.IDS.REL.DR
            Y.CUS.ID.REL.DR = FIELD(CUS.IDS.REL.DR,@VM,Y.CNT.CUS.IDS.REL.DR)
            Y.CNT.CUS.IDS.REL.CR = 1
            LOOP
            WHILE Y.CNT.CUS.IDS.REL.CR LE Y.TOT.CUS.IDS.REL.CR
                Y.CUS.ID.REL.CR = FIELD(CUS.IDS.REL,@VM,Y.CNT.CUS.IDS.REL.CR)
                IF Y.CUS.ID.REL.DR EQ Y.CUS.ID.REL.CR THEN
                    REC.IDS = 1
                    Y.CNT.CUS.IDS.REL.CR = Y.TOT.CUS.IDS.REL.CR
                    Y.CNT.CUS.IDS.REL.DR = Y.TOT.CUS.IDS.REL.DR
                END
                Y.CNT.CUS.IDS.REL.CR += 1
            REPEAT
            Y.CNT.CUS.IDS.REL.DR += 1
        REPEAT
    END

RETURN

END
