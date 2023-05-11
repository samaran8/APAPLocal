$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOFILE.IVR.CUENTACLIENTE(REC.IDS)
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 13-APRIL-2023      Conversion Tool       R22 Auto Conversion - VM to @VM and ++ to +=
* 13-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.RELATION

    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN

*----------
OPEN.FILES:
*----------

    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.RELATION='F.RELATION'
    F.RELATION=''
    CALL OPF(FN.RELATION,F.RELATION)

RETURN

*-------
PROCESS:
*-------

    LOCATE "@ID" IN D.FIELDS<1> SETTING Y.AGENCY.POS  THEN
        Y.ACCT.ID= D.RANGE.AND.VALUE<Y.AGENCY.POS>
        COMI = Y.ACCT.ID
        CALL IN2POSANT(19,'')
        Y.ACCT.ID = COMI
    END

    R.ACCOUNT=''
    CALL F.READ(FN.ACCOUNT,Y.ACCT.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    IF R.ACCOUNT THEN
        CUS.ID=R.ACCOUNT<AC.CUSTOMER>
        GOSUB GET.CUS.REL

        REC.IDS=1 : '*' : CUS.ID : Y.CUS.REL.ALL

    END ELSE

        REC.IDS=2

    END

RETURN

*-----------
GET.CUS.REL:
*-----------

    Y.CUS.REL.ALL = ''

    CUS.JH = R.ACCOUNT<AC.JOINT.HOLDER>
    CUS.REL.CODE = R.ACCOUNT<AC.RELATION.CODE>

    IF CUS.JH THEN
        Y.TOT.CUS.JH = DCOUNT(CUS.JH,@VM)
        Y.CNT.CUS.JH = 1
        LOOP
        WHILE Y.CNT.CUS.JH LE Y.TOT.CUS.JH
            Y.CUS.R = FIELD(CUS.JH,@VM,Y.CNT.CUS.JH)
            Y.CUS.C = FIELD(CUS.REL.CODE,@VM,Y.CNT.CUS.JH)
            GOSUB GET.TYPE
            IF Y.TYPE THEN
                Y.CUS.REL.ALL := ' / ':Y.CUS.R:'(':Y.TYPE:')'
            END ELSE
                Y.CUS.REL.ALL := ' / ':Y.CUS.R
            END
            Y.CNT.CUS.JH += 1
        REPEAT
    END

RETURN

*--------
GET.TYPE:
*--------

    Y.TYPE = ''
    R.RELATION = '' REL.ERR = ''
    CALL F.READ(FN.RELATION,Y.CUS.C,R.RELATION,F.RELATION,REL.ERR)
    IF R.RELATION THEN
        Y.TYPE = R.RELATION<EB.REL.DESCRIPTION>
        CHANGE '-' TO '' IN Y.TYPE
        CHANGE ' ' TO '' IN Y.TYPE
    END

RETURN

END
