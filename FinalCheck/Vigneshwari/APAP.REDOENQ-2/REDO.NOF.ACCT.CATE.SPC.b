$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOF.ACCT.CATE.SPC(Y.FINAL.ARR)


* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : TAM
* Program Name  : REDO.NOF.ACCT.CATE.SPC
* ODR NUMBER    : PACS00099482 - ODR-2011-01-0492
*--------------------------------------------------------------------------------------
* Description   : This no-file enquiry routine attached with the enquiry REDO.NOF.ACCT.CATE.SPC.ENQ
* In parameter  : none
* out parameter : none
*--------------------------------------------------------------------------------------
* Modification History :
*--------------------------------------------------------------------------------------
*   DATE             WHO             REFERENCE                      DESCRIPTION
*
* 05-08-2011      MARIMUTHU S     PACS00099482                      Initial creation
*--------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.H.INVENTORY.PARAMETER
    $INSERT I_F.REDO.ADMIN.CHQ.PARAM

MAIN:

    GOSUB OPENFILES
    GOSUB PROCESS
    GOSUB PROGRAM.END
RETURN

OPENFILES:

    FN.REDO.INV.PARAM = 'F.REDO.H.INVENTORY.PARAMETER'
    F.REDO.INV.PARAM = ''

    FN.REDO.ADMIN.CHQ.PARAM = 'F.REDO.ADMIN.CHQ.PARAM'
    F.REDO.ADMIN.CHQ.PARAM = ''

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

RETURN

PROCESS:


    CALL CACHE.READ(FN.REDO.INV.PARAM,'SYSTEM',R.REDO.INV.PARAM,INV.PAR.ERR)
    Y.MAIN.TYPES = R.REDO.INV.PARAM<IN.PR.INV.MAINT.TYPE>
    Y.MAIN.TYPES = CHANGE(Y.MAIN.TYPES,@VM,@FM)


    CALL CACHE.READ(FN.REDO.ADMIN.CHQ.PARAM,'SYSTEM',R.REDO.ADMIN.CHQ.PARAM,PAR.ERRR)

    Y.CNT = DCOUNT(Y.MAIN.TYPES,@FM)
    CNT = 1 ; Y.CATEGS = ''
    LOOP
    WHILE CNT LE Y.CNT
        LOCATE 'ADMIN.CHEQUES' IN Y.MAIN.TYPES<CNT> SETTING POS.EX THEN
            Y.DUP.ITEM.CODE = R.REDO.INV.PARAM<IN.PR.ITEM.CODE,POS.EX>
            Y.ITEM.CODES = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.ITEM.CODE>
            LOCATE Y.DUP.ITEM.CODE IN Y.ITEM.CODES<1,1> SETTING POS.XX THEN
                IF Y.CATEGS EQ '' THEN
                    Y.CATEGS = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.ACCOUNT,POS.XX>
                END ELSE
                    Y.CATEGS := @FM:R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.ACCOUNT,POS.XX>
                END
            END

            CNT = POS.EX+1
*            CNT = Y.CNT + 1
        END ELSE
            CNT = Y.CNT + 1
        END
    REPEAT

    Y.FINAL.ARR = Y.CATEGS


RETURN

PROGRAM.END:
RETURN
END
