SUBROUTINE REDO.S.FC.AA.BILL.DUE.AMT(AA.ID, AA.ARR)

*
* Subroutine Type : ROUTINE
* Attached to     : ROUTINE REDO.E.NOF.DATCUST
* Attached as     : ROUTINE
* Primary Purpose : To return value of AA.ARR.INTEREST>L.AA.REV.RT.TY  field
*
* Incoming:
* ---------
*
*
* Outgoing:
* ---------
* AA.ARR - data returned to the routine
*
* Error Variables:
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Juan Pablo Armas - TAM Latin America
* Date            :
*
*-----------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.BILL.DETAILS

    GOSUB INITIALISE
    GOSUB OPEN.FILES

    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END

RETURN  ;* Program RETURN
*-----------------------------------------------------------------------------------
PROCESS:
*======

    CALL F.READ(FN.AA.ACCOUNT.DETAILS,Y.ARRG.ID,R.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS,Y.ACT.DET.ERR)
    IF R.AA.ACCOUNT.DETAILS NE '' THEN
        Y.CONT = DCOUNT(R.AA.ACCOUNT.DETAILS<AA.AD.BILL.ID>,@VM)
        I.VAR = 1
        LOOP
        WHILE I.VAR LE Y.CONT
            BILL.REFERENCE = R.AA.ACCOUNT.DETAILS<AA.AD.BILL.ID,I.VAR>
            IF R.AA.ACCOUNT.DETAILS<AA.AD.BILL.TYPE,I.VAR,1> EQ 'PAYMENT' AND R.AA.ACCOUNT.DETAILS<AA.AD.SET.STATUS,I.VAR,1> EQ 'UNPAID' THEN
                CALL AA.GET.BILL.DETAILS(Y.ARRG.ID, BILL.REFERENCE, BILL.DETAILS, RET.ERROR)
                LOCATE 'PRMORA' IN BILL.DETAILS<AA.BD.PROPERTY,1> SETTING Y.POS THEN
                    AA.ARR +=  SUM(BILL.DETAILS<AA.BD.OS.PROP.AMOUNT,Y.POS>)    ;* 35
                END
            END
            I.VAR += 1
        REPEAT
    END

RETURN
*------------------------
INITIALISE:
*=========
    PROCESS.GOAHEAD = 1
    B.CONT = 0
    Y.ARRG.ID = AA.ID
    FN.AA.ACCOUNT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS  = ''
    R.AA.ACCOUNT.DETAILS = ''
    AA.ARR = 0
    BILL.REFERENCE = ''
    BILL.DETAILS = ''
    RET.ERROR = ''
RETURN

*------------------------
OPEN.FILES:
*=========
    CALL OPF(FN.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS)

RETURN
*------------
END
