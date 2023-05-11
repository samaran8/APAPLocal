* @ValidationCode : MjoxNjkwNjk1OTI5OkNwMTI1MjoxNjgwNjA2ODUyNDk1OklUU1MxOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 04 Apr 2023 16:44:12
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS1
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.AA
SUBROUTINE REDO.POST.CHQ.REV.CHARGE
    
*-----------------------------------------------------------------------------------
* Modification History:
*DATE              WHO                REFERENCE                        DESCRIPTION
*29-03-2023     CONVERSION TOOL       AUTO R22 CODE CONVERSION           VM TO @VM
*29-03-2023      MOHANRAJ R        MANUAL R22 CODE CONVERSION         Package name added APAP.AA
*-----------------------------------------------------------------------------------

    
*------------------------------------------------------
* Description: This routine is to update the REDO.LOAN.CHQ.RETURN in case of reversal
* and it is attached as post routine for reversal of the activity CHQ.RETURN.CHG.
*------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.APP.COMMON
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.REDO.LOAN.CHQ.RETURN

    IF c_aalocActivityStatus EQ 'REVERSE' THEN
        GOSUB PROCESS
    END

RETURN
*------------------------------------------------------
PROCESS:
*------------------------------------------------------
    GOSUB GET.LOC.REF.POS
    GOSUB GET.AMOUNT
    IF Y.CHARGE.AMT THEN
        GOSUB UPDATE.REV.TEMPLATE
    END

RETURN
*------------------------------------------------------
UPDATE.REV.TEMPLATE:
*------------------------------------------------------
    Y.LOAN.NO = c_aalocLinkedAccount      ;* Loan Account No.

    CALL F.READ(FN.REDO.LOAN.CHQ.RETURN,Y.LOAN.NO,R.REDO.LOAN.CHQ.RETURN,F.REDO.LOAN.CHQ.RETURN,RET.ERR)
    IF R.REDO.LOAN.CHQ.RETURN ELSE
        RETURN
    END
    R.REDO.LOAN.CHQ.RETURN<LN.CQ.RET.REV.RET.AMT> = R.REDO.LOAN.CHQ.RETURN<LN.CQ.RET.REV.RET.AMT> + Y.CHARGE.AMT
    CALL F.WRITE(FN.REDO.LOAN.CHQ.RETURN,Y.LOAN.NO,R.REDO.LOAN.CHQ.RETURN)

RETURN
*------------------------------------------------------
GET.AMOUNT:
*------------------------------------------------------
    Y.CHARGE.AMT = ''
    FIELD.2 = 'LOCAL.REF:':POS.L.BILL.AMOUNT:':1'
*    LOCATE AA$PROPERTY.ID IN AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.PROPERTY,1> SETTING POS.PROPER THEN
    LOCATE c_aalocPropertyId IN AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.PROPERTY,1> SETTING POS.PROPER THEN ;*MANUAL R22 CODE CONVERSION
        LOCATE FIELD.2 IN AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.FIELD.NAME,POS.PROPER,1> SETTING FLD.POS1 THEN
            Y.CHARGE.AMT =  AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.FIELD.VALUE,POS.PROPER,FLD.POS1>
        END
    END

RETURN
*------------------------------------------------------
GET.LOC.REF.POS:
*------------------------------------------------------

    LOC.REF.APPLICATION="AA.PRD.DES.BALANCE.MAINTENANCE"
    LOC.REF.FIELDS='L.BILL.REF':@VM:'L.BILL.AMOUNT' ;*AUTO R22 CODE CONVERSION
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.BILL.REF    = LOC.REF.POS<1,1>
    POS.L.BILL.AMOUNT = LOC.REF.POS<1,2>


    FN.REDO.LOAN.CHQ.RETURN = 'F.REDO.LOAN.CHQ.RETURN'
    F.REDO.LOAN.CHQ.RETURN  = ''
    CALL OPF(FN.REDO.LOAN.CHQ.RETURN,F.REDO.LOAN.CHQ.RETURN)

RETURN
END
