* @ValidationCode : MjoxMTEzNjg2NzM0OkNwMTI1MjoxNjgxMzc4MjA2Nzk0OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 15:00:06
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
SUBROUTINE REDO.DS.CALC.AC.CL.TOT.CHG(RES)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :GANESH.R
*Program   Name    :REDO.S.FETCH.CUST.IDEN
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*---------------------------------------------------------------------------------

*DESCRIPTION       :This program is used to check the customer record Field and get
*                   identification field and display in the deal slip
*[IDENTITY ID  >> "EEEE" = LEGAL.ID or L.CU.CIDENT or L.CU.NOUNICO or L.CU.ACTANAC
*    Just one of these values will be populated on CUSTOMER (so pick up the one being populated from above))
*LINKED WITH       :
* ----------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT.CLOSURE

    GOSUB INIT
RETURN
*-----*
INIT:
*-----*
    RES=R.NEW(AC.ACL.TOTAL.CHARGES) + R.NEW(AC.ACL.CLO.CHARGE.AMT)
    Y.TEMP.AMOUNT    =RES
    Y.LEN.CUR        ='2'
    Y.LEN.CUR        ="L%":Y.LEN.CUR
    Y.TEMP.AMOUNT.FIR=FIELD(Y.TEMP.AMOUNT,'.',1)
    Y.TEMP.AMOUNT.DEC=FIELD(Y.TEMP.AMOUNT,'.',2)
    Y.TEMP.AMOUNT.DEC=FMT(Y.TEMP.AMOUNT.DEC,Y.LEN.CUR)
    RES              =Y.TEMP.AMOUNT.FIR:'.':Y.TEMP.AMOUNT.DEC
RETURN
END
