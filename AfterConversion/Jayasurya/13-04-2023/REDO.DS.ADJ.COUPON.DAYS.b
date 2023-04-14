* @ValidationCode : MjoxNDg3NzUwMzY2OkNwMTI1MjoxNjgxMzc3NjEyOTE1OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 14:50:12
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
SUBROUTINE REDO.DS.ADJ.COUPON.DAYS(IN.OUT.PARA)
*------------------------------------------------------------------------------------------------------------
* DESCRIPTION : This deal slip routine should be attached to the DEAL.SLIP.FORMAT, REDO.BUY.SELL.DSLIP
*------------------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN Parameter    : NA
* OUT Parameter   : NA
*--------------------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : NAVEENKUMAR N
* PROGRAM NAME : REDO.DS.ADJ.COUPON.DAYS
*--------------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* Date             Author             Reference                   Description
* 10-Aug-2010      Naveenkumar N     ODR-2010-07-0082            Initial creation
* 05-May-2011      Pradeep S         PACS00056287                Mapping changed for Adj Coupon days
* 14-May-2011      Pradeep S         PACS00062654                Rounded to 4 decimal point
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*----------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SEC.TRADE

    GOSUB PROCESS
RETURN
********
PROCESS:
********
* Process to find y.cal
*
    GOSUB MULTI.LOCAL
    Y.ID = IN.OUT.PARA
    IN.OUT.PARA = ''
    Y.INTEREST.DAYS = R.NEW(SC.SBS.INTEREST.DAYS)
    Y.L.ST.ACTCOUPDAY = R.NEW(SC.SBS.LOCAL.REF)<1,L.ST.ACTCOUPDAY.POS>
*
*Dividing Y.L.ST.ACTCOUPDAY by Y.INTEREST.DAYS and passing to output variable
*
* Y.CAL = (Y.L.ST.ACTCOUPDAY/Y.INTEREST.DAYS)

    Y.CAL = (Y.L.ST.ACTCOUPDAY - Y.INTEREST.DAYS) / Y.L.ST.ACTCOUPDAY   ;*PACS00056287 - S/E

    Y.CAL = FMT(Y.CAL,"L4#10")  ;*PACS00062654 - S/E
    IF Y.CAL NE '' AND Y.CAL GT 0 THEN
        IN.OUT.PARA = Y.CAL
    END

RETURN
************
MULTI.LOCAL:
************
*Extracting the Local Field Positions
*
    APPLICATION.ID = 'SEC.TRADE'
    FIELD.NAME = 'L.ST.ACTCOUPDAY'
    FIELD.POS = ''
    CALL MULTI.GET.LOC.REF(APPLICATION.ID,FIELD.NAME,FIELD.POS)
    L.ST.ACTCOUPDAY.POS = FIELD.POS<1,1>
RETURN
END
