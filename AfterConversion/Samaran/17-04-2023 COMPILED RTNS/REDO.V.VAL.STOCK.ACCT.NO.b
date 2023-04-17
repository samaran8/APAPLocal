* @ValidationCode : Mjo1MzUzOTY3NjU6Q3AxMjUyOjE2ODE3MzQxNDEyMzU6c2FtYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 17 Apr 2023 17:52:21
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.VAL.STOCK.ACCT.NO
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: S SUDHARSANAN
* PROGRAM NAME: REDO.V.VAL.STOCK.ACCT.NO
* ODR NO      : ODR-2009-12-0275
*---------------------------------------------------------------------------------------------------
*DESCRIPTION: With the value input in the field STOCK.ACCT.NO, in the versions of
* STOCK.ENTRY,REDO.FICHA.IMP.PF and STOCK.ENTRY,REDO.FICHA.IMP.PJ this
* routine has to look in the ACCOUNT application and bring the values of the fields defined
* above and store those values in the equivalent fields of STOCK.ENTRY
*IN PARAMETER:NONE
*OUT PARAMETER:NONE
*LINKED WITH: Validation routine for STOCK.ENTRY,REDO.FICHA.IMP.PF and STOCK.ENTRY,REDO.FICHA.IMP.PJ
*------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO           REFERENCE         DESCRIPTION
*15.02.2010 S SUDHARSANAN    ODR-2009-12-0275  INITIAL CREATION
*----------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*17-04-2023            Conversion Tool             R22 Auto Code conversion                      FM TO @FM VM TO @VM
*17-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.STOCK.ENTRY
    GOSUB INIT
    GOSUB PROCESS
RETURN

******
INIT:
******
    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    FN.STOCK.ENTRY='F.STOCK.ENTRY'
    F.STOCK.ENTRY=''
    CALL OPF(FN.STOCK.ENTRY,F.STOCK.ENTRY)
    L.APPL = 'ACCOUNT':@FM:'STOCK.ENTRY'
    L.FIELDS = 'L.AC.ALPH.AC.NO':@VM:'L.AC.STD.ACC.NO':@VM:'L.AC.CHEK.DIGIT':@FM:'L.AC.ALPH.AC.NO':@VM:'L.AC.STD.ACC.NO':@VM:'L.AC.CHEK.DIGIT'
    L.POS = ''
    CALL MULTI.GET.LOC.REF(L.APPL,L.FIELDS,L.POS)
    ALPH.AC.NO.POS = L.POS<1,1>
    STD.ACC.NO.POS = L.POS<1,2>
    AC.CHECK.DIG.POS = L.POS<1,3>
    POS.L.AC.ALPH.AC.NO = L.POS<2,1>
    POS.L.AC.STD.ACC.NO = L.POS<2,2>
    POS.L.AC.CHEK.DIGIT= L.POS<2,3>
RETURN
*********
PROCESS:
*********
    ACC.ID = COMI
    CALL F.READ(FN.ACCOUNT,ACC.ID,R.ACC,F.ACCOUNT,ACC.ERR)
    IF R.ACC NE '' THEN
        R.NEW(STO.ENT.LOCAL.REF)<1,POS.L.AC.ALPH.AC.NO> = R.ACC<AC.LOCAL.REF,ALPH.AC.NO.POS>
        R.NEW(STO.ENT.LOCAL.REF)<1,POS.L.AC.STD.ACC.NO> = R.ACC<AC.LOCAL.REF,STD.ACC.NO.POS>
        R.NEW(STO.ENT.LOCAL.REF)<1,POS.L.AC.CHEK.DIGIT> = R.ACC<AC.LOCAL.REF,AC.CHECK.DIG.POS>
    END
RETURN
*-----------------------------------------------------
END
