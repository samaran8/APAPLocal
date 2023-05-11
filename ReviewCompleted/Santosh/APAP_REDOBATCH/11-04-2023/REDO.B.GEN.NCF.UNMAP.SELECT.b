* @ValidationCode : MjotMTg3MTUyMzkyODpDcDEyNTI6MTY4MTE5MjgyNjM1NjpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 11:30:26
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.GEN.NCF.UNMAP.SELECT
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
*This Routine will select all the charges generated during the COB
*routine
* Input/Output:
*--------------
* IN  : -NA-
* OUT : Y.STMT.LIST
*
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who           Reference            Description
* 25-MAR-2010        Prabhu.N       ODR-2009-10-0321     Initial Creation
* Date                  who                   Reference              
* 11-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION - No Change
* 11-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*--------------------------------------------------------------------------------
    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_F.CATEG.ENTRY
    $INSERT I_F.STMT.ENTRY
    $INSERT I_REDO.B.GEN.NCF.UNMAP.COMMON
    $INSERT I_F.DATES

    SEL.CATEG.CMD="SELECT " : FN.CATEG.ENTRY : " WITH  PL.CATEGORY GT 50000  AND  VALUE.DATE  GE ":R.DATES(EB.DAT.LAST.WORKING.DAY):" AND VALUE.DATE LT ":TODAY
    CALL EB.READLIST(SEL.CATEG.CMD,SEL.CATEG.LIST,'',NO.OF.REC,AC.ERR)
    LOOP
        REMOVE Y.CATEG.ID FROM SEL.CATEG.LIST SETTING CATEG.POS
    WHILE Y.CATEG.ID:CATEG.POS
        CALL F.READ(FN.CATEG.ENTRY,Y.CATEG.ID,R.CATEG.ENTRY,F.CATEG.ENTRY,CATEG.ERROR)
        LOCATE R.CATEG.ENTRY<AC.CAT.TRANSACTION.CODE>:'*':R.CATEG.ENTRY<AC.CAT.CUSTOMER.ID> IN Y.TXN.LIST SETTING TXN.POS THEN
            Y.STMT.LIST<TXN.POS>=Y.STMT.LIST<TXN.POS>:'*':Y.CATEG.ID
        END
        ELSE
            Y.TXN.LIST<-1>      =R.CATEG.ENTRY<AC.CAT.TRANSACTION.CODE>:'*':R.CATEG.ENTRY<AC.CAT.CUSTOMER.ID>
            Y.STMT.LIST<-1>     =R.CATEG.ENTRY<AC.CAT.TRANSACTION.CODE>:'*':R.CATEG.ENTRY<AC.CAT.CUSTOMER.ID>:'+':Y.CATEG.ID
        END
    REPEAT
    CALL BATCH.BUILD.LIST('',Y.STMT.LIST)
RETURN
END
