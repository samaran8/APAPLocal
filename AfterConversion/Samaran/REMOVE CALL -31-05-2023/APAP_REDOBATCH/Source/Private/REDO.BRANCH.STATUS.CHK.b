* @ValidationCode : MjotOTY4MzkzODY4OkNwMTI1MjoxNjg0ODU0NDA0NjYyOklUU1M6LTE6LTE6NTY6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 23 May 2023 20:36:44
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 56
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.BRANCH.STATUS.CHK
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This routine for the ODR-2009-12-0307 and HD1101270
*
* Input/Output:
*--------------
* IN : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS : @ID
* CALLED BY :
*
* Revision History:
*------------------------------------------------------------------------------------------
*   Date               who           Reference            Description
* 01-Feb-2011        Ganesh R        HD1101270           Initial Creation
* Date                  who                   Reference              
* 17-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION - No Change
* 17-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*------------------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
*
    $INSERT I_F.COMPANY
*
    $INSERT I_F.REDO.BRANCH.STATUS
*
    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN

*---*
INIT:
*---*
*--------------------------*
*Initialising the Variables
*--------------------------*
    Y.TSA.ID=ID.NEW
*
RETURN
*
*---------*
OPEN.FILES:
*---------*
*------------------*
* Calling the Files*
*------------------*

    FN.REDO.BRANCH.STATUS = 'F.REDO.BRANCH.STATUS'
    F.REDO.BRANCH.STATUS  = ''
    CALL OPF(FN.REDO.BRANCH.STATUS,F.REDO.BRANCH.STATUS)

RETURN
*--------*
PROCESS:
*--------*
*-----------------------------------------------------------*
*Check for COB TSA service
*-----------------------------------------------------------*
    ID.VAL = ID.NEW
    ID.VALUE = FIELD(ID.VAL,'/',2)
    IF ID.VALUE EQ 'COB' OR ID.NEW EQ 'COB' THEN
        GOSUB RAISE.OVERRIDE
    END
RETURN

***************
RAISE.OVERRIDE:
***************
*-----------------------------------------------------------*
*Raising the Overrides if the OPERATION.STATUS field is Open
*-----------------------------------------------------------*
    SEL.CMD = "SELECT " : FN.REDO.BRANCH.STATUS : " WITH OPERATION.STATUS EQ 'OPEN'"
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.ERR)
    IF NO.OF.REC GE 1 THEN
        ETEXT = "EB-REDO.BRANCH.OPENED"
        CALL STORE.END.ERROR
    END
*
RETURN
*
END
