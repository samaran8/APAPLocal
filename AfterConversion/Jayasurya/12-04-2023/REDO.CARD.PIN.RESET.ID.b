* @ValidationCode : MjoxNDIwNjM5ODI1OkNwMTI1MjoxNjgxMjE5MTg0ODk2OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Apr 2023 18:49:44
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
SUBROUTINE REDO.CARD.PIN.RESET.ID

****************************************************************
*DESCRIPTION:
*------------
*This routine is used to check whether the id is valid or not
*------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who           Reference            Description
* 12-05-2011           Prabhu.N      PACS00054646         Initial Creation
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*11-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM SM TO @SM
*11-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*----------------------------------------------------------------------------------------------
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.CARD.PIN.RESET
    $INSERT I_F.REDO.CARD.BIN
    $INSERT I_F.LATAM.CARD.ORDER
    GOSUB INIT
    GOSUB PROCESS
RETURN
*----
INIT:
*-----
    FN.REDO.CARD.BIN='F.REDO.CARD.BIN'
    F.REDO.CARD.BIN=''
    CALL OPF(FN.REDO.CARD.BIN,F.REDO.CARD.BIN)

    FN.LATAM.CARD.ORDER ='F.LATAM.CARD.ORDER'
    F.LATAM.CARD.ORDER =''
    CALL OPF(FN.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER)
RETURN
*-------
PROCESS:
*-------

    VAR.ID = ID.NEW[1,6]
    CALL F.READ(FN.REDO.CARD.BIN,VAR.ID,R.REDO.CARD.BIN,F.REDO.CARD.BIN,ERR)
    Y.CARD.TYPE=R.REDO.CARD.BIN<REDO.CARD.BIN.CARD.TYPE>
    CHANGE @VM TO @FM IN Y.CARD.TYPE
    Y.TOT.CARD.TYPE=DCOUNT(Y.CARD.TYPE,@FM)
    Y.CARD.CNT=1
    LOOP
    WHILE Y.CARD.CNT LE Y.TOT.CARD.TYPE
        Y.LATAM.CARD.ID=Y.CARD.TYPE<Y.CARD.CNT>:'.':ID.NEW
        Y.CARD.ERR=''
        CALL F.READ(FN.LATAM.CARD.ORDER,Y.LATAM.CARD.ID,R.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER,Y.CARD.ERR)
        IF NOT(Y.CARD.ERR) THEN
            RETURN
        END
        Y.CARD.CNT += 1
    REPEAT
    E='EB-REDO.NO.CARD'
    CALL STORE.END.ERROR
RETURN
END
