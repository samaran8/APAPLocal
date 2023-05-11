* @ValidationCode : MjotMjE1NDk4NDkxOkNwMTI1MjoxNjgwNjAzNDE3NjgxOklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Apr 2023 15:46:57
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
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.B.AC.AV.BAL.UPDATE.SELECT
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.B.AC.AV.BAL.UPDATE.SELECT
*--------------------------------------------------------------------------------------------------------
*Description       : REDO.APAP.B.AC.AV.BAL.UPDATE.SELECT is the SELECT routine, this routine is used to
*                    update the local reference field L.AC.AV.BAL which will be difference between the
*                    amount of WORKING.BALANCE and LOCKED.AMOUNT
*Linked With       : Batch BNK/REDO.B.AC.AV.BAL.UPDATE
*In  Parameter     : NA
*Out Parameter     : NA
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date               Who                    Reference                  Description
*   ------             -----                 -------------               -------------
* 24 Nov 2010       Shiva Prasad Y       ODR-2010-11-0196 CR012         Initial Creation
* 27-Jan-2012       Gangadhar.S.V.         Performance Tuning           Select condition removed
* 05-Feb-2013       Shekar                Performance tuning            select acct.ent.lwork.day and account from ac.locked.events
* Date                  who                   Reference              
* 04-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION // TO *
* 04-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES

*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_REDO.APAP.B.AC.AV.BAL.UPDATE.COMMON
*--------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
* This is the para from where the execution of the code starts
    GOSUB PROCESS.PARA

RETURN
*--------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************
*    SEL.CMD = 'SELECT ':FN.ACCOUNT:' WITH CUSTOMER NE ""' ;* 27-Jan-2012 - S
*-    SEL.CMD = 'SELECT ':FN.ACCOUNT:' UNLIKE 3A0X' ;* 27-Jan-2012 - E
*-    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,SEL.ERR)

*Shek -s
* Select ACCOUNT ids from ACCT.ENT.LWORK.DAY
    FN.ACCT.ENT.LWORK.DAY = 'F.ACCT.ENT.LWORK.DAY'
    F.ACCT.ENT.LWORK.DAY = ''
    CALL OPF(FN.ACCT.ENT.LWORK.DAY, F.ACCT.ENT.LWORK.DAY)


    SEL.CMD = 'SELECT ' : FN.ACCT.ENT.LWORK.DAY : ' WITH @ID LIKE 10N'
    CALL EB.READLIST(SEL.CMD, SEL.LIST.1, '', '', '')

    SEL.LIST = SEL.LIST.1

* Select ACCOUNT ids from ACCT.ENT.TODAY
    FN.ACCT.ENT.TODAY = 'F.ACCT.ENT.TODAY'
    F.ACCT.ENT.TODAY = ''
    CALL OPF(FN.ACCT.ENT.TODAY, F.ACCT.ENT.TODAY)


    SEL.CMD = 'SELECT ' : FN.ACCT.ENT.TODAY : ' WITH @ID LIKE 10N'
    CALL EB.READLIST(SEL.CMD, SEL.LIST.2, '', '', '')

    IF SEL.LIST.2 THEN
        SEL.LIST<-1> = SEL.LIST.2
    END

* Select expired account number from AC.LOCK.EVENTS
    L.WORK.DATE = TODAY

    FN.REDO.LOCK.EXP.LIST = 'F.REDO.LOCK.EXP.LIST'
    F.REDO.LOCK.EXP.LIST = ''
    CALL OPF(FN.REDO.LOCK.EXP.LIST, F.REDO.LOCK.EXP.LIST)

    Err = ''
    CALL F.READ(FN.REDO.LOCK.EXP.LIST, L.WORK.DATE, R.REDO.EXP.LIST, F.REDO.LOCK.EXP.LIST, Err)

*append AC.LOCK.LIST to acct.ent.lwork ids.... can contain duplicate ids. ;*R22 AUTO CONVERSTION // TO *
    IF R.REDO.EXP.LIST THEN
        SEL.LIST<-1> = R.REDO.EXP.LIST
    END


    CALL BATCH.BUILD.LIST('',SEL.LIST)

*delete the record from list file  ;*R22 AUTO CONVERSTION // TO *
    CALL F.DELETE(FN.REDO.LOCK.EXP.LIST, L.WORK.DATE)

*Shek -e
*
RETURN
*--------------------------------------------------------------------------------------------------------
END       ;* ENd of Program
