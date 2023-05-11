* @ValidationCode : MjoxOTcyMzgxNjY4OkNwMTI1MjoxNjgxMTExMzIwNzYwOklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Apr 2023 12:52:00
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
$PACKAGE APAP.TAM
SUBROUTINE REDO.GET.WAIVE.CATEG(WAIVE.CAT)
***********************************************************
*----------------------------------------------------------
* COMPANY NAME    : APAP
* DEVELOPED BY    : JOAQUIN COSTA
* PROGRAM NAME    : REDO.GET.WAIVE.CATEG
*----------------------------------------------------------
* DESCRIPTION     : This routine is used to get the CATEGORY
*                   used to register waived tax value
*------------------------------------------------------------

* LINKED WITH    : TELLER AUTH ROUTINE
* IN PARAMETER   : NONE
* OUT PARAMETER  : NONE

*----------------------------------------------------------------------
* Modification History :
*-----------------------
*  DATE             WHO          REFERENCE       DESCRIPTION
*  18.05.2012       JCOSTA       PACS00163682    INITIAL CREATION
*
** 10-04-2023 R22 Auto Conversion no changes
** 10-04-2023 Skanda R22 Manual Conversion - line no 126
*----------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
*
    $INSERT I_F.TELLER
*
    $INSERT I_F.REDO.MANAGER.CHQ.PARAM
    $INSERT I_F.REDO.ADMIN.CHQ.PARAM
*
    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB PRE.PROCESS
*
    IF PROCESS.GOHEAD THEN
        GOSUB PROCESS
    END
*
RETURN
*
*********
PROCESS:
*********
*
    Y.FLAG.NOS = ""
    Y.FLAG.INT = ""
*
    GOSUB READ.ALL.ACCT.NOSTRO
*
    IF Y.FLAG.NOS EQ "" THEN
        GOSUB READ.ALL.ACCT.INTERNAL
    END
*
    IF Y.FLAG.INT EQ "" AND Y.FLAG.NOS EQ "" THEN
        WAIVE.CAT = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.CATPL.DEF>
    END
*
RETURN
*
*********************
READ.ALL.ACCT.NOSTRO:
*********************
*
    Y.PARAM.ID = 'SYSTEM'
    CALL CACHE.READ(FN.REDO.MANAGER.CHQ.PARAM,Y.PARAM.ID,R.REDO.MANAGER.CHQ.PARAM,PARAM.ERR)
    Y.ACCOUNT.ALL = R.REDO.MANAGER.CHQ.PARAM<MAN.CHQ.PRM.ACCOUNT>
    Y.CATEGORY    = R.REDO.MANAGER.CHQ.PARAM<MAN.CHQ.PRM.CATEGORY>
*
    LOCATE Y.ACCT.NOS IN Y.ACCOUNT.ALL<1,1> SETTING ACCT.POS THEN
        WAIVE.CAT = Y.CATEGORY<1,ACCT.POS>
        Y.FLAG.NOS         = 1
    END
*
RETURN
*
***********************
READ.ALL.ACCT.INTERNAL:
***********************
*
    Y.ADMIN.ID = 'SYSTEM'
*
    CALL CACHE.READ(FN.REDO.ADMIN.CHQ.PARAM,Y.ADMIN.ID,R.REDO.ADMIN.CHQ.PARAM,Y.ADMIN.ERR)
    Y.ACCOUNT.ALL = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.ACCOUNT>
    Y.CATEGORY    = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.CATEGORY>
*
    LOCATE R.NEW(TT.TE.ACCOUNT.1) IN Y.ACCOUNT.ALL<1,1> SETTING ACCT.POS THEN
        WAIVE.CAT  = Y.CATEGORY<1,ACCT.POS>
        Y.FLAG.INT = 1
    END
*
    ACCT.POS = ""
    LOCATE R.NEW(TT.TE.ACCOUNT.2) IN Y.ACCOUNT.ALL<1,1> SETTING ACCT.POS THEN
        WAIVE.CAT  = Y.CATEGORY<1,ACCT.POS>
        Y.FLAG.INT = 1
    END
*
RETURN
*
******
INIT:
******
*
    Y.ACCT.FLAG    = ""
    PROCESS.GOHEAD = 1
*
    LREF.APP     = "TELLER"
    LREF.FIELDS  = "L.FT.NOSTRO.ACC"
    LREF.POS     = ''
*
    CALL APAP.TAM.MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS) ;* R22 Manual conversion
    WPOSCN            = LREF.POS<1,1>
*
    Y.ACCT.NOS      = R.NEW(TT.TE.LOCAL.REF)<1,WPOSCN>
*
    WAIVE.CAT  = ''
*
RETURN
*
************
OPEN.FILES:
************
*
    FN.REDO.MANAGER.CHQ.PARAM = 'F.REDO.MANAGER.CHQ.PARAM'
    F.REDO.MANAGER.CHQ.PARAM  = ''
*
    FN.REDO.ADMIN.CHQ.PARAM = 'F.REDO.ADMIN.CHQ.PARAM'
    F.REDO.ADMIN.CHQ.PARAM  = ''
*
RETURN
*
*************
PRE.PROCESS:
*************
*
    LOOP.CNT  = 1
    MAX.LOOPS = 1
*
    LOOP WHILE LOOP.CNT LE MAX.LOOPS AND PROCESS.GOHEAD
        BEGIN CASE
            CASE LOOP.CNT EQ 1
*
        END CASE
*
        LOOP.CNT += 1
    REPEAT

RETURN
*
END
