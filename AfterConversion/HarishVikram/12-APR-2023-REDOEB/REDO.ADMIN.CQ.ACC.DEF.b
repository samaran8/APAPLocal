* @ValidationCode : MjoxODYyNjMyNzcyOkNwMTI1MjoxNjgxMjkzNjQxMTc5OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 15:30:41
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOEB
SUBROUTINE REDO.ADMIN.CQ.ACC.DEF
*----------------------------------------------------------------------
*DESCRIPTION: This routine is auto new content routine to the CREDIT.ACCT.NO

*IN PARAMETER: NA
*OUT PARAMETER: NA
*LINKED WITH: FT
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*07-09-2011  MARIMUTHU S    PACS00121111
*10-06-2012  MARIMUTHU S    PACS00146445
*
* 12-APR-2023     Conversion tool   R22 Auto conversion   FM TO @FM
* 12-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*----------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.ADMIN.CHQ.PARAM
    $INSERT I_F.ACCOUNT
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.REDO.BRANCH.INT.ACCT.PARAM


MAIN:


    GOSUB PROCESS
    GOSUB PGM.END

PROCESS:

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.BRANCH.INT.ACCT.PARAM = 'F.REDO.BRANCH.INT.ACCT.PARAM'
    F.REDO.BRANCH.INT.ACCT.PARAM = ''

    POSS = ''
    APPL = 'FUNDS.TRANSFER'
    APL.FIELD = 'L.FT.LEGAL.ID'
    CALL MULTI.GET.LOC.REF(APPL,APL.FIELD,POSS)
    Y.CQ.POS = POSS<1,1>


    R.NEW(FT.CREDIT.ACCT.NO) = ''

    Y.CQ.TYPE = R.NEW(FT.LOCAL.REF)<1,Y.CQ.POS>

    CALL CACHE.READ(FN.REDO.BRANCH.INT.ACCT.PARAM,'SYSTEM',R.REDO.BRANCH.INT.ACCT.PARAM,CHQ.PAR.ERR)

    Y.CHQ.TYPE = R.REDO.BRANCH.INT.ACCT.PARAM<BR.INT.ACCT.ADMIN.CHQ.TYPE>
    Y.CHQ.ACC = R.REDO.BRANCH.INT.ACCT.PARAM<BR.INT.ACCT.ADMIN.CHQ.ACCOUNT>

    VIRTUAL.TAB.ID = 'ADMIN.CHQ.TYPE'
    CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID)
    Y.LOOKUP.LIST = VIRTUAL.TAB.ID<2>
    Y.LOOKUP.LIST = CHANGE(Y.LOOKUP.LIST,'_',@FM )

    Y.DESC.LIST = VIRTUAL.TAB.ID<11>
    Y.DESC.LIST = CHANGE(Y.DESC.LIST,'_',@FM)

    FLG = ''
    Y.CNT = DCOUNT(Y.DESC.LIST,@FM)
    LOOP
    WHILE Y.CNT GT 0 DO
        FLG += 1
        Y.DES = Y.DESC.LIST<FLG>
        GOSUB CHECK.LOC
        Y.CNT -= 1
    REPEAT

RETURN

CHECK.LOC:

    LOCATE Y.CQ.TYPE IN Y.DES<1,1> SETTING POS THEN
        Y.ID.VAL = Y.LOOKUP.LIST<FLG>

        LOCATE Y.ID.VAL IN Y.CHQ.TYPE<1,1> SETTING POS.TIT THEN
            R.NEW(FT.CREDIT.ACCT.NO) = Y.CHQ.ACC<1,POS.TIT>
        END
        RETURN
    END

RETURN

PGM.END:

END
