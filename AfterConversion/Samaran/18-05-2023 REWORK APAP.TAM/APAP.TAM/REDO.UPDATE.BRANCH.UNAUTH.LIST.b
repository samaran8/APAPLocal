* @ValidationCode : MjotMzAyMzIzODI3OkNwMTI1MjoxNjgyNTE4ODc3MjA3OklUU1M6LTE6LTE6Nzc1OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 26 Apr 2023 19:51:17
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 775
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
*---------------------------------------------------------------------------------------
*MODIFICATION HISTORY:
*DATE           WHO                 REFERENCE               DESCRIPTION
*24-APR-2023    CONVERSION TOOL     R22 AUTO CONVERSION     FM TO @FM,VM TO @VM, F.READ TO CACHE.READ
*24-APR-2023    VICTORIA S          R22 MANUAL CONVERSION   NO CHANGE
*----------------------------------------------------------------------------------------
SUBROUTINE REDO.UPDATE.BRANCH.UNAUTH.LIST
*-----------------------------------------------------------------------------
* Modification History
* Date          who                Reference            Description
*
* 13-06-2013    RIYAS              PACS00148239       Initial Creation
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.USER
    $INSERT I_F.REDO.EXCEP.REC.PARAM
    $INSERT I_F.VERSION
    $INSERT I_F.FUNDS.TRANSFER



    GOSUB OPEN.FILES
    GOSUB INITIALISE

    LOCATE APPLICATION IN Y.APPLICATION.LIST SETTING APP.POS THEN
        IF Y.BCH.CODE AND Y.DEP.CODE THEN

            GOSUB PROCESS
        END
    END

RETURN

*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

    Y.REC.STATUS = R.NEW(FT.RECORD.STATUS)
    Y.NO.OF.AUTH = R.VERSION(EB.VER.NO.OF.AUTH)
    Y.RET = ''


    IF Y.NO.OF.AUTH EQ 0 AND Y.REC.STATUS THEN
        CALL F.READ(FN.BRANCH.UNAUTH.LIST,Y.ID,R.BRANCH.UNAUTH.LIST,F.BRANCH.UNAUTH.LIST,LIST.ERR)
        CALL F.DELETE(FN.BRANCH.UNAUTH.LIST,Y.ID)
        RETURN
    END

    IF V$FUNCTION EQ 'A' THEN
        CALL F.READ(FN.BRANCH.UNAUTH.LIST,Y.ID,R.BRANCH.UNAUTH.LIST,F.BRANCH.UNAUTH.LIST,LIST.ERR)
        CALL F.DELETE(FN.BRANCH.UNAUTH.LIST,Y.ID)
        RETURN
    END

    IF V$FUNCTION EQ 'I' THEN
        Y.VERSION.NAME = APPLICATION:PGM.VERSION
        R.BRANCH.UNAUTH.LIST = Y.VERSION.NAME:'-':ID.NEW
        CALL F.WRITE(FN.BRANCH.UNAUTH.LIST,Y.ID,R.BRANCH.UNAUTH.LIST)
    END

RETURN
*-----------------------------------------------------------------------------
INITIALISE:
*-----------------------------------------------------------------------------

    LREF.APP = 'USER'
    LREF.FIELDS = 'L.US.IDC.CODE':@VM:'L.US.IDC.BR' ;*R22 AUTO CONVERSION
    LREF.POS=''
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    LOC.DEP.CODE.POS  = LREF.POS<1,1>
    LOC.BCH.CODE.POS  = LREF.POS<1,2>
    CALL CACHE.READ(FN.USER, OPERATOR, REC.USER, USER.ERR) ;*R22 AUTO CONVERSION
    Y.DEP.CODE = REC.USER<EB.USE.LOCAL.REF,LOC.DEP.CODE.POS>
    Y.BCH.CODE = REC.USER<EB.USE.LOCAL.REF,LOC.BCH.CODE.POS>
    Y.ID  = Y.BCH.CODE:'-':Y.DEP.CODE:'-':ID.NEW:'-':APPLICATION

    CALL CACHE.READ(FN.REDO.EXCEP.REC.PARAM,'SYSTEM',R.REDO.EXCEP.REC.PARAM,EXCEP.ERR)
    Y.APPLICATION.LIST  = R.REDO.EXCEP.REC.PARAM<EXCEP.APPLICATION.NAME>
    CHANGE @VM TO @FM IN Y.APPLICATION.LIST ;*R22 AUTO CONVERSION

RETURN
*-----------------------------------------------------------------------------
OPEN.FILES:
*-----------------------------------------------------------------------------
    FN.BRANCH.UNAUTH.LIST = 'F.BRANCH.UNAUTH.LIST'
    F.BRANCH.UNAUTH.LIST  = ''
    CALL OPF(FN.BRANCH.UNAUTH.LIST,F.BRANCH.UNAUTH.LIST)
    R.BRANCH.UNAUTH.LIST = ''

    FN.REDO.EXCEP.REC.PARAM = 'F.REDO.EXCEP.REC.PARAM'
    F.REDO.EXCEP.REC.PARAM  = ''
    CALL OPF(FN.REDO.EXCEP.REC.PARAM,F.REDO.EXCEP.REC.PARAM)
    R.REDO.EXCEP.REC.PARAM = ''

    FN.USER = 'F.USER'
    F.USER  =  ''
    CALL OPF(FN.USER,F.USER)

RETURN
*-----------------------------------------------------------------------------

END
