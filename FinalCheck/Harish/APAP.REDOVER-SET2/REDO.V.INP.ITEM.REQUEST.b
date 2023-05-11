* @ValidationCode : MjoxMjE0MjQ5NjEwOkNwMTI1MjoxNjgxMjgzMzM1MDAzOnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 12 Apr 2023 12:38:55
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
SUBROUTINE REDO.V.INP.ITEM.REQUEST
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COMPANY
    $INSERT I_F.USER
    $INSERT I_F.REDO.H.ORDER.DETAILS
    $INSERT I_F.REDO.H.REORDER.LEVEL
    $INSERT I_F.REDO.H.MAIN.COMPANY
    $INSERT I_F.REDO.H.INVENTORY.PARAMETER
*----------------------------------------------------------------------------
* Description:
* This routine will be attached to the version REDO.ORDER.DETAIL,ITEM.REQUEST as
* a INPUT routine
* COMPANY NAME : APAP
* DEVELOPED BY : JEEVA T
* PROGRAM NAME : REDO.V.INP.ITEM.REQUEST
*-------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO            REFERENCE                  DESCRIPTION
* 12.04.2010     JEEVA T        FIX FOR ISSUE HD1053868     INTIAL CREATION
* ------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*12-04-2023            Conversion Tool             R22 Auto Code conversion                FM TO @FM,VM TO @VM,SM TO @SM, Y.CNT + 1 TO +=1
*12-04-2023              Samaran T                R22 Manual Code conversion                         No Changes


*----------------------------------------------------------
MAIN:
*-----------------------------------------------------------------------------------------
    GOSUB INIT
    GOSUB PROCESS
RETURN

*-----------------------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------------------
    FN.REDO.H.INVENTORY.PARAMETER = 'F.REDO.H.INVENTORY.PARAMETER'
    F.REDO.H.INVENTORY.PARAMETER = ''
    CALL OPF(FN.REDO.H.INVENTORY.PARAMETER,F.REDO.H.INVENTORY.PARAMETER)

    FN.REDO.H.MAIN.COMPANY = 'F.REDO.H.MAIN.COMPANY'
    F.REDO.H.MAIN.COMPANY = ''
    CALL OPF(FN.REDO.H.MAIN.COMPANY,F.REDO.H.MAIN.COMPANY)

    FN.REDO.H.REORDER.LEVEL = 'F.REDO.H.REORDER.LEVEL'
    F.REDO.H.REORDER.LEVEL =''
    CALL OPF(FN.REDO.H.REORDER.LEVEL,F.REDO.H.REORDER.LEVEL)

    LOC.REF.APPLICATION="USER"
    LOC.REF.FIELDS='L.US.IDC.BR':@VM:'L.US.IDC.CODE'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.BR.VAL =LOC.REF.POS<1,1>
    POS.DETP.VAL = LOC.REF.POS<1,2>

    Y.CODE.DEPT = ''
    Y.BRANCH.LIST = R.USER<EB.USE.LOCAL.REF,POS.BR.VAL>
    Y.DEPT.LIST = R.USER<EB.USE.LOCAL.REF,POS.DETP.VAL>
    CHANGE @SM TO @FM IN Y.BRANCH.LIST
    CHANGE @VM TO @FM IN Y.BRANCH.LIST
    CHANGE @SM TO @FM IN Y.DEPT.LIST
    CHANGE @VM TO @FM IN Y.DEPT.LIST

    CALL F.READ(FN.REDO.H.REORDER.LEVEL,ID.COMPANY,R.REDO.H.REORDER.LEVEL,F.REDO.H.REORDER.LEVEL,Y.ERR)

RETURN
PROCESS:
*---------------------------------------------------------
    Y.ID = 'SYSTEM'

    CALL CACHE.READ(FN.REDO.H.INVENTORY.PARAMETER,Y.ID,R.REDO.H.INVENTORY.PARAMETER,Y.ERR)

    Y.ITEM.CODE = R.NEW(RE.ORD.ITEM.CODE)
    Y.SUPPLY.CODE = R.REDO.H.INVENTORY.PARAMETER<IN.PR.ITEM.CODE>
    CHANGE @VM TO @FM IN Y.SUPPLY.CODE
    CHANGE @SM TO @FM IN Y.SUPPLY.CODE
    LOCATE Y.ITEM.CODE IN Y.SUPPLY.CODE SETTING POS ELSE
        AF=RE.ORD.ITEM.CODE
        ETEXT = "EB-ITEM.CODE.NOT.PARA"
        CALL STORE.END.ERROR
    END
    R.REDO.H.MAIN.COMPANY = ''
    Y.GROUP = R.NEW(RE.ORD.REQUEST.COMPANY)
    CALL F.READ(FN.REDO.H.REORDER.LEVEL,Y.GROUP,R.REDO.H.REORDER.LEVEL,F.REDO.H.REORDER.LEVEL,Y.ERR)
    CALL F.READ(FN.REDO.H.MAIN.COMPANY,Y.GROUP,R.REDO.H.MAIN.COMPANY,F.REDO.H.MAIN.COMPANY,Y.ERR)
    IF R.REDO.H.MAIN.COMPANY AND NOT(R.NEW(RE.ORD.BRANCH.DES)) THEN
        ETEXT = 'AZ-INP.MISS'
        AF = RE.ORD.BRANCH.CODE
        CALL STORE.END.ERROR
        RETURN
    END

    Y.DES = R.NEW(RE.ORD.BRANCH.DES)
    LOCATE Y.DES IN R.REDO.H.MAIN.COMPANY<REDO.COM.DESCRIPTION,1> SETTING POS.1.1 THEN
        R.NEW(RE.ORD.BRANCH.CODE) = R.REDO.H.MAIN.COMPANY<REDO.COM.CODE,POS.1.1>
        GOSUB USER.CHECK
    END
    IF NOT(Y.DES) THEN
        GOSUB USER.CHECK.NOT.DES
    END
RETURN
*---------------------------------------------------------
USER.CHECK.NOT.DES:
*---------------------------------------------------------
    Y.ITME.CMP.LIST = R.REDO.H.REORDER.LEVEL<RE.ORD.ITEM.VALUE>
    LOCATE Y.ITEM.CODE IN Y.ITME.CMP.LIST<1,1,1> SETTING POS.CODE THEN
        Y.FLAG.ERROR = '1'
    END
    IF NOT(Y.FLAG.ERROR) THEN
        AF = RE.ORD.ITEM.CODE
        ETEXT = 'SC-INVALID.CODE'
        CALL STORE.END.ERROR
        RETURN
    END
RETURN
*---------------------------------------------------------
USER.CHECK:
*---------------------------------------------------------

    Y.FLAG.ERROR = ''
    Y.ERROR.CMP.FLAG = ''
    Y.CODE.VAL = R.NEW(RE.ORD.BRANCH.CODE)
    Y.ITEM.FINAL = R.NEW(RE.ORD.ITEM.CODE)

    Y.CNT = 1
    Y.COUNT = DCOUNT(Y.BRANCH.LIST,@FM)

    LOCATE ID.COMPANY IN Y.BRANCH.LIST SETTING POS.BR ELSE
        Y.ERROR.CMP.FLAG = '1'
    END

    LOOP
    WHILE Y.CNT LE Y.COUNT
        IF Y.GROUP EQ Y.BRANCH.LIST<Y.CNT> AND Y.CODE.VAL EQ Y.DEPT.LIST<Y.CNT> THEN
            Y.ERROR.CMP.FLAG = '1'
        END
        Y.CNT += 1
    REPEAT

    IF NOT(Y.ERROR.CMP.FLAG) THEN
        AF = RE.ORD.BRANCH.DES
        ETEXT = 'AC-INVALID.DEPARTMENT.CODE'
        CALL STORE.END.ERROR
        RETURN
    END

    Y.CODE.CMP.LIST = R.REDO.H.REORDER.LEVEL<RE.ORD.CODE>
    Y.ITME.CMP.LIST = R.REDO.H.REORDER.LEVEL<RE.ORD.ITEM.VALUE>
    LOCATE Y.CODE.VAL IN Y.CODE.CMP.LIST<1,1> SETTING POS.CODE THEN
        Y.ITEM.POS = Y.ITME.CMP.LIST<1,POS.CODE>
        CHANGE @SM TO @FM IN Y.ITEM.POS
        CHANGE @VM TO @FM IN Y.ITEM.POS
        LOCATE Y.ITEM.FINAL IN Y.ITEM.POS SETTING POS.ITEM THEN
            Y.FLAG.ERROR = '1'
        END
    END
    IF NOT(Y.FLAG.ERROR) THEN
        AF = RE.ORD.ITEM.CODE
        ETEXT = 'SC-INVALID.CODE'
        CALL STORE.END.ERROR
        RETURN
    END
RETURN
END
