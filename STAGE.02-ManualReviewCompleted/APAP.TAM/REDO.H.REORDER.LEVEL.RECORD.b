* @ValidationCode : MjotNjE5MjY1MTY5OkNwMTI1MjoxNjgxMjEwMTM0MTk3OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Apr 2023 16:18:54
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.H.REORDER.LEVEL.RECORD
*-----------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------------
*DESCRIPTION : This routine is used to check the ID value for the table REDO.PART.TT.PROCESS
*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : JEEVA T
* PROGRAM NAME : REDO.GET.DEPT.CODE
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO                REFERENCE         DESCRIPTION
* 11.11.2010      JEEVA T         ODR-2010-08-0017  INITIAL CREATION
* 11.04.2023     Conversion Tool       R22          Auto Conversion     - FM TO @FM, VM TO @VM, SM TO @SM
* 11.04.2023     Shanmugapriya M       R22          Manual Conversion   - No changes
*
* ----------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.H.REORDER.LEVEL
    $INSERT I_F.REDO.H.MAIN.COMPANY
    $INSERT I_F.REDO.H.ITEM.DETAILS
    $INSERT I_F.LOCKING

    GOSUB PROCESS

RETURN

*----------------------------------------------------------------------------
PROCESS:


    FN.REDO.H.ITEM.DETAILS = 'F.REDO.H.ITEM.DETAILS'
    F.REDO.H.ITEM.DETAILS = ''
    CALL OPF(FN.REDO.H.ITEM.DETAILS,F.REDO.H.ITEM.DETAILS)

    FN.REDO.H.MAIN.COMPANY= 'F.REDO.H.MAIN.COMPANY'
    F.REDO.H.MAIN.COMPANY = ''
    CALL OPF(FN.REDO.H.MAIN.COMPANY,F.REDO.H.MAIN.COMPANY)

    R.REDO.H.MAIN.COMPANY = ''
    CALL CACHE.READ(FN.REDO.H.ITEM.DETAILS,'SYSTEM',R.REDO.H.ITEM.DETAILS,Y.ERRR)
    Y.ITEM.LIST = R.REDO.H.ITEM.DETAILS<IT.DT.SUPPLY.CODE>
    CHANGE @VM TO '_' IN Y.ITEM.LIST
    CHANGE @FM TO '_' IN Y.ITEM.LIST
    CHANGE @SM TO '_' IN Y.ITEM.LIST
    T(RE.ORD.ITEM.VALUE) = '':@FM:Y.ITEM.LIST

    CALL F.READ(FN.REDO.H.MAIN.COMPANY,ID.NEW,R.REDO.H.MAIN.COMPANY,F.REDO.H.MAIN.COMPANY,Y.ERR)
    IF NOT(R.REDO.H.MAIN.COMPANY) THEN
        T(RE.ORD.CODE)<3> = 'NOINPUT'
    END

RETURN
*------------------------------------------------------------------------------
END
