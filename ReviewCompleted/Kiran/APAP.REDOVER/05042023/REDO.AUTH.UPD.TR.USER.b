* @ValidationCode : MjozNzM2NDQ0NTI6Q3AxMjUyOjE2ODA2ODkxNDM3MDI6c2FtYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 15:35:43
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
SUBROUTINE REDO.AUTH.UPD.TR.USER
*-----------------------------------------------------------------------------
*------------------------------------------------------------------------------------------
* DESCRIPTION :   This routine will be executed at check Record Routine for TELLER VERSIONS
*------------------------------------------------------------------------------------------
*
* COMPANY NAME : APAP
* DEVELOPED BY : VICTOR NAVA
* PROGRAM NAME : REDO.V.CHK.FX.OVR
*
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO                REFERENCE                   DESCRIPTION
*05-04-2023  Conversion Tool      R22 Auto Code conversion      FM TO @FM, VM TO @VM
*05-04-2023       Samaran T       Manual R22 Code Conversion         No Changes
* -----------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_System
    $INSERT I_REDO.FX.OVR.COMMON
*
    $INSERT I_F.FOREX
    $INSERT I_F.MM.MONEY.MARKET
    $INSERT I_F.SEC.TRADE


    LOC.REF.APPL="FOREX":@FM:"MM.MONEY.MARKET":@FM:"SEC.TRADE"
    LOC.REF.FIELDS="L.LIMIT.OVR":@VM:"L.CR.USER.APAP":@VM:"L.TR.USER.APAP":@FM:"L.LIMIT.OVR":@VM:"L.CR.USER.APAP":@VM:"L.TR.USER.APAP":@FM:"L.LIMIT.OVR":@VM:"L.CR.USER.APAP":@VM:"L.TR.USER.APAP"
    LOC.REF.POS=" "
    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,LOC.REF.POS)
    Y.FX.LIMIT.OVR.POS  = LOC.REF.POS<1,1>
    Y.FX.CR.USER.POS =  LOC.REF.POS<1,2>
    Y.FX.TR.USER.POS =  LOC.REF.POS<1,3>

    Y.MM.LIMIT.OVR.POS  = LOC.REF.POS<2,1>
    Y.MM.CR.USER.POS =  LOC.REF.POS<2,2>
    Y.MM.TR.USER.POS =  LOC.REF.POS<2,3>

    Y.SC.LIMIT.OVR.POS  = LOC.REF.POS<3,1>
    Y.SC.CR.USER.POS =  LOC.REF.POS<3,2>
    Y.SC.TR.USER.POS =  LOC.REF.POS<3,3>

    Y.APPLICATION = APPLICATION

    BEGIN CASE
        CASE Y.APPLICATION EQ "FOREX"
            GOSUB FX.DEFAULT.PARA
        CASE Y.APPLICATION EQ "MM.MONEY.MARKET"
            GOSUB MM.DEFAULT.PARA
        CASE Y.APPLICATION EQ "SEC.TRADE"
            GOSUB SC.DEFAULT.PARA
    END CASE

RETURN
*****************
FX.DEFAULT.PARA:
*****************
    R.NEW(FX.LOCAL.REF)<1,Y.FX.TR.USER.POS>    = OPERATOR
RETURN
*****************
MM.DEFAULT.PARA:
*****************
    R.NEW(MM.LOCAL.REF)<1,Y.MM.TR.USER.POS>    = OPERATOR
RETURN
*****************
SC.DEFAULT.PARA:
*****************
    R.NEW(SC.SBS.LOCAL.REF)<1,Y.SC.TR.USER.POS> = OPERATOR
RETURN
END
