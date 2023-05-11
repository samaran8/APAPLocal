* @ValidationCode : MjoxOTkzOTAzNTEyOkNwMTI1MjoxNjgwNjE5NzU3NjMxOklUU1M6LTE6LTE6MDoxOnRydWU6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 04 Apr 2023 20:19:17
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.


$PACKAGE APAP.TAM
SUBROUTINE REDO.CCRG.RL.BAL.CUS.DET.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine REDO.CCRG.RL.BAL.CUS.DET
*
* @author hpasquel@temenos.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package redo.ccrg
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 24/03/2011 - APAP : B5
*              First Version
*
*
* Date           Who                 Ref                  Modification
* 04.04.2023    Conversion Tool      R22                 Auto Conversion     - FM TO @FM, VM TO @VM
* 04.04.2023    Shanmugapriya M      R22                 Manual Conversion   - No changes
*
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*
    $INSERT I_F.CUSTOMER
    $INSERT I_F.CATEGORY
*
    $INSERT I_F.REDO.RISK.GROUP
*
*** </region>
*-----------------------------------------------------------------------------
    GOSUB INITIALISE
    GOSUB DEFINE.FIELDS
RETURN

*-----------------------------------------------------------------------------
DEFINE.FIELDS:
*-----------------------------------------------------------------------------

* TODO Define name, type and length for the key
    ID.F = "RL.BAL.CUS.DET.ID" ; ID.N = "90" ; ID.T =  "ANY"
*
    Z=0
    Z+=1 ; F(Z)  = "CUSTOMER.ID"       ; N(Z) = "10"   ; T(Z) = "CUS"
    CHECKFILE(Z) = "CUSTOMER":@FM:EB.CUS.SHORT.NAME:@FM:'.A'

    Z+=1 ; F(Z)  = "REL.CUS.ID"       ; N(Z) = "10"   ; T(Z) = "CUS"
    CHECKFILE(Z) = "CUSTOMER":@FM:EB.CUS.SHORT.NAME:@FM:'.A'

    Z+=1 ; F(Z)  = "RISK.LIMIT.ID"     ; N(Z) = "40"   ; T(Z) = "ANY"

    Z+=1 ; F(Z)  = "RISK.GROUP.ID"     ; N(Z) = "10"   ; T(Z) = "ANY"
    CHECKFILE(Z) = "REDO.RISK.GROUP":@FM:RG.GRP.SHORT.DESC:@FM:'.A'

    Z+=1 ; F(Z)  = "XX<CATEGORY"       ; N(Z) = "6"    ; T(Z) = "" ; T(Z)<4> = "R##-###"
    CHECKFILE(Z) = "CATEGORY":@FM:EB.CAT.DESCRIPTION:@FM:'L.A'

    Z+=1 ; F(Z)  = "XX-BALANCE.TYPE"   ; N(Z) = "35"   ; T(Z) = "" : @FM : Y.BAL.TYPE.VALUES
    Z+=1 ; F(Z)  = "XX-DIR.BALANCE"    ; N(Z) = "18"   ; T(Z) = "AMT" ; T(Z)<2> = '':@VM:LCCY
    Z+=1 ; F(Z)  = "XX-INT.RECEIVABLE" ; N(Z) = "18"   ; T(Z) = "AMT" ; T(Z)<2> = '':@VM:LCCY
    Z+=1 ; F(Z)  = "XX-CON.BALANCE"    ; N(Z) = "18"   ; T(Z) = "AMT" ; T(Z)<2> = '':@VM:LCCY
    Z+=1 ; F(Z)  = "XX-TOTAL"          ; N(Z) = "18"   ; T(Z) = "AMT" ; T(Z)<2> = '':@VM:LCCY
    Z+=1 ; F(Z)  = "XX-RESERVED.1"     ; N(Z) = "35"   ; T(Z) = "" ; T(Z)<3> = 'NOINPUT'
    Z+=1 ; F(Z)  = "XX>RESERVED.2"     ; N(Z) = "35"   ; T(Z) = "" ; T(Z)<3> = 'NOINPUT'
    Z+=1 ; F(Z)  = "RESERVED.3"        ; N(Z) = "35"   ; T(Z) = "" ; T(Z)<3> = 'NOINPUT'
    V = Z + 9
*
RETURN

*-----------------------------------------------------------------------------
INITIALISE:
*-----------------------------------------------------------------------------

    MAT F = "" ; MAT N = "" ; MAT T = ""
    MAT CHECKFILE = "" ; MAT CONCATFILE = ""
    ID.CHECKFILE = "" ; ID.CONCATFILE = ""

    Y.BAL.TYPE.VALUES = 'REDO.CCRG.BAL.TYPE'
    CALL EB.LOOKUP.LIST(Y.BAL.TYPE.VALUES)
    Y.BAL.TYPE.VALUES = Y.BAL.TYPE.VALUES<2>

RETURN
*-----------------------------------------------------------------------------
END
