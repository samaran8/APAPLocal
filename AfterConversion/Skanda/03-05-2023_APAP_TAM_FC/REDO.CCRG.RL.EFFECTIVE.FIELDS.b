* @ValidationCode : MjotNDE1NDc3MzY4OkNwMTI1MjoxNjgwNjE5NzU3NzMxOklUU1M6LTE6LTE6MDoxOnRydWU6Ti9BOlIyMV9BTVIuMDotMTotMQ==
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
SUBROUTINE REDO.CCRG.RL.EFFECTIVE.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine REDO.CCRG.RL.EFFECTIVE
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
* 04.04.2023    Conversion Tool      R22                 Auto Conversion     - FM TO @FM
* 04.04.2023    Shanmugapriya M      R22                 Manual Conversion   - No changes
*
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
    $INSERT I_F.USER
    $INSERT I_F.CUSTOMER

*** </region>
*-----------------------------------------------------------------------------
    GOSUB INITIALISE
    GOSUB DEFINE.FIELDS
RETURN

*-----------------------------------------------------------------------------
DEFINE.FIELDS:
*-----------------------------------------------------------------------------

* TODO Define name, type and length for the key
    ID.F = "RL.EFFECTIVE.ID" ; ID.N = "35" ; ID.T =  "A"
    Z=0
    Z+=1 ; F(Z)  = "USER.ID"           ; N(Z) = "35"   ; T(Z) = "A" : @FM : "" : @FM : ""
    CHECKFILE(Z) = "USER":@FM:EB.USE.USER.NAME
    Z+=1 ; F(Z)  = "CUSTOMER.ID"       ; N(Z) = "10"   ; T(Z) = "CUS" : @FM : "" : @FM : ""
    CHECKFILE(Z) = "CUSTOMER":@FM:EB.CUS.SHORT.NAME:@FM:'.A'
    Z+=1 ; F(Z)  = "START.DATE"        ; N(Z) = "11"   ; T(Z) = "RELTIME":@FM:@FM:"":@FM:"RDD DD  DD ##:##"
    Z+=1 ; F(Z)  = "LINK.1"            ; N(Z) = "35"   ; T(Z) = "A"
    Z+=1 ; F(Z)  = "LINK.2"            ; N(Z) = "35"   ; T(Z) = "A"
    Z+=1 ; F(Z)  = "RESERVED.1"        ; N(Z) = "35"   ; T(Z) = "" ; T(Z)<3> = 'NOINPUT'
    Z+=1 ; F(Z)  = "RESERVED.2"        ; N(Z) = "35"   ; T(Z) = "" ; T(Z)<3> = 'NOINPUT'
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

RETURN
*-----------------------------------------------------------------------------
END
