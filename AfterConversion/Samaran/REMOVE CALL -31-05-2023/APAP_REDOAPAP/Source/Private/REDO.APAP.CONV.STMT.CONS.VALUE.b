* @ValidationCode : MjoxMDc2NjM4ODU2OkNwMTI1MjoxNjg0ODM2MDM2NDMzOklUU1M6LTE6LTE6LTE1OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 23 May 2023 15:30:36
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : -15
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.CONV.STMT.CONS.VALUE
*-----------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.CONV.STMT.CONS.VALUE
*------------------------------------------------------------------------------
*Description  : This is a conversion routine used to fetch the value of LAST.TRANS.DATE from ACCOUNT
*Linked With  :
*In Parameter : O.DATA
*Out Parameter: O.DATA
*-------------------------------------------------------------------------------
* Modification History :
*-----------------------
*  Date            Who                        Reference                    Description
* ------          ------                      -------------                -------------
* 12-11-2010      Sakthi Sellappillai         ODR-2010-08-0173 N.73       Initial Creation
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*12-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   NO CHANGE
*12-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    GOSUB INITIALISE
    GOSUB PROCESS
RETURN
*------------------------------------------------------------------------------------
INITIALISE:
*------------------------------------------------------------------------------------
    Y.TEMP.VALUE = ''
RETURN
*------------------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------------------
    Y.TEMP.VALUE = O.DATA
    O.DATA = "FD7=" : Y.TEMP.VALUE : "^^"
RETURN
*-------------------------------------------------------------------------------------
END
