* @ValidationCode : MjotMTY1MzkxNjQ2ODpDcDEyNTI6MTY4MjQxMjM0NjA5OTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MTpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:46
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.DEFAULT.DEPT.ACCT.NO
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This routine is to default internal account FT versions
* Input/Output:
*--------------
* IN : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS : @ID
* CALLED BY :
*
* Revision History:
*------------------------------------------------------------------------------------------
*   Date               who                       Reference                      Description
* 04-08-2011         Bharath G                 PACS00100502               Default internal Acct from REDO.ISSUE.DEPT.CODE.
*11-04-2023         Conversion Tool        R22 Auto Code conversion          No Changes
*11-04-2023          Samaran T             R22 Manual Code Conversion         No Changes
 
 
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.ISSUE.DEPT.CODE

    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN

*----*
INIT:
*----*
    Y.DEPT.ID = COMI

RETURN
*---------*
OPEN.FILES:
*---------*
    FN.REDO.ISSUE.DEPT.CODE ='F.REDO.ISSUE.DEPT.CODE'
    F.REDO.ISSUE.DEPT.CODE = ''
    R.REDO.ISSUE.DEPT.CODE = ''
    CALL OPF(FN.REDO.ISSUE.DEPT.CODE,F.REDO.ISSUE.DEPT.CODE)

RETURN
*-------*
PROCESS:
*-------*
    CALL CACHE.READ(FN.REDO.ISSUE.DEPT.CODE,Y.DEPT.ID,R.REDO.ISSUE.DEPT.CODE,Y.ERR)
    IF R.REDO.ISSUE.DEPT.CODE THEN
        R.NEW(FT.CREDIT.ACCT.NO) =  R.REDO.ISSUE.DEPT.CODE<REDO.IDC.DEPT.ACCT.NO>
    END

RETURN
END
