* @ValidationCode : Mjo3NTc4ODI2NDI6Q3AxMjUyOjE2ODU1MzYwNzY2MjY6c2FtYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 31 May 2023 17:57:56
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
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.VAL.MAT.DATE
*-----------------------------------------------------------------------------
*Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By : Temenos Application Management
*Program Name : REDO.CARD.GENERATION.RECORD
*------------------------------------------------------------------------------
*Description : REDO.APAP.VAL.MAT.DATE is a validation routine for
* the version AZ.ACCOUNT, OPEN.CPH which populates the
* field L.VAL.MAT.DATE
*Linked With : AZ.ACCOUNT,OPEN.CPH
*In Parameter : N/A
*Out Parameter: N/A
*Files Used : AZ.ACCOUNT
*-------------------------------------------------------------------------------
* Modification History :
*-----------------------
* Date Who Reference Description
* ------ ------ ------------- -------------
* 29-07-2010 JEEVA T ODR-2009-10-0346 B.21 Initial Creation
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*19-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   NO CHANGE
*19-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION CALL RTN FORMAT MODIFIED
*----------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------

*********************************************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System
    $INSERT I_F.AZ.ACCOUNT
	$USING APAP.REDOVER

**************************************************************************
**********
MAIN.PARA:
**********

    GOSUB PROCESS.PARA
RETURN
**************************************************************************
*************
PROCESS.PARA:
*************

    Y.MAT.DATE=COMI
    Y.VAL.DATE=R.NEW(AZ.VALUE.DATE)
    Y.VAL.MAT.DATE=Y.VAL.DATE:'-':Y.MAT.DATE
    CALL System.setVariable("CURRENT.MAT.DATE",Y.MAT.DATE)
    GOSUB FIND.MULTI.LOCAL.REF
    R.NEW(AZ.LOCAL.REF)<1,LOC.L.VAL.MAT.DATE.POS>=Y.VAL.MAT.DATE
    APAP.REDOVER.redoVPrincipalIntRate();* R22 Manual conversion
RETURN
**************************************************************************
*************
FIND.MULTI.LOCAL.REF:
*************
    APPL.ARRAY='AZ.ACCOUNT'
    FLD.ARRAY='L.VAL.MAT.DATE'
    FLD.POS=''
    CALL GET.LOC.REF(APPL.ARRAY,FLD.ARRAY,FLD.POS)
    LOC.L.VAL.MAT.DATE.POS=FLD.POS<1,1>
RETURN
**************************************************************************
END
