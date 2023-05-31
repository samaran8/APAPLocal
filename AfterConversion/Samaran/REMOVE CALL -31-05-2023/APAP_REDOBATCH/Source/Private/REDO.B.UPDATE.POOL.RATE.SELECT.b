* @ValidationCode : MjotMTAxOTU5MzM1NTpDcDEyNTI6MTY4NDg1NDQwMTQxNDpJVFNTOi0xOi0xOjE5NzoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 23 May 2023 20:36:41
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 197
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.UPDATE.POOL.RATE.SELECT
*-----------------------------------------------------------------------------------------------------------------------------
*DESCRIPTION:
* This routine is the select routine of the COB job REDO.B.UPDATE.POOL.REATE which updates the local reference field TASA.POOL
*  based on the value in the local table POOL.RATE and other related local reference fields in ACCOUNT and AZ.ACCOUNT
* This routine selects ACCOUNT and AZ.ACCOUNT records with value 'Yes' in the local reference field AUTOMATIC.REVIEW
* ----------------------------------------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS     : -NA-
* CALLED BY : -NA-
*
* Revision History:
*---------------------------------------------------------------------------------------------
*   Date               who           Reference            Description
*---------------------------------------------------------------------------------------------
* 03-MAY-2010  N.Satheesh Kumar   ODR-2009-10-0325       Initial Creation
* 23-DEC-2010  JEEVA T             PACS00171685          COB PERFORMANCE
* 17-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION - FM TO @FM
* 17-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*____---------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_F.DATES
    $INSERT I_REDO.B.UPDATE.POOL.RATE.COMMON

    IF CONTROL.LIST EQ '' THEN
        CONTROL.LIST = 'ACCOUNT':@FM:'AZ.ACCOUNT'
    END

    IF CONTROL.LIST<1,1> EQ 'ACCOUNT' THEN
*SEL.CMD = 'SELECT ':FN.ACCOUNT:' WITH L.EB.REVIEW EQ "YES"'
        CALL F.READ(FN.REDO.W.UPD.REVIEW.ACCT,'ACCOUNT',R.REDO.W.UPD.REVIEW.ACCT,F.REDO.W.UPD.REVIEW.ACCT,Y.ERR)
        SEL.LIST = R.REDO.W.UPD.REVIEW.ACCT
        Y.APPL='ACCOUNT'
    END ELSE
*SEL.CMD = 'SELECT ':FN.AZ.ACCOUNT:' WITH L.EB.REVIEW EQ "YES" AND ROLLOVER.DATE GT ':R.DATES(EB.DAT.LAST.WORKING.DAY):' AND ROLLOVER.DATE LE ':TODAY
        CALL F.READ(FN.REDO.W.UPD.REVIEW.ACCT,'AZ.ACCOUNT',R.REDO.W.UPD.REVIEW.ACCT,F.REDO.W.UPD.REVIEW.ACCT,Y.ERR)
        SEL.LIST = R.REDO.W.UPD.REVIEW.ACCT
        Y.APPL='AZ.ACCOUNT'
    END
*CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,Y.ERR)
    CALL BATCH.BUILD.LIST('',SEL.LIST)
RETURN
END
