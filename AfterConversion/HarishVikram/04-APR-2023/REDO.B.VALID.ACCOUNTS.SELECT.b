* @ValidationCode : MjotMTYxODkyMzgyMDpDcDEyNTI6MTY4MDYwNDc0MDgwNjpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 04 Apr 2023 16:09:00
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.VALID.ACCOUNTS.SELECT
*-----------------------------------------------------------------------------
* Description:
* This routine is a multithreaded routine to select the records in the mentioned applns
*------------------------------------------------------------------------------------------
* * Input / Output
* * --------------
* IN     : -NA-
* OUT    : -NA-
*------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : NATCHIMUTHU
* PROGRAM NAME : REDO.B.VALID.ACCOUNTS.SELECT
* ODR          : ODR-2010-09-0171
*
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE            	 WHO            REFERENCE         	 DESCRIPTION
* 07.10.2010    	 NATCHIMUTHU     ODR-2010-09-0171      INITIAL CREATION
* 04-APR-2023     	Conversion tool   R22 Auto conversion   FM TO @FM
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_BATCH.FILES
    $INSERT I_F.REDO.APAP.H.PARAMETER
    $INSERT I_F.REDO.ACCT.STATUS.CODE
    $INSERT I_REDO.B.VALID.ACCOUNTS.COMMON

    $INSERT I_F.REDO.INTERFACE.PARAM
    $INSERT I_F.REDO.INTERFACE.ACT
    $INSERT I_F.REDO.INTERFACE.ACT.DETAILS
    $INSERT I_F.REDO.INTERFACE.NOTIFY
    $INSERT I_F.REDO.INTERFACE.MON.TYPE
    $INSERT I_F.REDO.INTERFACE.SMAIL
    $INSERT I_F.REDO.APAP.CLEAR.PARAM
    $INSERT I_F.LOCKING
*   $INSERT I_BATCH.FILES

    GOSUB PROCESS
RETURN


*********
PROCESS:
*********
    SEL.CCY.LIST = ''
    SEL.ID = ''
    FINAL.LIST.ARRAY = ''
    POS = ''


*  IF NOT(CONTROL.LIST) THEN
*      GOSUB BUILD.CONTRL
*  END

    Y.R.REDO.APAP.CLEAR.PARAM = Y.CATEG.APERTA

    CHANGE @FM TO " OR WITH CATEGORY EQ " IN Y.R.REDO.APAP.CLEAR.PARAM

* IF CONTROL.LIST<1,1> EQ 'PROCESS' THEN
    SEL.ACCT.LIST = ''
    SEL.ACCT.CMD   = "SELECT " :FN.ACCOUNT:" WITH CATEGORY EQ " : Y.R.REDO.APAP.CLEAR.PARAM
    CALL EB.READLIST(SEL.ACCT.CMD,SEL.ACCT.LIST,'',ACCT.CNT,'')
    FINAL.LIST.ARRAY = SEL.ACCT.LIST
* END

* IF CONTROL.LIST<1,1> EQ 'CLEAR' THEN
    SEL.ACCT.LIST = ''
    SEL.AC.CLS = "SELECT ":FN.AC.CLS
    CALL EB.READLIST(SEL.AC.CLS,SEL.ACCT.LIST,'',ACCT.CNT,'')
    FINAL.LIST.ARRAY<-1> = SEL.ACCT.LIST
* END

    CALL BATCH.BUILD.LIST('',FINAL.LIST.ARRAY)

RETURN


END
