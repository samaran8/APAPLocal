* @ValidationCode : MjoyMDc1NDE1ODY2OkNwMTI1MjoxNjg0ODU0NDA2MzQ1OklUU1M6LTE6LTE6MTY5OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 23 May 2023 20:36:46
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 169
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.CHANGE.CHEQ.STATUS(SOL.CK.ID)
***********************************************************
*----------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : HARISH.Y
* PROGRAM NAME : REDO.CHANGE.CHEQ.STATUS
*----------------------------------------------------------
* DESCRIPTION : This routine will change the STATUS from 40 to 80,If it exceeds or equals the No.of.Days
*               mentioned in the REDO.CHEQ.PARAM.DATE
*------------------------------------------------------------

*    LINKED WITH : REDO.CHANGE.CHEQ.STATUS
*    IN PARAMETER: NONE
*    OUT PARAMETER: NONE
* Modification History :
*------------------------------------------------------------
*DATE             WHO         REFERENCE         DESCRIPTION
*04.03.2010      HARISH.Y     ODR-2009-12-0275  INITIAL CREATION
*Modification
* Date                  who                   Reference              
* 18-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION - No Change
* 18-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*-------------------------------------------------------------
*-------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CHEQUE.ISSUE
    $INSERT I_F.REDO.H.SOLICITUD.CK
    $INSERT I_F.REDO.H.CHEQ.CHANGE.PARAM
    $INSERT I_REDO.CHANGE.CHEQ.STATUS.COMMON
    $INSERT I_F.CHEQUE.REGISTER



    GOSUB PROCESS

RETURN
*-----------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------
    R.CHQ.ISS = ''
    CHEQUE.STATUS = '40'
    START.DATE = ''
    CALL F.READ(FN.REDO.H.SOLICITUD.CK,SOL.CK.ID,R.SOL.CK,F.REDO.H.SOLICITUD.CK,CK.ERR)
    IF R.SOL.CK THEN
        DATE.OF.REQUEST  = R.SOL.CK<REDO.H.SOL.DATE.OF.REQ>
    END
    PARAM.ID='SYSTEM'
    R.CHANGE.PARAM=''
    CALL CACHE.READ(FN.REDO.H.CHEQ.CHANGE.PARAM,PARAM.ID,R.CHANGE.PARAM,CHANGE.PARAM.ERR)
    IF R.CHANGE.PARAM THEN
        LOCATE CHEQUE.STATUS IN R.CHANGE.PARAM<REDO.CHEQ.PARAM.CHEQ.STATUS,1> SETTING POS THEN
            DAYS.TO.CHANGE = R.CHANGE.PARAM<REDO.CHEQ.PARAM.NO.OF.DAYS,POS>
            DAYS.TO.CHANGE = DAYS.TO.CHANGE:"C"
        END
    END
    CALL CDT('',DATE.OF.REQUEST,DAYS.TO.CHANGE)
    IF DATE.OF.REQUEST LE TODAY THEN
        R.SOL.CK<REDO.H.SOL.CHEQUE.STATUS> = '80'
        APP.NAME = 'REDO.H.SOLICITUD.CK'
        OFSVERSION = 'REDO.H.SOLICITUD.CK,CHEQ.STAT'
        TRANSACTION.ID = SOL.CK.ID
        OFS.ARRAY=R.SOL.CK
        GOSUB UPDATE.FILES
    END
    GOSUB ID.FORMATION

*Y.SEQ.NO=Y.LAST.EVENT.SEQ+1
    Y.SEQ.NO=FMT(Y.SEQ.NO,"R%7")
    CHEQ.ISS.ID=Y.CHEQUE.TYPE:".":Y.ACCOUNT:".":Y.SEQ.NO

    R.CHQ.ISS<CHEQUE.IS.CHEQUE.STATUS> = R.SOL.CK<REDO.H.SOL.CHEQUE.STATUS>
    R.CHQ.ISS<CHEQUE.IS.LOCAL.REF,POS.L.SOLICITUDCKID>=SOL.CK.ID

    APP.NAME = 'CHEQUE.ISSUE'
    OFSVERSION = 'CHEQUE.ISSUE,CREATE.NEW'
    TRANSACTION.ID = CHEQ.ISS.ID
    OFS.ARRAY=R.CHQ.ISS
    GOSUB UPDATE.FILES



RETURN
*-------------------------------------------------------------------
UPDATE.FILES:
*-------------------------------------------------------------------

    OFSFUNCT = 'I'
    PROCESS  = 'PROCESS'
    GTSMODE = ''
    NO.OF.AUTH = '0'
    OFSRECORD = ''
    OFS.MSG.ID =''
    OFS.SOURCE.ID = 'REDO.CHQ.ISSUE'
    OFS.ERR = ''
    CALL OFS.BUILD.RECORD(APP.NAME,OFSFUNCT,PROCESS,OFSVERSION,GTSMODE,NO.OF.AUTH,TRANSACTION.ID,OFS.ARRAY,OFSRECORD)
    CALL OFS.POST.MESSAGE(OFSRECORD,OFS.MSG.ID,OFS.SOURCE.ID,OFS.ERR)
RETURN
*-----------------------------------------------------------------------
ID.FORMATION:
*-----------------------------------------------------------------------
    Y.ACCOUNT=R.SOL.CK<REDO.H.SOL.ACCOUNT>
    Y.CHEQUE.TYPE=R.SOL.CK<REDO.H.SOL.CHEQUE.TYPE>
*Y.CHQ.REG.ID=Y.CHEQUE.TYPE:".":Y.ACCOUNT
*R.CHEQUE.REGISTER = ''
*CALL F.READ(FN.CHEQUE.REGISTER,Y.CHQ.REG.ID,R.CHEQUE.REGISTER,F.CHEQUE.REGISTER,CHQ.ERR)
*IF R.CHEQUE.REGISTER NE '' THEN
*Y.LAST.EVENT.SEQ=R.CHEQUE.REGISTER<CHEQUE.REG.LAST.EVENT.SEQ>
*END
*ELSE
*Y.LAST.EVENT.SEQ='0'
*END

    R.REDO.CONCAT.CHEQUE.REGISTER =''
    CALL F.READ(FN.REDO.CONCAT.CHEQUE.REGISTER,SOL.CK.ID,R.REDO.CONCAT.CHEQUE.REGISTER,F.REDO.CONCAT.CHEQUE.REGISTER,CONCAT.ERR)
    Y.SEQ.NO = R.REDO.CONCAT.CHEQUE.REGISTER<1>


RETURN
END
