* @ValidationCode : MjoxMjI4Mzk5ODE4OkNwMTI1MjoxNjgxMjk1NTQ1NDg1OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 16:02:25
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
$PACKAGE APAP.REDOEB
SUBROUTINE REDO.EB.MSG.GEN
*-----------------------------------------------------------------------------

*------------
*DESCRIPTION:
*------------
*This routine is attached as a SIGN ON routine for ARC-IB user.This will change the
*to status of EB.SECURE.MESSAGE to UNREAD if TO.CUSTOMER has entry in REDO.T.MSG.DET

*--------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-

*--------------
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-

*------------------
* Revision History:
*------------------
*   Date               who           Reference            Description
* 10-FEB-2010        Prabhu.N       ODR-2009-12-0279    Initial Creation
*
* 12-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 12-APR-2023     Conversion tool    R22 Auto conversion       No changes
*------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.EB.SECURE.MESSAGE
    $INSERT I_F.REDO.T.MSG.DET
    $INSERT I_EB.EXTERNAL.COMMON
    GOSUB INIT
    GOSUB PROCESS
RETURN
*----
INIT:
*----

    FN.REDO.T.MSG.DET='F.REDO.T.MSG.DET'
    F.REDO.T.MSG.DET=''
    CALL OPF(FN.REDO.T.MSG.DET,F.REDO.T.MSG.DET)

    FN.EB.SECURE.MESSAGE='F.EB.SECURE.MESSAGE'
    F.EB.SECURE.MESSAGE=''
    CALL OPF(FN.EB.SECURE.MESSAGE,F.EB.SECURE.MESSAGE)
RETURN
*--------
PROCESS:
*---------


*---------------------------------------------------------------------------------------------------
*  Set TO.STATUS ,DATE,TIME fields if TO.CUSTOMER has entry in EB.SECURE.MESSAGE
*---------------------------------------------------------------------------------------------------
    CALL F.READ(FN.REDO.T.MSG.DET,EB.EXTERNAL$CUSTOMER,R.MSG.DET,F.REDO.T.MSG.DET,ERR)
    IF R.MSG.DET NE '' THEN
        Y.SECURE.MSG.ID=R.MSG.DET
        CALL F.READ(FN.EB.SECURE.MESSAGE,Y.SECURE.MSG.ID,R.EB.SECURE.MSG,F.EB.SECURE.MESSAGE,SE.ERR)
        R.EB.SECURE.MSG<EB.SM.TO.STATUS>='UNREAD'
        R.EB.SECURE.MSG<EB.SM.DATE.SENT>=TODAY
        VAR.CUR.TIME = OCONV(TIME(),"MTS")
        VAR.CUR.HOURS = VAR.CUR.TIME[1,2]
        VAR.CUR.MIN = VAR.CUR.TIME[4,2]
        VAR.CUR.SEC = VAR.CUR.TIME[7,2]
        R.EB.SECURE.MSG<EB.SM.TIME.SENT>=VAR.CUR.HOURS :':': VAR.CUR.MIN :':': VAR.CUR.SEC

*    WRITE R.EB.SECURE.MSG TO F.EB.SECURE.MESSAGE,Y.SECURE.MSG.ID ;*Tus Start
        CALL F.WRITE(FN.EB.SECURE.MESSAGE,Y.SECURE.MSG.ID,R.EB.SECURE.MSG) ;*Tus End
    END

RETURN
END
