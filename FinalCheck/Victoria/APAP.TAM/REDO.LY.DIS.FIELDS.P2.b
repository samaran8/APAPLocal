* @ValidationCode : MjoxMTYxMjg2MjA5OkNwMTI1MjoxNjgxMjExMjMwMDM0OjMzM3N1Oi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 16:37:10
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.LY.DIS.FIELDS.P2
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* The functionality is to disable EXC.COND or INC.COND fields according
* if value parameterized in the APP.EXC.COND or APP.INC.COND fields is "TODAS" (All)
*
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
*   Date               who           Reference            Description
* 26-Aug-2011       RMONDRAGON       ODR-2011-06-0243     First Version
* 29-Nov-2011       RMONDRAGON       ODR-2011-06-0243     Update
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*11/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             NOCHANGE
*11/04/2023         SURESH           MANUAL R22 CODE CONVERSION           CALL Rtn format modified
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.LY.PROGRAM

    IF VAL.TEXT THEN
        VAR.APP.INC.COND = R.NEW(REDO.PROG.APP.INC.COND)
    END ELSE
        VAR.APP.INC.COND = COMI
    END

    GOSUB PROCESS

RETURN

********
PROCESS:
********

    IF VAL.TEXT EQ '' THEN
        VAL.TEXT = 'VALIDATED'
        CALL APAP.REDOVER.REDO.V.AVAIL.PROGRAM ;*MANUAL R22 CODE CONVERSION
        CALL APAP.REDOVER.REDO.V.DISDELAY ;*MANUAL R22 CODE CONVERSION
        CALL APAP.TAM.REDO.LY.DIS.FIELDS.T ;*MANUAL R22 CODE CONVERSION
        CALL APAP.TAM.REDO.LY.DIS.FIELDS.P ;*MANUAL R22 CODE CONVERSION
        VAL.TEXT = ''
    END ELSE
        CALL APAP.REDOVER.REDO.V.AVAIL.PROGRAM ;*MANUAL R22 CODE CONVERSION
        CALL APAP.REDOVER.REDO.V.DISDELAY ;*MANUAL R22 CODE CONVERSION
        CALL APAP.TAM.REDO.LY.DIS.FIELDS.T ;*MANUAL R22 CODE CONVERSION
        CALL APAP.TAM.REDO.LY.DIS.FIELDS.P ;*MANUAL R22 CODE CONVERSION
    END

    VAR.COND.TYPE.EXINC=R.NEW(REDO.PROG.COND.TYPE.EXINC)
    VAR.APP.EXC.COND=R.NEW(REDO.PROG.APP.EXC.COND)

    IF VAR.APP.EXC.COND EQ 'TODAS' AND VAR.APP.INC.COND EQ 'TODAS' THEN
        AF = REDO.PROG.APP.INC.COND
        ETEXT='EB-APP.COND'
        CALL STORE.END.ERROR
    END

    IF VAR.COND.TYPE.EXINC EQ 'ESTADO.CUENTA' AND VAR.APP.INC.COND EQ 'ESPECIFICA' THEN
        GOSUB PROCESS1
    END

    IF VAR.COND.TYPE.EXINC EQ 'ESTADO.CUENTA' AND VAR.APP.INC.COND EQ 'TODAS' THEN
        T(REDO.PROG.INC.EST.ACCT)<3>='NOINPUT'
        GOSUB PROCESS1
    END

    IF VAR.COND.TYPE.EXINC EQ 'ESTADO.PRESTAMO' AND VAR.APP.INC.COND EQ 'ESPECIFICA' THEN
        GOSUB PROCESS2
    END

    IF VAR.COND.TYPE.EXINC EQ 'ESTADO.PRESTAMO' AND VAR.APP.INC.COND EQ 'TODAS' THEN
        T(REDO.PROG.INC.EST.LOAN)<3>='NOINPUT'
        GOSUB PROCESS2
    END

    IF VAR.COND.TYPE.EXINC EQ 'CONDICION.PRESTAMO' AND VAR.APP.INC.COND EQ 'ESPECIFICA' THEN
        GOSUB PROCESS3
    END

    IF VAR.COND.TYPE.EXINC EQ 'CONDICION.PRESTAMO' AND VAR.APP.INC.COND EQ 'TODAS' THEN
        T(REDO.PROG.INC.COND.LOAN)<3>='NOINPUT'
        GOSUB PROCESS3
    END

    IF VAR.COND.TYPE.EXINC EQ 'MCC.TDEBITO' OR VAR.COND.TYPE.EXINC EQ 'MERCHANTID.TDEBITO' AND VAR.APP.INC.COND EQ 'ESPECIFICA' THEN
        GOSUB PROCESS4
    END

    IF VAR.COND.TYPE.EXINC EQ 'MCC.TDEBITO' OR VAR.COND.TYPE.EXINC EQ 'MERCHANTID.TDEBITO' AND VAR.APP.INC.COND EQ 'TODAS' THEN
        T(REDO.PROG.INC.COND)<3>='NOINPUT'
        GOSUB PROCESS4
    END

RETURN

*--------
PROCESS1:
*--------

    Y.EXC.EST.ACCT = R.NEW(REDO.PROG.EXC.EST.ACCT)
    IF Y.EXC.EST.ACCT EQ '' THEN
        T(REDO.PROG.EXC.EST.ACCT)<3>='NOINPUT'
    END

    T(REDO.PROG.EXC.EST.LOAN)<3>='NOINPUT'
    T(REDO.PROG.EXC.COND.LOAN)<3>='NOINPUT'
    T(REDO.PROG.EXC.COND)<3>='NOINPUT'
    T(REDO.PROG.INC.EST.LOAN)<3>='NOINPUT'
    T(REDO.PROG.INC.COND.LOAN)<3>='NOINPUT'
    T(REDO.PROG.INC.COND)<3>='NOINPUT'

RETURN

*--------
PROCESS2:
*--------

    Y.EXC.EST.LOAN = R.NEW(REDO.PROG.EXC.EST.LOAN)
    IF Y.EXC.EST.LOAN EQ '' THEN
        T(REDO.PROG.EXC.EST.LOAN)='NOINPUT'
    END

    T(REDO.PROG.EXC.EST.ACCT)<3>='NOINPUT'
    T(REDO.PROG.EXC.COND.LOAN)<3>='NOINPUT'
    T(REDO.PROG.EXC.COND)<3>='NOINPUT'
    T(REDO.PROG.INC.EST.ACCT)<3>='NOINPUT'
    T(REDO.PROG.INC.COND.LOAN)<3>='NOINPUT'
    T(REDO.PROG.INC.COND)<3>='NOINPUT'

RETURN

*--------
PROCESS3:
*--------

    Y.EXC.COND.LOAN = R.NEW(REDO.PROG.EXC.COND.LOAN)
    IF Y.EXC.COND.LOAN EQ '' THEN
        T(REDO.PROG.EXC.COND.LOAN)='NOINPUT'
    END

    T(REDO.PROG.EXC.EST.ACCT)<3>='NOINPUT'
    T(REDO.PROG.EXC.EST.LOAN)='NOINPUT'
    T(REDO.PROG.EXC.COND)<3>='NOINPUT'
    T(REDO.PROG.INC.EST.ACCT)<3>='NOINPUT'
    T(REDO.PROG.INC.EST.LOAN)<3>='NOINPUT'
    T(REDO.PROG.INC.COND)<3>='NOINPUT'

RETURN

*--------
PROCESS4:
*--------

    Y.EXC.COND = R.NEW(REDO.PROG.EXC.COND)
    IF Y.EXC.COND EQ '' THEN
        T(REDO.PROG.EXC.COND)='NOINPUT'
    END

    T(REDO.PROG.EXC.EST.ACCT)<3>='NOINPUT'
    T(REDO.PROG.EXC.EST.LOAN)='NOINPUT'
    T(REDO.PROG.EXC.COND.LOAN)='NOINPUT'
    T(REDO.PROG.INC.EST.ACCT)<3>='NOINPUT'
    T(REDO.PROG.INC.EST.LOAN)<3>='NOINPUT'
    T(REDO.PROG.INC.COND.LOAN)<3>='NOINPUT'

RETURN

END
