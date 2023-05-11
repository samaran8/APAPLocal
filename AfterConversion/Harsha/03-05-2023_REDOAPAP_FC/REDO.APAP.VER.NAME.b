* @ValidationCode : MjoxNDc5MDYxOTU0OkNwMTI1MjoxNjgyNTAyNjc2NTA2OklUU1M6LTE6LTE6Mzg3OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 26 Apr 2023 15:21:16
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 387
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.VER.NAME
*----------------------------------------------------------------------------
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: JEYACHANDRAN S
* PROGRAM NAME:
* ODR NO      :
*----------------------------------------------------------------------
* DESCRIPTION  :This routine is used to display the versions based upon the user selection in Payment Type field
* IN PARAMETER :NA
* OUT PARAMETER:NA
* LINKED WITH  :
* LINKED FILE  :
*----------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO                 REFERENCE           DESCRIPTION
* 28.09.2010   Jeyachandran S                           INITIAL CREATION
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*19-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  FM to @FM , VM to @VM
*19-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------
*-------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.MULTI.TRANSACTION.SERVICE
    $INSERT I_F.MULTI.TRANSACTION.PARAMETER
    $INSERT I_F.TELLER.ID

    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

*--------------
OPENFILES:

    FN.MULTI.TRANSACTION.SERVICE = 'F.MULTI.TRANSACTION.SERVICE'
    F.MULTI.TRANSACTION.SERVICE = ''
    CALL OPF(FN.MULTI.TRANSACTION.SERVICE,F.MULTI.TRANSACTION.SERVICE)

    FN.MULTI.TRANSACTION.PARAMETER = 'F.MULTI.TRANSACTION.PARAMETER'
    F.MULTI.TRANSACTION.PARAMETER = ''
    CALL OPF(FN.MULTI.TRANSACTION.PARAMETER,F.MULTI.TRANSACTION.PARAMETER)

    FN.TELLER.ID = 'F.TELLER.ID'
    F.TELLER.ID = ''
    CALL OPF(FN.TELLER.ID,F.TELLER.ID)
RETURN

*-----------
PROCESS:

    Y.SEL.DESC = COMI
    Y.ID = R.NEW(REDO.MTS.PAYMENT.MODE)<1,AV>
    CALL F.READ(FN.MULTI.TRANSACTION.PARAMETER,Y.ID,R.MULTI.TRANSACTION.PARAMETER,F.MULTI.TRANSACTION.PARAMETER,F.ERR)
    Y.DESC<-1> = R.MULTI.TRANSACTION.PARAMETER<REDO.TXN.PARAM.SHORT.DESC>
    Y.DESC = CHANGE(Y.DESC,@VM,@FM)
    LOCATE Y.SEL.DESC IN Y.DESC SETTING POS THEN
        Y.VERSION = R.MULTI.TRANSACTION.PARAMETER<REDO.TXN.PARAM.VERSION,POS>
        R.NEW(REDO.MTS.VERSION.NAME)<1,AV> = Y.VERSION
    END

    Y.RESIDUAL.VAL = R.NEW(REDO.MTS.RESIDUAL)
    IF Y.RESIDUAL.VAL EQ 'NO' THEN
        T(REDO.MTS.RESIDUAL.MODE)<3> = 'NOINPUT'
    END

    Y.TYPE = R.NEW(REDO.MTS.SETTLEMENT.TYPE)
    Y.OPERATION = R.NEW(REDO.MTS.OPERATION)
    IF Y.TYPE EQ 'MULTIPLE' THEN
        T(REDO.MTS.ARRANGEMENT.ID)<3> = 'NOINPUT'
*        T(REDO.MTS.DR.CR.ACCOUNT.NO)<3> = 'NOINPUT'
    END

    IF Y.ID EQ 'Cash' THEN
        T(REDO.MTS.ACCOUNT.SETTLEMENT)<3> = 'NOINPUT'
    END

RETURN
END
