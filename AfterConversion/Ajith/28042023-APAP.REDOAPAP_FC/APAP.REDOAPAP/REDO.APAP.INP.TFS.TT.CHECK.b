* @ValidationCode : MjoxNTg1ODU2Njk0OkNwMTI1MjoxNjgyNjY5ODc4Njg0OmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 28 Apr 2023 13:47:58
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
*-----------------------------------------------------------------------------
* <Rating>-127</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.APAP.INP.TFS.TT.CHECK
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.INP.TFS.TT.CHECK
*--------------------------------------------------------------------------------------------------------
*Description       : This is an INPUT routine, the routine checks if the user has sufficient amount with
*                    the currency that is been defined in the TELLER.PARAMETER for different currencies
*Linked With       : Version T24.FUND.SERVICES,REDO.MULTI.TXN
*In  Parameter     : N/A
*Out Parameter     : N/A
*Files  Used       : T24.FUND.SERVICES                   As          I       Mode
*                    TFS.TRANSACTION                     As          I       Mode
*                    TELLER.PARAMETER                    As          I       Mode
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date               Who                  Reference                 Description
*   ------             -----               -------------              -------------
* 21 July 2010     Shiva Prasad Y      ODR-2009-10-0318 B.126        Initial Creation
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*13-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   NO CHANGE
*13-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION FMto@FM,SMto@SM,VMto@VM
*----------------------------------------------------------------------------------------
*
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.TFS.TRANSACTION
    $INSERT I_F.TELLER.PARAMETER
    $INSERT I_F.USER
*--------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
* This is the para from where the execution of the code starts

    GOSUB OPEN.PARA
    GOSUB PROCESS.PARA

RETURN
*--------------------------------------------------------------------------------------------------------
**********
OPEN.PARA:
**********
* In this para of the code, file variables are initialised and opened
    FN.TFS.TRANSACTION = 'F.TFS.TRANSACTION'
    F.TFS.TRANSACTION  = ''
    CALL OPF(FN.TFS.TRANSACTION,F.TFS.TRANSACTION)

    FN.TELLER.PARAMETER = 'F.TELLER.PARAMETER'
    F.TELLER.PARAMETER  = ''
    CALL OPF(FN.TELLER.PARAMETER,F.TELLER.PARAMETER)

RETURN
*--------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************
* This is the main processing para
    GOSUB FIND.MULTI.LOCAL.REF
    GOSUB GET.TT.PARAM.USER.DET
    GOSUB CHECK.TFS.TXN.CODE

RETURN
*--------------------------------------------------------------------------------------------------------
**********************
GET.TT.PARAM.USER.DET:
**********************

    TELLER.PARAMETER.ID = ID.COMPANY
    GOSUB READ.TELLER.PARAMETER

    Y.DESGN.LIST   = R.TELLER.PARAMETER<TT.PAR.LOCAL.REF><1,LOC.L.TT.DESGN.POS>
    Y.CASHIER.ROLE = R.USER<EB.USE.LOCAL.REF><1,LOC.L.US.CASIER.ROL.POS>

RETURN
*--------------------------------------------------------------------------------------------------------
*******************
CHECK.TFS.TXN.CODE:
*******************
    Y.TXN.CODES = R.NEW(TFS.TRANSACTION)
    CHANGE @VM TO @FM IN Y.TXN.CODES ;*R22 MANUAL CODE CONVERSION

    Y.CODE.COUNT = DCOUNT(Y.TXN.CODES,@FM) ;*R22 MANUAL CODE CONVERSION
    Y.COUNT = 1

    LOOP
    WHILE Y.COUNT LE Y.CODE.COUNT
        TFS.TRANSACTION.ID = Y.TXN.CODES<Y.COUNT>
        GOSUB READ.TFS.TRANSACTION
        IF R.TFS.TRANSACTION<TFS.TXN.INTERFACE.TO> NE 'TT' THEN
            Y.COUNT += 1
            CONTINUE
        END

        Y.TFS.CCY = R.NEW(TFS.CURRENCY)<1,Y.COUNT>
        Y.TFS.AMT = R.NEW(TFS.AMOUNT)<1,Y.COUNT>
        GOSUB CHECK.DESGN.CCY.AMT
        GOSUB CHECK.AMOUNT

        IF TEXT THEN
            EXIT
        END
        Y.COUNT += 1
    REPEAT

RETURN
*--------------------------------------------------------------------------------------------------------
********************
CHECK.DESGN.CCY.AMT:
********************
    Y.DESGN.COUNT = DCOUNT(Y.DESGN.LIST,@SM) ;*R22 MANUAL CODE CONVERSION
    Y.DES.COUNT   = 1

    LOOP
    WHILE Y.DES.COUNT LE Y.DESGN.COUNT
        Y.TT.DESGN = R.TELLER.PARAMETER<TT.PAR.LOCAL.REF><1,LOC.L.TT.DESGN.POS,Y.DES.COUNT>
        Y.TT.CCY   = R.TELLER.PARAMETER<TT.PAR.LOCAL.REF><1,LOC.L.TT.LIMIT.CCY.POS,Y.DES.COUNT>

        IF Y.TT.DESGN EQ Y.CASHIER.ROLE AND Y.TT.CCY EQ Y.TFS.CCY THEN
            Y.TT.AMT  = R.TELLER.PARAMETER<TT.PAR.LOCAL.REF><1,LOC.L.TT.LIMIT.AMT.POS,Y.DES.COUNT>
            Y.TOT.AMT = Y.TOT.AMT + Y.TT.AMT
        END

        Y.DES.COUNT += 1
    REPEAT

RETURN
*--------------------------------------------------------------------------------------------------------
*************
CHECK.AMOUNT:
*************
    IF NOT(Y.TOT.AMT) THEN
        RETURN
    END

    IF Y.TOT.AMT GT Y.TFS.AMT THEN
        TEXT = 'TT.INSUFF.AMOUNT'
        CALL STORE.OVERRIDE(CURR.NO)
    END

RETURN
*--------------------------------------------------------------------------------------------------------
**********************
READ.TELLER.PARAMETER:
**********************
* In this para of the code, file TELLER.PARAMETER is read
    R.TELLER.PARAMETER  = ''
    TELLER.PARAMETER.ER = ''
*Tus start
*  CALL F.READ(FN.TELLER.PARAMETER,TELLER.PARAMETER.ID,R.TELLER.PARAMETER,F.TELLER.PARAMETER,TELLER.PARAMETER.ER)
    CALL CACHE.READ(FN.TELLER.PARAMETER,TELLER.PARAMETER.ID,R.TELLER.PARAMETER,TELLER.PARAMETER.ER)
*Tus End
RETURN
*--------------------------------------------------------------------------------------------------------
*********************
READ.TFS.TRANSACTION:
*********************
* In this para of the code, file TFS.TRANSACTION is read
    R.TFS.TRANSACTION  = ''
    TFS.TRANSACTION.ER = ''
    CALL F.READ(FN.TFS.TRANSACTION,TFS.TRANSACTION.ID,R.TFS.TRANSACTION,F.TFS.TRANSACTION,TFS.TRANSACTION.ER)

RETURN
*--------------------------------------------------------------------------------------------------------
*********************
FIND.MULTI.LOCAL.REF:
*********************
* In this para of the code, local reference field positions are obtained
    APPL.ARRAY = 'USER':@FM:'TELLER.PARAMETER'
    FLD.ARRAY  = 'L.US.CASIER.ROL':@FM:'L.TT.DESGN':@VM:'L.TT.LIMIT.CCY':@VM:'L.TT.LIMIT.AMT'
    FLD.POS    = ''

    CALL MULTI.GET.LOC.REF(APPL.ARRAY,FLD.ARRAY,FLD.POS)

    LOC.L.US.CASIER.ROL.POS = FLD.POS<1,1>
    LOC.L.TT.DESGN.POS      = FLD.POS<2,1>
    LOC.L.TT.LIMIT.CCY.POS  = FLD.POS<2,2>
    LOC.L.TT.LIMIT.AMT.POS  = FLD.POS<2,3>

RETURN
*--------------------------------------------------------------------------------------------------------
END       ;* End of Program
