* @ValidationCode : MjoxMDM1OTc5OTI1OkNwMTI1MjoxNjgyNjY5OTI5NjA5OmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 28 Apr 2023 13:48:49
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
* <Rating>-77</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.APAP.VAL.COMP.ID
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.VAL.COMP.ID
*--------------------------------------------------------------------------------------------------------
*Description       : This is a VALIDATION routine, attached to the local reference field L.TT.COMP.ID,
*                    the routine populates the fields company name, bill condition and bill type by
*                    reading the values from the file REDO.THIRDPRTY.PARAMETER
*Linked With       : Version T24.FUND.SERVICES,MULTI.TXN
*In  Parameter     : N/A
*Out Parameter     : N/A
*Files  Used       : T24.FUND.SERVICES                   As          I       Mode
*                    REDO.THIRDPRTY.PARAMETER            As          I       Mode
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date               Who                  Reference                 Description
*   ------             -----               -------------              -------------
* 13 July 2010     Shiva Prasad Y      ODR-2009-10-0318 B.126        Initial Creation
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*19-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  NO CHANGE
*19-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION VMto@VM
*----------------------------------------------------------------------------------------
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.REDO.THIRDPRTY.PARAMETER
*--------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
* This is the para from where the execution of the code starts
    IF NOT(COMI) THEN
        RETURN
    END
    IF MESSAGE EQ 'VAL' THEN
        RETURN
    END
    GOSUB OPEN.PARA
    GOSUB PROCESS.PARA

RETURN
*--------------------------------------------------------------------------------------------------------
**********
OPEN.PARA:
**********
* In this para of the code, file variables are initialised and opened

    FN.REDO.THIRDPRTY.PARAMETER = 'F.REDO.THIRDPRTY.PARAMETER'
    F.REDO.THIRDPRTY.PARAMETER = ''
    CALL OPF(FN.REDO.THIRDPRTY.PARAMETER,F.REDO.THIRDPRTY.PARAMETER)

RETURN
*--------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************
* This is the main processing para
    GOSUB UPDATE.FIELDS

RETURN
*--------------------------------------------------------------------------------------------------------
**************
UPDATE.FIELDS:
**************
* In this para of the code, the file REDO.THIRDPRTY.PARAMETER is read and values are updated on to the fields on screen
    REDO.THIRDPRTY.PARAMETER.ID = COMI
    GOSUB READ.REDO.THIRDPRTY.PARAMETER

    IF NOT(R.REDO.THIRDPRTY.PARAMETER) THEN
        RETURN
    END

    GOSUB FIND.MULTI.LOCAL.REF
    R.NEW(TFS.LOCAL.REF)<1,LOC.L.TT.CMPNY.NAME.POS> = R.REDO.THIRDPRTY.PARAMETER<REDO.TP.COMP.NAME>
    R.NEW(TFS.LOCAL.REF)<1,LOC.L.TT.BILL.COND.POS>  = R.REDO.THIRDPRTY.PARAMETER<REDO.TP.BILL.COND>
    R.NEW(TFS.LOCAL.REF)<1,LOC.L.TT.BILL.TYPE.POS>  = R.REDO.THIRDPRTY.PARAMETER<REDO.TP.BILL.TYPE>

RETURN
*--------------------------------------------------------------------------------------------------------
******************************
READ.REDO.THIRDPRTY.PARAMETER:
******************************
* In this para of the code, file REDO.THIRDPRTY.PARAMETER is read
    R.REDO.THIRDPRTY.PARAMETER  = ''
    REDO.THIRDPRTY.PARAMETER.ER = ''
    CALL F.READ(FN.REDO.THIRDPRTY.PARAMETER,REDO.THIRDPRTY.PARAMETER.ID,R.REDO.THIRDPRTY.PARAMETER,F.REDO.THIRDPRTY.PARAMETER,REDO.THIRDPRTY.PARAMETER.ER)

RETURN
*--------------------------------------------------------------------------------------------------------
*********************
FIND.MULTI.LOCAL.REF:
*********************
* In this para of the code, local reference field positions are obtained

    APPL.ARRAY = 'T24.FUND.SERVICES'
    FLD.ARRAY  = 'L.TT.CMPNY.NAME':@VM:'L.TT.BILL.COND':@VM:'L.TT.BILL.TYPE';*R22 MANUAL CODE CONVERSION
    FLD.POS    = ''
    CALL MULTI.GET.LOC.REF(APPL.ARRAY,FLD.ARRAY,FLD.POS)

    LOC.L.TT.CMPNY.NAME.POS = FLD.POS<1,1>
    LOC.L.TT.BILL.COND.POS  = FLD.POS<1,2>
    LOC.L.TT.BILL.TYPE.POS  = FLD.POS<1,3>

RETURN
*--------------------------------------------------------------------------------------------------------
END       ;* End of program
