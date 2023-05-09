* @ValidationCode : MjotMTk3NTUxOTI0NzpDcDEyNTI6MTY4MTM3MzcxMjcyNTphaml0aDotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 13 Apr 2023 13:45:12
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
* <Rating>-45</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.APAP.INP.TFS.LINES.AMT
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.INP.TFS.LINES.AMT
*--------------------------------------------------------------------------------------------------------
*Description       : This is an INPUT routine, the routine validates if the total amount in the local
*                    field is equal to the sum of amounts of all the TFS lines
*Linked With       : Version T24.FUND.SERVICES,REDO.MULTI.TXN
*In  Parameter     : N/A
*Out Parameter     : N/A
*Files  Used       : T24.FUND.SERVICES                   As          I       Mode
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date               Who                  Reference                 Description
*   ------             -----               -------------              -------------
* 19 July 2010     Shiva Prasad Y      ODR-2009-10-0318 B.126        Initial Creation
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*13-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   NO CHANGE
*13-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------
*
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.T24.FUND.SERVICES
*--------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
* This is the para from where the execution of the code starts
    Y.TOT.AMT = 0

    GOSUB PROCESS.PARA

RETURN
*--------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************
* This is the main processing para
    GOSUB FIND.MULTI.LOCAL.REF
    GOSUB CHECK.AMOUNT

RETURN
*--------------------------------------------------------------------------------------------------------
*************
CHECK.AMOUNT:
*************
    IF NOT(R.NEW(TFS.LOCAL.REF)<1,LOC.L.TFS.TXN.AMT.POS>) THEN
        RETURN
    END

    Y.TXN.AMTS = R.NEW(TFS.AMOUNT)

    LOOP
        REMOVE Y.AMT FROM Y.TXN.AMTS SETTING Y.AMT.POS
    WHILE Y.AMT:Y.AMT.POS
        Y.TOT.AMT = Y.TOT.AMT + Y.AMT
    REPEAT

    IF Y.TOT.AMT NE R.NEW(TFS.LOCAL.REF)<1,LOC.L.TFS.TXN.AMT.POS> THEN
        AF = TFS.AMOUNT
        ETEXT = 'EB-AMT.NE.TFS.LINES'
        CALL STORE.END.ERROR
    END

RETURN
*--------------------------------------------------------------------------------------------------------
********************
FIND.MULTI.LOCAL.REF:
********************
* In this para of the code, local reference field positions are obtained
    APPL.ARRAY = 'T24.FUND.SERVICES'
    FLD.ARRAY  = 'L.TFS.TXN.AMT'
    FLD.POS    = ''
    CALL MULTI.GET.LOC.REF(APPL.ARRAY,FLD.ARRAY,FLD.POS)
    LOC.L.TFS.TXN.AMT.POS  =  FLD.POS<1,1>

RETURN
*--------------------------------------------------------------------------------------------------------
END       ;* End of PRogram
