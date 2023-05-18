* @ValidationCode : MjotODIwMjE1NDg4OkNwMTI1MjoxNjg0Mzg4MDA2MTYwOnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 18 May 2023 11:03:26
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
$PACKAGE APAP.TAM
SUBROUTINE REDO.GET.INTEREST.PROPERTY(ARR.ID,PROP.NAME,OUT.PROP,ERR)
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: H GANESH
* PROGRAM NAME: REDO.GET.INTEREST.PROPERTY
* ODR NO      : ODR-2009-10-0325
*----------------------------------------------------------------------
*DESCRIPTION:  This routine is to get the interest property
* name for the given arrangement ID


*IN PARAMETER:
*       ARR.ID<1> -------> Arrangement ID. * Mandatory
*       ARR.ID<2> -------> Effective Date.
*       PROP.NAME -------> 'PRINCIPAL' or 'PENALTY'.       * Mandatory

*OUT PARAMETER:
*       OUT.PROP-----> Property Name

*LINKED WITH: AA
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*02.11.2010  H GANESH     ODR-2009-10-0325     INITIAL CREATION
*19.05.2011  H GANESH     PACS00055012 - B.16  Since the penalty interest hold period
*                                              start date. Logic has been changed to
*                                              if interest property present in payment
*                                              schedule as principal interest property
* Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*10/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION           FM TO @FM, VM TO @VM, SM TO @SM,++ TO +=
*10/04/2023         SURESH           MANUAL R22 CODE CONVERSION         CALL routine format modified
*----------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.PAYMENT.SCHEDULE
    $INSERT I_F.AA.INTEREST.ACCRUALS
    $USING APAP.AA

    GOSUB INIT
*GOSUB OPENFILES
    GOSUB PROCESS
RETURN
*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------
    ERR              = ''
    OUT.PROP         = ''
    Y.PRIN.INTEREST  = ''
    Y.PENAL.INTEREST = ''

RETURN
*----------------------------------------------------------------------
*OPENFILES:
*----------------------------------------------------------------------
*FN.AA.INTEREST.ACCRUALS='F.AA.INTEREST.ACCRUALS'
*F.AA.INTEREST.ACCRUALS=''
*CALL OPF(FN.AA.INTEREST.ACCRUALS,F.AA.INTEREST.ACCRUALS)

*RETURN
*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------

    IN.AA.ID=ARR.ID<1>
    GOSUB GET.PAYMENT.SCHEDULE

    IN.PROPERTY.CLASS='INTEREST'
*CALL APAP.TAM.REDO.GET.PROPERTY.NAME(IN.AA.ID,IN.PROPERTY.CLASS,R.OUT.AA.RECORD,OUT.PROPERTY,OUT.ERR) ;*MANUAL R22 CODE CONVERSION
    CALL APAP.TAM.redoGetPropertyName(IN.AA.ID,IN.PROPERTY.CLASS,R.OUT.AA.RECORD,OUT.PROPERTY,OUT.ERR) ;*MANUAL R22 CODE CONVERSION
    Y.PROPERTY.CNT=DCOUNT(OUT.PROPERTY,@FM)
*Y.VAR1=1
*LOOP
*WHILE Y.VAR1 LE Y.PROPERTY.CNT
*Y.INT.ACC=IN.AA.ID:'-':OUT.PROPERTY<Y.VAR1>
*R.INT.ACC=''
*CALL F.READ(FN.AA.INTEREST.ACCRUALS,Y.INT.ACC,R.INT.ACC,F.AA.INTEREST.ACCRUALS,INT.ACC.ERR)
*IF R.INT.ACC<AA.INT.ACC.PERIOD.START> NE '' THEN
*Y.PRIN.INTEREST=OUT.PROPERTY<Y.VAR1>
*END ELSE
*Y.PENAL.INTEREST=OUT.PROPERTY<Y.VAR1>
*END
*Y.VAR1++
*REPEAT

    Y.VAR1=1
    LOOP
    WHILE Y.VAR1 LE Y.PROPERTY.CNT

        Y.INT.PROP=OUT.PROPERTY<Y.VAR1>
        LOCATE Y.INT.PROP IN Y.PAYMENT.PROPERTY SETTING POS THEN
            Y.PRIN.INTEREST=OUT.PROPERTY<Y.VAR1>
        END ELSE
            Y.PENAL.INTEREST=OUT.PROPERTY<Y.VAR1>
        END
        Y.VAR1 += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT


    IF PROP.NAME EQ 'PRINCIPAL' THEN
        OUT.PROP=Y.PRIN.INTEREST
        RETURN
    END
    IF PROP.NAME EQ 'PENALTY' THEN
        OUT.PROP=Y.PENAL.INTEREST
    END ELSE
*IF OUT.PROP EQ '' THEN
        ERR='Property Not Found'
    END

RETURN
*------------------------------------------------------------------
GET.PAYMENT.SCHEDULE:
*------------------------------------------------------------------
* Here Payment schedule is read for that particular arrangement


    PROPERTY.CLASS = 'PAYMENT.SCHEDULE'
    PROPERTY = ''
    IF ARR.ID<2> EQ '' THEN
        EFF.DATE = ''
    END ELSE
        EFF.DATE = ''
        EFF.DATE = ARR.ID<2>
    END
    ERR.MSG = ''
    R.PAY.SCH.COND = ''
*CALL APAP.AA.REDO.CRR.GET.CONDITIONS(IN.AA.ID,EFF.DATE,PROPERTY.CLASS,PROPERTY,R.PAY.SCH.COND,ERR.MSG) ;*MANUAL R22 CODE CONVERSION
    CALL APAP.AA.redoCrrGetConditions(IN.AA.ID,EFF.DATE,PROPERTY.CLASS,PROPERTY,R.PAY.SCH.COND,ERR.MSG) ;*MANUAL R22 CODE CONVERSION
    Y.PAYMENT.PROPERTY=R.PAY.SCH.COND<AA.PS.PROPERTY>
    CHANGE @SM TO @FM IN Y.PAYMENT.PROPERTY
    CHANGE @VM TO @FM IN Y.PAYMENT.PROPERTY

RETURN

END
