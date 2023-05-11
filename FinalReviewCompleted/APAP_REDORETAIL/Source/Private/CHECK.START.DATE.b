* @ValidationCode : MjotMTU0OTU0MDcwMzpDcDEyNTI6MTY4MTEzMjc3MTA3MjpJVFNTOi0xOi0xOjYxOjE6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 10 Apr 2023 18:49:31
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 61
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDORETAIL
SUBROUTINE CHECK.START.DATE
* ====================================================================================
*
*    -VALIDATE THAT INS.START.DATE MUST BE EQUAL TO NEXT PAYMENT DATE
*
*
* ====================================================================================
*
* Subroutine Type :
* Attached to     :
* Attached as     :
* Primary Purpose :
*
*
* Incoming:
* ---------
* ASSOCIATED.LOAN.ID
*
* Outgoing:
* ---------
* NA
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : APAP
* Development by  : Santiago Jijon
* Date            : 01 FEB 2012
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*06-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM
*06-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*=======================================================================

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.APAP.H.INSURANCE.DETAILS

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN


*** <region name= Initialise>
***
INITIALISE:

    FN.INS.DET      =  'F.APAP.H.INSURANCE.DETAILS'
    F.INS.DET       =  ''
    R.INS.DET       =  ''

    DUE.DATES = ''    ;* Holds the list of Schedule due dates
    DUE.TYPES = ''    ;* Holds the list of Payment Types for the above dates
    DUE.TYPE.AMTS = ''          ;* Holds the Payment Type amounts
    DUE.PROPS = ''    ;* Holds the Properties due for the above type
    DUE.PROP.AMTS = ''          ;* Holds the Property Amounts for the Properties above
    DUE.OUTS = ''     ;* Oustanding Bal for the date
    DUE.METHODS = ""

    SCHED.ARR = ''

    ARR.ID = R.NEW(INS.DET.ASSOCIATED.LOAN)
    ARR.DATE = R.NEW(INS.DET.INS.START.DATE)
    ARR.END.DATE = R.NEW(INS.DET.INS.END.DATE)
    CYCLE.DATE = TODAY : @FM : ''
    SIM.REF = ''
    CURR.NO = 0

RETURN
*** </region>

*** <region name= open.files>
***
OPEN.FILES:

    CALL OPF(FN.INS.DET,F.INS.DET)


RETURN
*** </region>

*** <region name= Process>
***
PROCESS:

    IF R.NEW(INS.DET.MANAGEMENT.TYPE) EQ 'INCLUIR EN CUOTA' THEN
        CALL AA.SCHEDULE.PROJECTOR(ARR.ID, SIM.REF, "",CYCLE.DATE, TOT.PAYMENT, DUE.DATES, DUE.DEFER.DATES, DUE.TYPES, DUE.METHODS, DUE.TYPE.AMTS, DUE.PROPS, DUE.PROP.AMTS, DUE.OUTS)         ;* Routine to Project complete schedules

        Y.NEXTPAY = FIELD(DUE.DATES,@FM,1)
        Y.DAY.NEXT = RIGHT(Y.NEXTPAY,2)
        Y.DAY.ARR  = RIGHT(ARR.DATE,2)

        IF Y.DAY.NEXT NE Y.DAY.ARR THEN
            GOSUB THROW.OVERRIDE
        END
    END
RETURN
*** </region>

*** <region name= THROW.OVERRIDE>
***
THROW.OVERRIDE:

    TEXT = "APAP.ASSOCIATED.LOAN.DATE"
    CURR.NO = DCOUNT(R.NEW(INS.DET.OVERRIDE),@VM) + 1
    CALL STORE.OVERRIDE(CURR.NO)
RETURN
*** </region>


END
