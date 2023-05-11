$PACKAGE APAP.TAM
SUBROUTINE REDO.TELLER.PROCESS.AUTORTN
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: S SUDHARSANAN
* PROGRAM NAME: REDO.TELLER.PROCESS.AUTORTN
* ODR NO      : ODR-2009-10-0322
*----------------------------------------------------------------------
*DESCRIPTION: This is the  Routine for REDO.TELLER.REJECT to
* default the value for the REDO.TELLER.PROCESS application from REDO.TELLER.REJECT
* It is AUTOM NEW CONTENT routine

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH: REDO.TELLER.REJECT
*----------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*15.05.2010  S SUDHARSANAN   ODR-2009-10-0322   INITIAL CREATION
** 17-04-2023 R22 Auto Conversion no changes
** 17-04-2023 Skanda R22 Manual Conversion - No changes
*----------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.TELLER.PROCESS
    $INSERT I_F.REDO.TELLER.REJECT

    GOSUB INIT
    GOSUB PROCESS
RETURN

*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------


    FN.REDO.TELLER.REJECT = 'F.REDO.TELLER.REJECT'
    F.REDO.TELLER.REJECT = ''
    CALL OPF(FN.REDO.TELLER.REJECT,F.REDO.TELLER.REJECT)

RETURN

*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------

    Y.DATA = ""
    CALL BUILD.USER.VARIABLES(Y.DATA)
    Y.REDO.TELLER.REJECT.ID=FIELD(Y.DATA,"*",2)
    CALL F.READ(FN.REDO.TELLER.REJECT,Y.REDO.TELLER.REJECT.ID,R.REDO.TELLER.REJECT,F.REDO.TELLER.REJECT,REJ.ERR)

    Y.CONCEPT =R.REDO.TELLER.REJECT<TEL.REJ.CONCEPT>
    Y.CURRENCY =R.REDO.TELLER.REJECT<TEL.REJ.CURRENCY>
    Y.CLIENT.ID =R.REDO.TELLER.REJECT<TEL.REJ.CLIENT.ID>
    Y.CLIENT.NAME =R.REDO.TELLER.REJECT<TEL.REJ.CLIENT.NAME>
    Y.AMOUNT =R.REDO.TELLER.REJECT<TEL.REJ.AMOUNT>
    Y.GROUP =R.REDO.TELLER.REJECT<TEL.REJ.GROUP>
    Y.SUB.GROUP =R.REDO.TELLER.REJECT<TEL.REJ.SUB.GROUP>
    Y.CATEGORY=R.REDO.TELLER.REJECT<TEL.REJ.CATEGORY>
    Y.PAYMENT.TYPE=R.REDO.TELLER.REJECT<TEL.REJ.PAYMENT.TYPE>

    R.NEW(TEL.PRO.AMOUNT)=Y.AMOUNT
    R.NEW(TEL.PRO.CURRENCY)=Y.CURRENCY
    R.NEW(TEL.PRO.CLIENT.ID)=Y.CLIENT.ID
    R.NEW(TEL.PRO.CLIENT.NAME)=Y.CLIENT.NAME
    R.NEW(TEL.PRO.PAYMENT.TYPE)=Y.PAYMENT.TYPE
    R.NEW(TEL.PRO.CATEGORY)=Y.CATEGORY
    R.NEW(TEL.PRO.GROUP)=Y.GROUP
    R.NEW(TEL.PRO.CONCEPT)=Y.CONCEPT
    R.NEW(TEL.PRO.SUB.GROUP)=Y.SUB.GROUP

RETURN

END
