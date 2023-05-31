* @ValidationCode : MjotMTgwMzc0NDg2NjpDcDEyNTI6MTY4NDgzNjA0MjY1MjpJVFNTOi0xOi0xOjY5NzoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 23 May 2023 15:30:42
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 697
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.INSURANCE.GENERATE

*-----------------------------------------------------------------------------------------------------------------
* Description : This routine will be used to perform various checks on the
* fields in the version APAP.H.INSURANCE.DETAILS,INP
*-----------------------------------------------------------------------------------------------------------------
* Modification History
* Date                  who                   Reference              
* 05-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION FM TO @FM 
* 05-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*
*-----------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.APAP.H.INSURANCE.DETAILS
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.REDO.H.POLICY.NUMBER
    $INSERT I_F.REDO.APAP.H.COMP.NAME
    $INSERT I_F.COLLATERAL

*-----------------------------------------------------------------------------------------------------------------
MAIN.LOGIC:


    GOSUB INITIALISE

    GOSUB APP.VALIDATIONS

RETURN
*-----------------------------------------------------------------------------------------------------------------
INITIALISE:

    FN.REDO.APAP.H.COMP.NAME = 'F.REDO.APAP.H.COMP.NAME'
    F.REDO.APAP.H.COMP.NAME = ''
    R.REDO.APAP.H.COMP.NAME = ''
    CALL OPF(FN.REDO.APAP.H.COMP.NAME,F.REDO.APAP.H.COMP.NAME)

    FN.AA.ACCOUNT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS = ''
    R.AA.ACCOUNT.DETAILS = ''
    CALL OPF (FN.AA.ACCOUNT.DETAILS, F.AA.ACCOUNT.DETAILS)

    FN.REDO.H.POLICY.NUMBER = 'F.REDO.H.POLICY.NUMBER'
    F.REDO.H.POLICY.NUMBER  = ''
    R.REDO.H.POLICY.NUMBER = ''
    CALL OPF(FN.REDO.H.POLICY.NUMBER,F.REDO.H.POLICY.NUMBER)
    Y.REDO.POL.NUM.ERR = ''

    FN.COLLATERAL = 'F.COLLATERAL'
    F.COLLATERAL = ''
    R.COLLATERAL = ''
    CALL OPF(FN.COLLATERAL, F.COLLATERAL)

    ARR.ID = R.NEW(INS.DET.ASSOCIATED.LOAN)

    CALL F.READ(FN.AA.ACCOUNT.DETAILS, ARR.ID, R.AA.ACCOUNT.DETAILS, F.AA.ACCOUNT.DETAILS, READ.ERR)

RETURN
*-----------------------------------------------------------------------------------------------------------------
APP.VALIDATIONS:

    GOSUB POPULATE.DATES

    GOSUB CLASS.POLICY.VALIDATIONS

    GOSUB INS.POLICY.VALIDATIONS

    GOSUB INS.COMPANY.VALIDATIONS

    GOSUB INS.AMT.DATE.VALIDATIONS

    GOSUB OTHER.VALIDATIONS

RETURN
*-----------------------------------------------------------------------------------------------------------------
POPULATE.DATES:

* Populate the Policy Origin date, Policy Start date  if Null by today's date
* and Policy Expiry date to Loan Maturity Date

    ARR.ID = R.NEW(INS.DET.ASSOCIATED.LOAN)
    EFF.DATE = TODAY

    IF NOT(R.NEW(INS.DET.POLICY.ORG.DATE)) THEN

        R.NEW(INS.DET.POLICY.ORG.DATE) = TODAY

    END

    IF NOT(R.NEW(INS.DET.POL.START.DATE)) THEN

        R.NEW(INS.DET.POL.START.DATE) = TODAY

    END

    IF NOT(R.NEW(INS.DET.POL.EXP.DATE)) THEN

        IF R.NEW(INS.DET.INS.POLICY.TYPE) EQ 'FHA' THEN
            R.NEW(INS.DET.POL.EXP.DATE) = R.NEW(INS.DET.POL.START.DATE)     ;* Policy Expiry date should be TODAY for FHA policies
        END ELSE
            R.NEW(INS.DET.POL.EXP.DATE) = R.AA.ACCOUNT.DETAILS<AA.AD.MATURITY.DATE>
        END

    END


    PROPERTY.CLASS = 'TERM.AMOUNT'
    GOSUB GET.PROPERTY

    CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, PROPERTY.CLASS, AA.PROPERTY, EFF.DATE, RET.IDS, RET.CONDITIONS, RET.ERR)

    R.TERM.AMOUNT = RAISE(RET.CONDITIONS)

    IF NOT(R.NEW(INS.DET.INS.AMOUNT)) THEN

        R.NEW(INS.DET.INS.AMOUNT) = R.TERM.AMOUNT<AA.AMT.AMOUNT>

    END

*Validation for Insurance Starte Date & Insurance End date

    IF (R.NEW(INS.DET.CLASS.POLICY) EQ 'GROUP' AND R.NEW(INS.DET.MANAGEMENT.TYPE) EQ 'Incluir en Cuota') THEN

        IF NOT (R.NEW(INS.DET.INS.START.DATE)) THEN

            AF = INS.DET.INS.START.DATE
            ETEXT = "EB-REDO.CHARGE.START.DATE"
            CALL STORE.END.ERROR

        END

        IF NOT (R.NEW(INS.DET.INS.END.DATE)) THEN

            AF = INS.DET.INS.END.DATE
            ETEXT = "EB-REDO.CHARGE.END.DATE"
            CALL STORE.END.ERROR

        END
    END

    IF (R.NEW(INS.DET.INS.END.DATE) GT R.TERM.AMOUNT<AA.AMT.MATURITY.DATE>) THEN

        AF = INS.DET.INS.END.DATE
        ETEXT = "EB-GREATER.THAN.MATURITY.DATE"
        CALL STORE.END.ERROR
    END

    IF (R.NEW(INS.DET.POL.EXP.DATE) GT R.TERM.AMOUNT<AA.AMT.MATURITY.DATE>) THEN

        AF = INS.DET.POL.EXP.DATE
        ETEXT = "EB-GREATER.THAN.MATURITY.DATE"
        CALL STORE.END.ERROR

    END

    IF (R.NEW(INS.DET.POL.EXP.DATE) LE R.NEW(INS.DET.POLICY.ORG.DATE)) AND (R.NEW(INS.DET.INS.POLICY.TYPE) NE 'FHA' )THEN

        AF = INS.DET.POL.EXP.DATE
        ETEXT = "EB-LESS.THAN.POL.START.DATE"
        CALL STORE.END.ERROR

    END


*Populate Collateral ID

    LOC.REF.APPL="AA.PRD.DES.TERM.AMOUNT"
    LOC.REF.FIELDS="L.AA.COL"
    LOC.REF.POS=" "
    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,LOC.REF.POS)

    COLLATERAL.POS = LOC.REF.POS<1,1>

    IF NOT(R.NEW(INS.DET.COLLATERAL.ID)) THEN

        COLL.IDS  = RAISE(RAISE(R.TERM.AMOUNT<AA.AMT.LOCAL.REF,COLLATERAL.POS>))

        COLL.COUNT = DCOUNT(COLL.IDS,@FM)

        IF COLL.COUNT GT 1 THEN

            FOR COLL.CNT = 1 TO COLL.COUNT

                R.NEW(INS.DET.COLLATERAL.ID)<-1> = COLL.IDS<COLL.CNT>

            NEXT COLL.CNT

            R.NEW(INS.DET.COLLATERAL.ID) = LOWER(R.NEW(INS.DET.COLLATERAL.ID))

        END ELSE

            R.NEW(INS.DET.COLLATERAL.ID) = R.TERM.AMOUNT<AA.AMT.LOCAL.REF,COLLATERAL.POS,1>

        END
    END

    GOSUB POPULATE.MANAGEMENT.TYPE

RETURN

*-----------------------------------------------------------------------------------------------------------------

POPULATE.MANAGEMENT.TYPE:

*Populate Management Type for Insurance

    IF NOT(R.NEW(INS.DET.MANAGEMENT.TYPE)) THEN

        IF R.NEW(INS.DET.CLASS.POLICY) EQ 'ED' OR R.NEW(INS.DET.CLASS.POLICY) EQ 'FHA' THEN

            R.NEW(INS.DET.MANAGEMENT.TYPE) = 'No Incluir en Cuota'

        END

        IF R.NEW(INS.DET.INS.POLICY.TYPE) EQ 'VU' AND R.NEW(INS.DET.CLASS.POLICY) EQ 'GROUP' THEN         ;*Ref: PACS00039517

            R.NEW(INS.DET.MANAGEMENT.TYPE) = 'No Incluir en Cuota'

        END

        IF R.NEW(INS.DET.INS.POLICY.TYPE) EQ 'VI' AND R.NEW(INS.DET.CLASS.POLICY) EQ 'GROUP' THEN

            R.NEW(INS.DET.MANAGEMENT.TYPE) = 'Incluir en Cuota'

        END

        IF R.NEW(INS.DET.INS.POLICY.TYPE) EQ 'VE' AND R.NEW(INS.DET.CLASS.POLICY) EQ 'GROUP' THEN         ;*Ref: PACS00038954

            COLLATERAL.ID = R.NEW(INS.DET.COLLATERAL.ID)

            CALL F.READ(FN.COLLATERAL, COLLATERAL.ID, R.COLLATERAL, F.COLLATERAL, COLL.ERR)

            R.NEW(INS.DET.INS.AMOUNT) = R.COLLATERAL<COLL.NOMINAL.VALUE>

            R.NEW(INS.DET.MANAGEMENT.TYPE) = 'Incluir en Cuota'

        END

        IF NOT(R.NEW(INS.DET.MANAGEMENT.TYPE)) THEN     ;* If management Type is not specified then default it to 'Included on Fee'

            R.NEW(INS.DET.MANAGEMENT.TYPE) = 'Incluir en Cuota'

        END

    END


RETURN
*-----------------------------------------------------------------------------------------------------------------

CLASS.POLICY.VALIDATIONS:

    TEST.VAR = R.NEW(INS.DET.CLASS.POLICY)
    IF R.NEW(INS.DET.CLASS.POLICY) EQ 'ED' THEN
        IF R.NEW(INS.DET.MANAGEMENT.TYPE) EQ 'Incluir en Cuota' THEN        ;*When CLASS.POLICY is 'ENDORSED' then MGMT.TYPE should not be 'Included on Fee'
            AF = INS.DET.MANAGEMENT.TYPE
            ETEXT="EB-REDO.NOT.INCLUDED.FEE"
            CALL STORE.END.ERROR
        END
    END

* When CLASS.POLICY is 'FHA' then MGMT.TYPE should not be 'Included on Fee'

    IF R.NEW(INS.DET.CLASS.POLICY) EQ 'FHA' THEN
        IF R.NEW(INS.DET.MANAGEMENT.TYPE) EQ 'Incluir en Cuota' THEN
            AF = INS.DET.MANAGEMENT.TYPE
            ETEXT = "EB-REDO.NOT.INCLUDED.FEE.FHA"
            CALL STORE.END.ERROR
        END
    END

*When CLASS.POLICY is 'GROUP' then MGMT.TYPE should not be 'Not Included on Fee'

    IF R.NEW(INS.DET.CLASS.POLICY) EQ "GROUP" AND R.NEW(INS.DET.INS.POLICY.TYPE) NE 'VU' THEN
        IF R.NEW(INS.DET.MANAGEMENT.TYPE) EQ 'No Incluir en Cuota' THEN
            AF = INS.DET.MANAGEMENT.TYPE
            ETEXT = "EB-REDO.INCLUDED.FEE"
            CALL STORE.END.ERROR
        END
    END

RETURN
*-----------------------------------------------------------------------------------------------------------------

INS.POLICY.VALIDATIONS:

*When CLASS.POLICY is GROUP and value in INS.POLICY.TYPE field is not equal to SINGLE LIFE INSURANCE,
*and POL.START.DATE or POL.EXP.DATE is NULL throw an error

    IF R.NEW(INS.DET.CLASS.POLICY) EQ "GROUP" AND R.NEW(INS.DET.INS.POLICY.TYPE) NE 'VU' THEN
        IF R.NEW(INS.DET.POL.START.DATE) EQ "" THEN
            AF=INS.DET.POL.START.DATE
            ETEXT="EB-REDO.CHARGE.START.DATE"
            CALL STORE.END.ERROR
        END

        IF R.NEW(INS.DET.POL.EXP.DATE) EQ "" THEN
            AF=INS.DET.POL.EXP.DATE
            ETEXT="EB-REDO.CHARGE.START.DATE"
            CALL STORE.END.ERROR
        END
    END

*When INS.POLICY.TYPE is SINGLE LIFE INSURANCE then MGMT.TYPE should be 'Included on Bill'
    IF R.NEW(INS.DET.INS.POLICY.TYPE) EQ ' VU' THEN
        IF R.NEW(INS.DET.MANAGEMENT.TYPE) EQ 'Incluir en Cuota' THEN
            AF= INS.DET.MANAGEMENT.TYPE
            ETEXT = "EB-REDO.FEE.TYPE.LIFE.POLICIES"
            CALL STORE.END.ERROR
        END
    END

    IF R.NEW(INS.DET.INS.POLICY.TYPE) EQ 'VE' AND R.NEW(INS.DET.CLASS.POLICY) EQ 'GROUP' THEN

        IF NOT (R.NEW(INS.DET.MON.POL.AMT)) THEN
            AF = INS.DET.MON.POL.AMT
            ETEXT = "EB-INPUT.MANDATORY.FOR.VE.INS"
            CALL STORE.END.ERROR
        END

        IF NOT(R.NEW(INS.DET.MON.POL.AMT.DATE)) THEN
            AF = INS.DET.MON.POL.AMT.DATE
            ETEXT = "EB-INPUT.MANDATORY.FOR.VE.INS"
            CALL STORE.END.ERROR
        END

    END

RETURN
*-----------------------------------------------------------------------------------------------------------------

INS.COMPANY.VALIDATIONS:

*If INS.POLICY.TYPE is FHA and if COMPANY.NAME is not BNV, then throw an error

    IF R.NEW(INS.DET.INS.POLICY.TYPE) EQ 'FHA' THEN

        COMPANY.ID = R.NEW(INS.DET.INS.COMPANY)

        CALL F.READ(FN.REDO.APAP.H.COMP.NAME,COMPANY.ID,R.REDO.APAP.H.COMP.NAME,F.REDO.APAP.H.COMP.NAME,Y.APAP.H.COMP.ERR)

        COMPANY.NAME = R.REDO.APAP.H.COMP.NAME<REDO.CMP.INS.COMP.NAME>

        IF COMPANY.NAME NE 'BNV' THEN
            AF = INS.DET.INS.COMPANY
            ETEXT = "EB-REDO.BNV.INSURANCE.POLICY"
            CALL STORE.END.ERROR
        END
    END

RETURN
*-----------------------------------------------------------------------------------------------------------------

INS.AMT.DATE.VALIDATIONS:

* POL.AMT.DATE is entered only for Vehicle Loans

    IF R.NEW(INS.DET.INS.POLICY.TYPE) NE 'VE' THEN
        IF R.NEW(INS.DET.MON.POL.AMT.DATE) NE '' THEN
            AF= INS.DET.MON.POL.AMT.DATE
            ETEXT ='EB-REDO.MONTHLY.POLICY.DATE'
            CALL STORE.END.ERROR
        END
    END

RETURN
*-----------------------------------------------------------------------------------------------------------------

OTHER.VALIDATIONS:

* EXTRA.AMT field cannot be input when value in INS.POLICY.TYPE field is FHA or Single Life Insurance Policy & and
* when CLASS.POLICY field has value ENDORSED , if then throw an error

* ARR.ID = R.NEW(INS.DET.ASSOCIATED.LOAN)

* CALL F.READ(FN.AA.ACCOUNT.DETAILS, ARR.ID, R.AA.ACCOUNT.DETAILS, F.AA.ACCOUNT.DETAILS, READ.ERR)

    IF R.NEW(INS.DET.EXTRA.AMT) THEN
        IF R.NEW(INS.DET.INS.POLICY.TYPE) EQ 'FHA' OR R.NEW(INS.DET.INS.POLICY.TYPE)EQ 'VU' THEN
            IF R.NEW(INS.DET.CLASS.POLICY) EQ 'ED' THEN
                AF= INS.DET.MON.POL.AMT
                ETEXT = "EB-REDO.NO.POLICY.AMT"
                CALL STORE.END.ERROR
            END
        END
    END


*MON.TOT.PRE.AMT is calculated as sum of MON.POL.AMT + EXTRA.AMT

*    R.NEW(INS.DET.MON.TOT.PRE.AMT) = R.NEW(INS.DET.MON.POL.AMT) + R.NEW(INS.DET.EXTRA.AMT)

    GOSUB CALCULATE.MONTHLY.TOTAL

*POLICY.EXP.DATE should be defaulted with Loan Maturity date when CLASS.POLICY is GROUP or FHA

*    IF R.NEW(INS.DET.CLASS.POLICY) EQ 'GROUP' OR R.NEW(INS.DET.CLASS.POLICY) EQ 'FHA' THEN
*        R.NEW(INS.DET.POL.EXP.DATE) = R.AA.ACCOUNT.DETAILS<AA.AD.MATURITY.DATE>
*    END

* Populate the SEN.POLICY.NUMBER and  POLICY.NUMBER from the Insurance Policy Table

*    IF R.NEW(INS.DET.CLASS.POLICY) EQ 'GROUP' THEN

    IF NOT(R.NEW(INS.DET.SEN.POLICY.NUMBER)) THEN
        INS.POLICY.TYPE = R.NEW(INS.DET.INS.POLICY.TYPE)
        CALL F.READ(FN.REDO.H.POLICY.NUMBER,INS.POLICY.TYPE,R.REDO.H.POLICY.NUMBER,F.REDO.H.POLICY.NUMBER,Y.REDO.POL.NUM)

        SEN.POLICY.NUMBER = R.REDO.H.POLICY.NUMBER<REDO.ARR.POL.POLICY.NUMBER>
        R.NEW(INS.DET.SEN.POLICY.NUMBER) = SEN.POLICY.NUMBER
    END


    IF NOT(R.NEW(INS.DET.POLICY.NUMBER)) THEN
        POLICY.NUMBER = SEN.POLICY.NUMBER + 1
        R.NEW(INS.DET.POLICY.NUMBER) = POLICY.NUMBER
    END

*    END

RETURN

*-----------------------------------------------------------------------------------------------------------------

CALCULATE.MONTHLY.TOTAL:

    MONTHLY.AMT = R.NEW(INS.DET.MON.POL.AMT)
    MONTHLY.AMT = RAISE(MONTHLY.AMT)
    TOTAL.AMT = ''
    MONTH.COUNT = DCOUNT(MONTHLY.AMT,@FM)

    FOR CNT = 1 TO MONTH.COUNT

        TOTAL.AMT = TOTAL.AMT + MONTHLY.AMT<CNT>

    NEXT CNT

    R.NEW(INS.DET.MON.TOT.PRE.AMT) = TOTAL.AMT + R.NEW(INS.DET.EXTRA.AMT)



RETURN
*-----------------------------------------------------------------------------------------------------------------

GET.PROPERTY:

* Get the property Name for the property class

    ARR.INFO = ''
    R.ARRANGEMENT = ''
    AA.PROPERTY = ''

    ARR.INFO<1> = R.NEW(INS.DET.ASSOCIATED.LOAN)
    ARR.DATE = R.AA.ACCOUNT.DETAILS<AA.AD.BASE.DATE>

    CALL AA.GET.ARRANGEMENT.PROPERTIES(ARR.INFO, ARR.DATE, R.ARRANGEMENT, PROP.LIST)

    CLASS.LIST = ''
    CALL AA.GET.PROPERTY.CLASS (PROP.LIST, CLASS.LIST)

    CLASS.LIST = RAISE(CLASS.LIST)
    PROP.LIST = RAISE(PROP.LIST)

    CLASS.CTR = ''
    LOOP
        REMOVE Y.CLASS FROM CLASS.LIST SETTING CLASS.POS
        CLASS.CTR +=1
    WHILE Y.CLASS:CLASS.POS
        IF Y.CLASS EQ PROPERTY.CLASS THEN
            AA.PROPERTY = PROP.LIST<CLASS.CTR>
            BREAK
        END
    REPEAT

RETURN
*-----------------------------------------------------------------------------------------------------------------

END
