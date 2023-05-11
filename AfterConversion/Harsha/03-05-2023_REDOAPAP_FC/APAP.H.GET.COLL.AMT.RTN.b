* @ValidationCode : MjotMTMxMzExNDE2NjpDcDEyNTI6MTY4MjUwMjYxMTc5MTpJVFNTOi0xOi0xOjEwNjM6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 26 Apr 2023 15:20:11
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 1063
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE APAP.H.GET.COLL.AMT.RTN(BEHAVIOUR , AMT, ALLOW.MULTI, FIELD.NAME)
* ====================================================================================
*
*    - DESCRIPTION
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
* NA
*
*
* Outgoing:
* ---------
* NA
*
*-----------------------------------------------------------------------------------
* Modification History:
* Date                   who                   Reference              
* 04-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION VM TO @VM AND FM TO @FM AND SM TO @SM AND I TO I.VAR AND J TO J.VAR AND F.READ TO CACHE.READ AND F.CURR TO R.CURR AND AMT+ AMT.CURR TO AMT+
* 04-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*
* Development for :
* Development by  : Jorge Valarezo
* Date            :
* Amended by      : Santiago Jijon
* Date            : 2001-10-28
*=======================================================================

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COLLATERAL
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.CURRENCY
    $INSERT I_BATCH.FILES
    $INSERT I_F.APAP.H.INSURANCE.DETAILS
    $INSERT I_F.REDO.FC.PROD.COLL.POLICY

*************************************************************************

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB PROCESS

*
RETURN
***<region name=PROCESS>
* ======
PROCESS:
* ======

    COLL.FIELD.NAME = 'ASSOCIATED.LOAN'
    CALL EB.GET.APPL.FIELD(APPLICATION,COLL.FIELD.NAME,'',Y.APP.ERR)

    GOSUB TYPE.AA

    BEGIN CASE
        CASE BEHAVIOUR EQ 'MONTO DEL PRESTAMO'
*EXTRACT AMOUNT FROM ASSOCIATED LOAN
            IF R.NEW(INS.DET.INS.AMOUNT)<1,1> EQ '' THEN
                GOSUB ASSOCIATED.LOAN
            END


        CASE BEHAVIOUR EQ 'VALOR DE LA CONSTRUCCION'
*EXTRACT TOTAL AMOUNT OF BUILDING FROM COLLATERAL
            IF R.NEW(INS.DET.INS.AMOUNT)<1,1> EQ '' THEN
                Y.CAMPO = Y.L.COL.TOT.BD.AR
                GOSUB GET.VALUE.AMOUNT
            END

        CASE BEHAVIOUR EQ 'VALOR NOMINAL GARANTIA'
*EXTRACT ALL NOMINAL AMOUNTS FROM COLLATERAL ID
            IF R.NEW(INS.DET.INS.AMOUNT)<1,1> EQ '' THEN
                GOSUB AMT.NOMINAL
            END


        CASE BEHAVIOUR EQ 'INGRESO MANUAL'
*ESTE CASO DEBE SER CONSIDETADO EN EL COMPORTAMIENTO REGULAR DEBIDO A QUE SI INGRESA ESTE VALOR R.NEW DEBE SER IGUAL A ''
*ALLOW TO INPUT VALUE
            AMT=''

        CASE BEHAVIOUR EQ 'FECHA VENCIMIENTO PRESTAMO'

            IF R.NEW(INS.DET.POL.EXP.DATE) EQ '' THEN
                GOSUB EXTRACT.EXP.DATE
*R.NEW(INS.DET.POL.EXP.DATE) = MAX.EXP.DATE
            END

            IF R.NEW(INS.DET.INS.END.DATE) EQ '' THEN
                GOSUB EXTRACT.EXP.DATE
*R.NEW(INS.DET.INS.END.DATE) = MAX.EXP.DATE
            END


        CASE BEHAVIOUR EQ 'CAMPO MANDATORIO'
            AMT = R.NEW(INS.DET.INS.AMOUNT)

    END CASE

RETURN
***</region>
***<region name=OPEN.FILES>
* =========
OPEN.FILES:
* =========
*

    CALL OPF(FN.COLLATERAL,F.COLLATERAL)
    CALL OPF(FN.AA.ACCOUNT.DETAILS, F.AA.ACCOUNT.DETAILS)
    CALL OPF(FN.AA.TERM.AMT, F.AA.TERM.AMT)
    CALL OPF(FN.AA.ARR, F.AA.ARR)
    CALL OPF(FN.CURR,F.CURR)
    CALL OPF(FN.COLL.POL,F.COLL.POL)

RETURN
***</region>
***<region name=INITIALISE>
* =========
INITIALISE:
* =========
*
    LOOP.CNT             =  0
    MAX.LOOPS            =  0
    PROCESS.GOAHEAD      =  1

    FN.COLLATERAL        =  'F.COLLATERAL'
    F.COLLATERAL         =  ''
    R.COLLATERAL         =  ''
    ID.COLLATERAL        =  ''
    Y.APP.ERR            =  ''

    CO.CODE              =  ''

    FN.AA.ACCOUNT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS = ''
    R.AA.ACCOUNT.DETAILS = ''

    FN.COLL.POL          = 'F.REDO.FC.PROD.COLL.POLICY'
    F.COLL.POL           = ''
    R.COLL.POL           = ''

    FN.AA.TERM.AMT       =  'F.AA.ARR.TERM.AMOUNT'
    F.AA.TERM.AMT        =  ''
    R.AA.TERM.AMT        =  ''
    ID.AA.TERM.AMT       =  ''

    FN.AA.ARR            =  'F.AA.ARRANGEMENT'
    F.AA.ARR             =  ''
    R.AA.ARR             =  ''

    FN.CURR              =  'F.CURRENCY'
    F.CURR               =  ''
    R.CURR               =  ''
    Y.ID.CURR            =  ''

    APP.NANE             =  ''
    ASSOCIATED.AMT       =  ''

    SEL.CMD              =  ''
    SEL.LIST             =  ''
    SEL.LIST.DATES       =  ''
    NO.OF.SEL.DATES      =  0
    NO.OF.REC            =  ''
    ITR                  =  0
    AMT.CURR             =  0
    AMT                  =  0

    LOC.REF.FIELDS = "L.COL.TOT.VALUA": @VM :"L.COL.TOT.BD.AR"
    CALL MULTI.GET.LOC.REF("COLLATERAL",LOC.REF.FIELDS,LOC.REF.POS)
    Y.L.COL.TOT.VALUA.POS = LOC.REF.POS<1,1>
    Y.L.COL.TOT.BD.AR     = LOC.REF.POS<1,2>


RETURN
***</region>
***<region name=TYPE.AA>
*===============
TYPE.AA:  ;* Verifica si el tipo de poliza es permitido para el typo a AA o de Collateral
*===============

    Y.SW = 1
    Y.AA.ID = R.NEW(INS.DET.ASSOCIATED.LOAN)<1,1>
    CALL F.READ(FN.AA.ARR, Y.AA.ID, R.AA.ARR, F.AA.ARR, Y.APP.ERR)
    Y.PRODUCTO = R.AA.ARR<AA.ARR.PRODUCT>

    CALL F.READ(FN.COLL.POL, Y.PRODUCTO, R.COLL.POL, F.COLL.POL, Y.COLL.ERR)
    Y.POL.TYPE = R.NEW(INS.DET.INS.POLICY.TYPE)

    Y.TOT.TIP.POL = DCOUNT(R.COLL.POL<REDO.CPL.TIPO.POLIZA>,@VM)
    FOR I.VAR=1 TO Y.TOT.TIP.POL
        LOCATE Y.POL.TYPE IN R.COLL.POL<REDO.CPL.TIPO.POLIZA,I.VAR> SETTING POS THEN
            Y.SW = 0
            EXIT
        END
    NEXT

    IF Y.SW EQ 1 THEN
        Y.TOT.COLL.CODE = DCOUNT(R.COLL.POL<REDO.CPL.COLLATERAL.CODE>,@VM)
        FOR I.VAR=1 TO Y.TOT.COLL.CODE
            Y.TOT.POL.TYPE = DCOUNT(R.COLL.POL<REDO.CPL.POLICY.TYPE,I.VAR>,@SM)
            FOR J.VAR=1 TO Y.TOT.POL.TYPE
                LOCATE Y.POL.TYPE IN R.COLL.POL<REDO.CPL.POLICY.TYPE,I.VAR,J.VAR> SETTING POS THEN
                    Y.SW = 0
                    I.VAR = Y.TOT.COLL.CODE
                    EXIT
                END
            NEXT
        NEXT
    END

    IF Y.SW EQ 1 THEN
        AF = COLL.FIELD.NAME
        ETEXT = "EB-B2-ASSOCIATED.LOAN"
        CALL STORE.END.ERROR
    END

RETURN
***</region>
***<region name=ASSOCIATED.LOAN>
*===============
ASSOCIATED.LOAN:
*===============

    NO.OF.COLL     = DCOUNT(R.NEW(COLL.FIELD.NAME),@VM)
    APP.NAME      = 'AA.ARR.TERM.AMOUNT'
    ASSOCIATED.AMT = 'AMOUNT'

    CALL EB.GET.APPL.FIELD(APP.NAME, ASSOCIATED.AMT, '',Y.APP.ERR)
    FOR ITR.COLL=1 TO NO.OF.COLL

        REC.ID = R.NEW(COLL.FIELD.NAME)<1,ITR.COLL>   ;*Associated AA IDs
        GOSUB SELECT.AA.TERM

    NEXT
    R.NEW(INS.DET.INS.AMOUNT)<1,1> = AMT<1,1>
RETURN
***</region>
***<region name=SELECT.AA.TERM>
*============
SELECT.AA.TERM:
*============
    PROPERTY.CLASS = 'TERM.AMOUNT'
    CALL F.READ(FN.AA.ARR, REC.ID, R.AA.ARR, F.AA.ARR, Y.APP.ERR)

    CALL F.READ(FN.AA.ACCOUNT.DETAILS,Y.AA.ID,R.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS,Y.ERR)
    Y.ARR.DATE = R.AA.ACCOUNT.DETAILS<AA.AD.BASE.DATE>        ;* Obtiene la fecha base del presatamo para poder extraer las actividades
    Y.AA.ID = REC.ID
    GOSUB GET.PROPERTY
    GOSUB GET.AA.CONDITIONS
    IF returnConditions THEN
        R.AA.TERM.AMT = returnConditions
    END

    IF R.AA.ARR<AA.ARR.CURRENCY> NE R.NEW(INS.DET.CURRENCY) THEN
        GOSUB CHANGE.MONEY.AA
        AMT += AMT.CURR   ;*R22 AUTO CONVERSTION AMT+ AMT.CURR TO AMT+
    END ELSE
        AMT += R.AA.TERM.AMT<ASSOCIATED.AMT>
    END



RETURN
***</region>
***<region name=AMT.NOMINAL>
*============
AMT.NOMINAL:
*============
    COLL.FIELD.NAME = 'COLLATERAL.ID'
    CALL EB.GET.APPL.FIELD(APPLICATION,COLL.FIELD.NAME,'',Y.APP.ERR)
    NO.OF.COLL=DCOUNT(R.NEW(COLL.FIELD.NAME),@VM)
    FOR ITR.COLL=1 TO NO.OF.COLL
        REC.ID = R.NEW(COLL.FIELD.NAME)<1,ITR.COLL>
        Y.CAMPO = Y.L.COL.TOT.VALUA.POS
        GOSUB GET.VALUE.AMOUNT
    NEXT

RETURN
***</region>
***<region name=GET.VALUE.AMOUNT>
*=================
GET.VALUE.AMOUNT:
*=================

    LOAN.FIELD.NAME = 'ASSOCIATED.LOAN'
    CALL EB.GET.APPL.FIELD(APPLICATION,LOAN.FIELD.NAME,'',Y.APP.ERR)
    NO.OF.LOAN = DCOUNT(R.NEW(LOAN.FIELD.NAME),@VM)


    FOR ITR.LOAN=1 TO NO.OF.LOAN
        COLL.FIELD.NAME = 'COLLATERAL.ID'
        CALL EB.GET.APPL.FIELD(APPLICATION,COLL.FIELD.NAME,'',Y.APP.ERR)
        NO.OF.COLL = DCOUNT(R.NEW(COLL.FIELD.NAME)<1,ITR.LOAN>,@SM)
        FOR ITR.COLL=1 TO NO.OF.COLL
            REC.ID = R.NEW(COLL.FIELD.NAME)<1,ITR.LOAN,ITR.COLL>
            CALL F.READ(FN.COLLATERAL, REC.ID, R.COLLATERAL, F.COLLATERAL, Y.APP.ERR)
            IF R.COLLATERAL NE '' THEN
                IF R.COLLATERAL<COLL.CURRENCY> NE R.NEW(INS.DET.CURRENCY) THEN
                    GOSUB CHANGE.MONEY.COLL
                    AMT += AMT.CURR    ;*R22 AUTO CONVERSTION AMT+ AMT.CURR TO AMT+
                END ELSE
                    AMT = AMT + R.COLLATERAL<COLL.LOCAL.REF,Y.CAMPO>
                END
            END
        NEXT
    NEXT
    R.NEW(INS.DET.INS.AMOUNT)<1,1> = AMT<1,1>
RETURN
***</region>
***<region name=CHANGE.MONEY.AA>
*============
CHANGE.MONEY.AA:
*============
* CHANGE MONEY IN AA
    Y.ID.CURR = R.AA.ARR<AA.ARR.CURRENCY>
    CALL CACHE.READ(FN.CURR, Y.ID.CURR, R.CURR, Y.APP.ERR) ;*R22 AUTO CONVERSTION F.READ TO CACHE.READ AND F.CURR TO R.CURR
    IF Y.ID.CURR EQ 'DOP' THEN
        RETURN
    END
    LOCATE '1' IN R.CURR<EB.CUR.CURRENCY.MARKET,1> SETTING Z THEN
        AMT.CURR = R.AA.TERM.AMT<ASSOCIATED.AMT> * R.CURR<EB.CUR.REVAL.RATE,Z>
    END
RETURN
***</region>
***<region name=CHANGE.MONEY.COLL>
*============
CHANGE.MONEY.COLL:
*============
* CHANGE MONEY IN COLLATERAL
    IF R.COLLATERAL<COLL.CURRENCY> NE R.NEW(INS.DET.CURRENCY) THEN
        Y.ID.CURR = R.COLLATERAL<COLL.CURRENCY>
        CALL CACHE.READ(FN.CURR, Y.ID.CURR, R.CURR, Y.APP.ERR) ;*R22 AUTO CONVERSTION F.READ TO CACHE.READ
        IF Y.ID.CURR NE 'DOP' THEN
            LOCATE '1' IN R.CURR<EB.CUR.CURRENCY.MARKET,1> SETTING Z THEN
                AMT.CURR = R.COLLATERAL<COLL.LOCAL.REF,Y.CAMPO> * R.CURR<EB.CUR.REVAL.RATE,Z>
            END
        END
    END

RETURN
***</region>
***<region name=EXTRACT.EXP.DATE>
*============
EXTRACT.EXP.DATE:
*============

    NO.OF.LOAN=DCOUNT(R.NEW(INS.DET.ASSOCIATED.LOAN),@VM)
    FOR ITR.LOAN=1 TO NO.OF.LOAN
        ARR.ID = R.NEW(INS.DET.ASSOCIATED.LOAN)<1,ITR.LOAN>
        CALL F.READ(FN.AA.ACCOUNT.DETAILS, ARR.ID, R.AA.ACCOUNT.DETAILS, F.AA.ACCOUNT.DETAILS, READ.ERR)
        POL.EXP.DATE.LOANS<-1> = R.AA.ACCOUNT.DETAILS<AA.AD.MATURITY.DATE>

        IF ITR.LOAN EQ 1 THEN
            MAX.EXP.DATE = R.AA.ACCOUNT.DETAILS<AA.AD.MATURITY.DATE>
        END  ELSE
            IF R.AA.ACCOUNT.DETAILS<AA.AD.MATURITY.DATE> GT MAX.EXP.DATE THEN
                MAX.EXP.DATE = R.AA.ACCOUNT.DETAILS<AA.AD.MATURITY.DATE>
            END
        END
    NEXT
    AMT = MAX.EXP.DATE
RETURN
***</region>
***<region name=GET.PROPERTY>
*==============
GET.PROPERTY:
*==============
* Get the property Name for the property class

    ARR.INFO = Y.AA.ID
    R.ARRANGEMENT = ''
    AA.PROPERTY = ''
    CLASS.LIST = ''
    CLASS.CTR = ''
    PROP.LIST = ''
    CALL AA.GET.ARRANGEMENT.PROPERTIES(ARR.INFO, Y.ARR.DATE, R.ARRANGEMENT, PROP.LIST)
    CALL AA.GET.PROPERTY.CLASS (PROP.LIST, CLASS.LIST)
    CLASS.LIST = RAISE(CLASS.LIST)
    PROP.LIST = RAISE(PROP.LIST)
    LOOP
        REMOVE Y.CLASS FROM CLASS.LIST SETTING CLASS.POS
        CLASS.CTR +=1
    WHILE Y.CLASS:CLASS.POS
        IF Y.CLASS EQ PROPERTY.CLASS THEN
            AA.PROPERTY = PROP.LIST<CLASS.CTR>
            RETURN
        END
    REPEAT
RETURN
***</region>
***<region name=GET.AA.CONDITIONS>
*====================
GET.AA.CONDITIONS:
*====================
    Y.EFFEC.DATE = TODAY
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.INFO,PROPERTY.CLASS,AA.PROPERTY,Y.EFFEC.DATE,returnIds,returnConditions,returnError)
    CHANGE @VM TO @FM IN returnConditions
    CHANGE @SM TO @VM IN returnConditions
RETURN

***</region>
END
