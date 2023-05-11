* @ValidationCode : Mjo0Mjc5MTEwNzE6Q3AxMjUyOjE2ODA2MTAyOTU2OTE6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 04 Apr 2023 17:41:35
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.NOFILE.CHARGES.DIS(R.DATA)
**
* Subroutine Type : ENQUIRY
* Attached to     : REDO
* Attached as     : NOFILE.ROUTINE
* Primary Purpose : Show disbursement charges list
*-----------------------------------------------------------------------------
* MODIFICATIONS HISTORY
*
* 1/11/10 - First Version.
*
*
** Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM SM TO @SM
**04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_ENQUIRY.COMMON

    $INSERT I_F.AA.PAYMENT.RULES
    $INSERT I_F.AA.PROPERTY
    $INSERT I_F.AA.PRODUCT.DESIGNER

*---------------------------------------------------------------------------------------------------------
MAIN.LOGIC:

    GOSUB INITIALISE
    GOSUB PROCESS

RETURN
*---------------------------------------------------------------------------------------------------------
INITIALISE:

    FN.AA.ARR.PAYRUL = 'F.AA.PRD.DES.PAYMENT.RULES'
    F.AA.ARR.PAYRUL = ''
    R.AA.ARR.PAYRUL = ''

    FN.AA.ARR.PROPERTY = 'F.AA.PROPERTY'
    F.AA.ARR.PROPERTY = ''
    R.AA.ARR.PROPERTY = ''

    FN.AA.PRODUCT.CATALOG = "F.AA.PRODUCT.CATALOG"
    F.AA.PRODUCT.CATALOG = ""
    R.AA.PRODUCT.CATALOG = ""


    Y.PROPERTIES.LIST = ''
    Y.CONT.PROPERTIES = 0
    Y.PROPERTY = ''
    Y.DESCRIPTION =''
    Y.BANDERA = 1


    Y.E.VALUE = D.RANGE.AND.VALUE<1>
    IF NOT(Y.E.VALUE) OR Y.E.VALUE EQ 0 THEN
        Y.E.VALUE = R.DATA
    END

    CALL OPF (FN.AA.ARR.PAYRUL, F.AA.ARR.PAYRUL)

    CALL OPF (FN.AA.ARR.PROPERTY, F.AA.ARR.PROPERTY)

    CALL OPF (FN.AA.PRODUCT.CATALOG, F.AA.PRODUCT.CATALOG)

RETURN
*---------------------------------------------------------------------------------------------------------
PROCESS:

*AA.ARR.PAYRUL.ID = 'CARGOS.DES'

    SELECT.STATEMENT = 'SELECT ':FN.AA.ARR.PAYRUL:" WITH @ID LIKE CARGOS.DES-..."
    AA.PAYMENT.RULES.LIST = ''
    LIST.NAME = ''
    SELECTED = ''
    SYSTEM.RETURN.CODE = ''
    CALL EB.READLIST(SELECT.STATEMENT,AA.PAYMENT.RULES.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)
    REMOVE AA.PAYMENT.RULES.ID FROM AA.PAYMENT.RULES.LIST SETTING AA.PAYMENT.RULES.MARK


    SELECT.STATEMENT = 'SELECT ':FN.AA.PRODUCT.CATALOG:" WITH @ID LIKE ":Y.E.VALUE:"..."
    AA.PAYMENT.RULES.LIST = ''
    LIST.NAME = ''
    SELECTED = ''
    SYSTEM.RETURN.CODE = ''
    CALL EB.READLIST(SELECT.STATEMENT,AA.PRODUCT.CATALOG.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)

    REMOVE AA.PRODUCT.CATALOG.ID FROM AA.PRODUCT.CATALOG.LIST SETTING AA.PRODUCT.CATALOG

    R.AA.ARR.PAYRUL = ''
    YERR = ''
    CALL F.READ(FN.AA.ARR.PAYRUL, AA.PAYMENT.RULES.ID , R.AA.ARR.PAYRUL, F.AA.ARR.PAYRUL, YERR)
    IF YERR THEN
        E = YERR
        RETURN
    END

    R.AA.PRODUCT.CATALOG = ''
    YERR = ''
    CALL F.READ(FN.AA.PRODUCT.CATALOG,AA.PRODUCT.CATALOG.ID,R.AA.PRODUCT.CATALOG,F.AA.PRODUCT.CATALOG,YERR)
    IF YERR THEN
        E = YERR
        RETURN
    END

*  Y.PROPERTIES.LIST =  R.AA.ARR.PAYRUL<AA.PAYRULE.PROPERTY>
*  Y.CONT.PROPERTIES = DCOUNT(Y.PROPERTIES.LIST, VM)
* Start Upgrade Change 20161006
    FINA.VAL = R.AA.ARR.PAYRUL<AA.PAYRULE.FINANCIAL.STATUS>
    LOCATE "BOTH" IN FINA.VAL<1> SETTING FIN.POSN THEN
        Y.PROPERTIES.LIST =  R.AA.ARR.PAYRUL<AA.PAYRULE.PROPERTY, FIN.POSN >
    END
    Y.CONT.PROPERTIES = DCOUNT(Y.PROPERTIES.LIST, @SM)

    Y.CONT  =1
    LOOP
    WHILE Y.CONT  LE Y.CONT.PROPERTIES
*    Y.PROPERTY = R.AA.ARR.PAYRUL<AA.PAYRULE.PROPERTY, Y.CONT>
        Y.PROPERTY = R.AA.ARR.PAYRUL<AA.PAYRULE.PROPERTY, FIN.POSN ,Y.CONT>
* End Upgrade Change 20161006
        Y.ERR = ''
        R.AA.ARR.PROPERTY = ''
        GOSUB FILTER.PRD.CATALOG
        IF Y.BANDERA THEN
            CALL CACHE.READ(FN.AA.ARR.PROPERTY, Y.PROPERTY, R.AA.ARR.PROPERTY, Y.ERR)
            Y.DESCRIPTION = R.AA.ARR.PROPERTY<AA.PROP.DESCRIPTION, 1>
            R.DATA<-1> = Y.PROPERTY:'*':Y.DESCRIPTION
        END
        Y.CONT +=1
    REPEAT

RETURN
*---------------------------------------------------------------------------------------------------------

FILTER.PRD.CATALOG:

    Y.BANDERA = 1
    LOCATE Y.PROPERTY IN R.AA.PRODUCT.CATALOG<AA.PRD.PROPERTY, 1> SETTING POS.Y ELSE
        Y.BANDERA = 0
    END

RETURN
*---------------------------------------------------------------------------------------------------------


END
