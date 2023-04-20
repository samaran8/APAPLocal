SUBROUTINE REDO.FC.S.CUSTOMER.EVAL
*------------------------------------------------------------------------------------------------------------------
* Developer    : jvalarezoulloa@temenos.com
* Date         : 2012-04-17
* Description  : Evaluate if a Customer has a status Close
* Input/Output:
* -------------
* In  :
*
* Out :
*------------------------------------------------------------------------------------------------------------------
* Revision History:
* -----------------
* Version          Date               Name                      Description
* -------          ----               ----                      ------------
* MOdified by>
*
*                  18/09/2012         mgudino                   Add DECESED validation
*                  19/3/2012          Prakash                   Value in CUSTOMER's local field
*                                                               L.CU.TIPO.CL should not be 'CLIENTE MENOR'
*------------------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_EB.TRANS.COMMON
*
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.CREATE.ARRANGEMENT

    GOSUB INITIALISE
    GOSUB OPENFILES
    GOSUB PROCESS
    GOSUB CLIENTE.MENOR.CUS.CHECK

RETURN
*------------------------------------------------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------------------------------------------------
    Y.ID.CUSTOMER = COMI
    CALL F.READ(FN.CUSTOMER,Y.ID.CUSTOMER,R.CUSTOMER,F.CUSTOMER,ERR.MSJ)

    IF OFS$HOT.FIELD MATCHES 'CUSTOMER...' THEN
        GOSUB CUSTOMER.EVALUATE
    END
    ELSE
        IF COMI THEN
            Y.ID.CUSTOMER = R.NEW(REDO.FC.CUSTOMER)
            GOSUB CUSTOMER.EVALUATE
        END
    END

RETURN
*------------------------------------------------------------------------------------------------------------------
CUSTOMER.EVALUATE:
*------------------------------------------------------------------------------------------------------------------

    IF R.CUSTOMER THEN
        IF R.CUSTOMER<EB.CUS.CUSTOMER.STATUS> EQ 4 THEN
            AF = REDO.FC.CUSTOMER
            ETEXT = 'EB-FC-CUST-STATUS-CLOSE'
            CALL STORE.END.ERROR
        END
        IF R.CUSTOMER<EB.CUS.CUSTOMER.STATUS> EQ 3 THEN
            AF = REDO.FC.CUSTOMER
            ETEXT = 'EB-REDO.AC.DECESED'
            CALL STORE.END.ERROR
        END
    END ELSE
        AF = REDO.FC.CUSTOMER
        ETEXT = 'EB-FC-CUST-DONT-EXITS'
        CALL STORE.END.ERROR
    END

RETURN
*------------------------------------------------------------------------------------------------------------------
CLIENTE.MENOR.CUS.CHECK:
*------------------------------------------------------------------------------------------------------------------
* CLIENTE MENOR - RETAIL CUSTOMER not allowed - PACS00253689 - Start

    CU.TIPO.CL.VALUE = R.CUSTOMER<EB.CUS.LOCAL.REF,CU.TIPO.CL>
    IF CU.TIPO.CL.VALUE EQ 'CLIENTE MENOR' THEN
        AF = REDO.FC.CUSTOMER
        ETEXT = 'EB-CUS.TYPE.CLIENTE.MENOR'
        CALL STORE.END.ERROR
    END

* CLIENTE MENOR - RETAIL CUSTOMER not allowed - PACS00253689 - End
RETURN

*------------------------------------------------------------------------------------------------------------------
INITIALISE:
*------------------------------------------------------------------------------------------------------------------

    FN.CUSTOMER = "F.CUSTOMER"
    F.CUSTOMER  = ""
    R.CUSTOMER  = ""

*PACS00253689 - Start
    APPLICATION.ARRAY = "CUSTOMER"
    FLD.NAME = 'L.CU.TIPO.CL'
    Y.POS = ''

    CALL MULTI.GET.LOC.REF(APPLICATION.ARRAY,FLD.NAME,Y.POS)
    CU.TIPO.CL = Y.POS<1,1>
*PACS00253689 - End

RETURN
*------------------------------------------------------------------------------------------------------------------
OPENFILES:
*------------------------------------------------------------------------------------------------------------------
    CALL OPF(FN.CUSTOMER, F.CUSTOMER)
RETURN
END
