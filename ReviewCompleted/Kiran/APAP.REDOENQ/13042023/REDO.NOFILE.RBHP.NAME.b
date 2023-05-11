$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOFILE.RBHP.NAME(Y.OUT)
*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : john chrisptopher
* Program Name :
*-----------------------------------------------------------------------------
* Description :Enquiry routine to retreive image of padrones
* Linked with :
* In Parameter :
* Out Parameter :
*
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 13-APRIL-2023      Conversion Tool       R22 Auto Conversion - FM to @FM
* 13-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_System
    $INSERT I_F.REDO.ID.CARD.CHECK
    $INSERT JBC.h

    GOSUB INIT
    GOSUB PROCESS
RETURN

*******
INIT:
*******
    Y.OUT=''

    FN.CUS.RNC = 'F.CUSTOMER.L.CU.RNC'
    F.CUS.RNC = ''
    CALL OPF(FN.CUS.RNC,F.CUS.RNC)

    FN.CUS.CIDENT = 'F.CUSTOMER.L.CU.CIDENT'
    F.CUS.CIDENT = ''
    CALL OPF(FN.CUS.CIDENT,F.CUS.CIDENT)

    FN.CUS.LEGAL.ID = 'F.REDO.CUSTOMER.LEGAL.ID'
    F.CUS.LEGAL.ID = ''
    CALL OPF(FN.CUS.LEGAL.ID,F.CUS.LEGAL.ID)

    FN.CUSTOMER='F.CUSTOMER'
    F.CUSTOMER=''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

RETURN
********
PROCESS:
********
    BEGIN CASE
        CASE R.NEW(REDO.CUS.PRF.IDENTITY.TYPE) EQ "CEDULA"
            CIDENT.NUMBER = R.NEW(REDO.CUS.PRF.IDENTITY.NUMBER)
            GOSUB CIDENT.PROOF.CHECK
        CASE R.NEW(REDO.CUS.PRF.IDENTITY.TYPE) EQ "RNC"
            RNC.NUMBER = R.NEW(REDO.CUS.PRF.IDENTITY.NUMBER)
            GOSUB RNC.PROOF.CHECK
        CASE R.NEW(REDO.CUS.PRF.IDENTITY.TYPE) EQ "PASAPORTE"
            PASSPORT.NUMBER = R.NEW(REDO.CUS.PRF.IDENTITY.NUMBER)
            GOSUB PASSPORT.PROOF.CHECK
    END CASE
RETURN
********************
CIDENT.PROOF.CHECK:
********************
    CALL F.READ(FN.CUS.CIDENT,CIDENT.NUMBER,R.CUS.CIDENT,F.CUS.CIDENT,CUS.ERR)
    IF R.CUS.CIDENT THEN
        CUS.ID = FIELD(R.CUS.CIDENT,"*",2)
        CALL F.READ(FN.CUSTOMER,CUS.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
        IF NOT(CUSTOMER.ERR) THEN
            Y.OUT = R.CUSTOMER<EB.CUS.NAME.1>
        END
    END ELSE
        GOSUB FETCH.PADRONE.CIDENT
    END
RETURN
*****************
RNC.PROOF.CHECK:
******************
    CALL F.READ(FN.CUS.RNC,RNC.NUMBER,R.CUS.RNC,F.CUS.RNC,CUS.RNC.ERR)
    IF R.CUS.RNC THEN
        CUS.ID = FIELD(R.CUS.RNC,"*",2)
        CALL F.READ(FN.CUSTOMER,CUS.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
        IF NOT(CUSTOMER.ERR) THEN
            Y.OUT = R.CUSTOMER<EB.CUS.NAME.1>
        END
    END ELSE
        GOSUB FETCH.PADRONE.RNC
    END
RETURN
***********************
PASSPORT.PROOF.CHECK:
************************
    CALL F.READ(FN.CUS.LEGAL.ID,PASSPORT.NUMBER,R.CUS.LEGAL.ID,F.CUS.LEGAL.ID,CUS.LEGAL.ERR)
    IF R.CUS.LEGAL.ID THEN
        CUS.ID = FIELD(R.CUS.LEGAL.ID,"*",2)
        CALL F.READ(FN.CUSTOMER,CUS.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
        Y.OUT = R.CUSTOMER<EB.CUS.NAME.1>
    END
RETURN
**********************
FETCH.PADRONE.CIDENT:
**********************
    Cedule = "padrone$":CIDENT.NUMBER
    Param1 = "com.padrone.ws.util.MainClass"
    Param2 = "callPadrone"
    Param3 = Cedule
    Ret = ""
    ACTIVATION = "APAP_PADRONES_WEBSERVICES"
    INPUT_PARAM=Cedule
    ERROR.CODE = CALLJEE(ACTIVATION,INPUT_PARAM)
    IF ERROR.CODE THEN
        E= "EB-JAVACOMP":@FM:ERROR.CODE
    END ELSE
        Ret=INPUT_PARAM
    END
    IF Ret NE "" THEN
        CIDENT.RESULT = Ret
        CHANGE '$' TO '' IN CIDENT.RESULT
        CHANGE '#' TO @FM IN CIDENT.RESULT
        CIDENT.RESULT.ERR = CIDENT.RESULT<1>
        CHANGE '::' TO @FM IN CIDENT.RESULT.ERR
        CHANGE '::' TO @FM IN CIDENT.RESULT
        IF CIDENT.RESULT.ERR<1> EQ "SUCCESS" THEN     ;* On successfull CIDENT number

            Y.APELLIDO = CIDENT.RESULT<2>
            Y.NOMBRE = CIDENT.RESULT<4>
            CUSTOMER.FULL.NAME = Y.NOMBRE:' ':Y.APELLIDO

            Y.OUT = CUSTOMER.FULL.NAME
        END
    END
RETURN
*********************
FETCH.PADRONE.RNC:
***********************
    Cedule = "rnc$":RNC.NUMBER
    Param1 = "com.padrone.ws.util.MainClass"
    Param2 = "callPadrone"
    Param3 = Cedule
    Ret = ""
    ACTIVATION = "APAP_PADRONES_WEBSERVICES"
    INPUT_PARAM=Cedule
    ERROR.CODE = CALLJEE(ACTIVATION,INPUT_PARAM)
    IF ERROR.CODE THEN
        E= "EB-JAVACOMP":@FM:ERROR.CODE
    END ELSE
        Ret=INPUT_PARAM
    END

    IF Ret NE "" THEN
        RNC.RESULT = Ret
        CHANGE '$' TO '' IN RNC.RESULT
        CHANGE '#' TO @FM IN RNC.RESULT
        RNC.RESULT.ERR = RNC.RESULT<1>
        CHANGE '::' TO @FM IN RNC.RESULT.ERR
        IF RNC.RESULT.ERR<1> EQ "SUCCESS" THEN
            CUSTOMER.FULL.NAME = RNC.RESULT<2>
            Y.OUT = CUSTOMER.FULL.NAME
        END
    END
RETURN
*****************************************************************
END
