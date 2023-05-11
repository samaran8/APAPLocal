* @ValidationCode : MjoxOTIwOTIxOTY5OkNwMTI1MjoxNjgyNjY0Mzg2ODExOklUU1M6LTE6LTE6MTM4OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 28 Apr 2023 12:16:26
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 138
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOSRTN
SUBROUTINE REDO.S.FC.VI.EXP.TT.GRUPO.R(AA.ID,VI.ARR)

*
* Subroutine Type : ROUTINE
* Attached to     : ROUTINE REDO.FC.ENQPARMS
* Attached as     : ROUTINE
* Primary Purpose : Get the sum of customer operations and members of the group BY each Risk group
*
* Incoming:
* ---------
*
*
* Outgoing:
* ---------
* AA.ARR - data returned to the routine
*
* Error Variables:
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Bryan Torres- TAM Latin America
* Date            : 4/10/2001
*
* MG 2013 03 20
*Modification history
*Date                Who               Reference                  Description
*10-04-2023      conversion tool     R22 Auto code conversion     VM TO @VM,FM TO @FM,IF Condition added
*10-04-2023      Mohanraj R          R22 Manual code conversion   Add call routine prefix

*-----------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.CUSTOMER
    $INSERT I_System

    $USING APAP.REDOFCFI
    
    GOSUB INITIALISE
    GOSUB OPEN.FILES

    IF PROCESS.GOAHEAD THEN
        GOSUB VERIFY.PROCESS
    END

RETURN  ;* Program RETURN
*-----------------------------------------------------------------------------------
VERIFY.PROCESS:
*======

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    Y.ERROR = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    CALL F.READ(FN.CUSTOMER,Y.CUS.ID,R.CUSTOMER,F.CUSTOMER,Y.ERROR)
    IF R.CUSTOMER THEN
        IF R.CUSTOMER<EB.CUS.CUSTOMER.TYPE> EQ 'PROSPECT' THEN
            VI.ARR = ''
            RETURN
        END
    END

    IF ID.CUST THEN
        IF ID.CUST.LIM THEN
            IF ID.CUST EQ Y.CUS.ID THEN
                GOSUB PROCESS
            END ELSE
                GOSUB OFS.PROCESS
            END
        END
    END ELSE
        GOSUB OFS.PROCESS

    END

RETURN
*-----------------------------------------------------------------------------------
OFS.PROCESS:
*======


    Y.MESSAGE.ACC  = "REDO.CCRG.CUSTOMER,MAN/////,/,,CUSTOMER.ID::=" : Y.CUS.ID

    AC.EXPECTED.RECS = "TAM.OFS.SRC"
    OFS.RESP   = ""; TXN.COMMIT = "" ;* R22 Manual conversion - Start
*CALL OFS.GLOBUS.MANAGER("AC.EXPECTED.RECS",Y.MESSAGE.ACC)
    CALL OFS.CALL.BULK.MANAGER("AC.EXPECTED.RECS",Y.MESSAGE.ACC, OFS.RESP, TXN.COMMIT) ;* R22 Manual conversion - End
    Y.F.RES = FIELD(Y.MESSAGE.ACC,",",1)
    Y.RESCTA = FIELD(Y.F.RES,"/",3)

    IF Y.RESCTA LT 0 THEN
* error
    END ELSE
*CALL REDO.FC.E.MAIN(Y.CUS.ID,DATA.ENQ)
** R22 Manual conversion
        CALL APAP.REDOFCFI.redoFcEMain(Y.CUS.ID,DATA.ENQ)
        IF DATA.ENQ THEN
            GOSUB DISPONIB
        END

    END

RETURN


*-----------------------------------------------------------------------------------
Y.DATA.ENQ:
*======
    DATA.ENQ = CHANGE(DATA.ENQ,@FM,@VM)
    Y.COUNT.DATA = DCOUNT(DATA.ENQ,@VM)
    FOR Y.I = 1 TO Y.COUNT.DATA
        DATA.ENQ.AUX = DATA.ENQ
        DATA.ENQ.AUX = FIELD(DATA.ENQ.AUX<1,Y.I>,"*",1)
        Y.DATA.ENQ.AUX<-1>=DATA.ENQ.AUX
    NEXT Y.I
    Y.DATA.ENQ.AUX = CHANGE(Y.DATA.ENQ.AUX,@FM,@VM)
    GOSUB DISPONIB
RETURN

*-----------------------------------------------------------------------------------
DISPONIB:
*======




    LOCATE  "GLOBAL.LINKED" IN Y.DATA.ENQ.AUX<1,1> SETTING Y.RISK.GR THEN
        DEL DATA.ENQ<1,Y.RISK.GR>
        DEL Y.DATA.ENQ.AUX<1,Y.RISK.GR>
    END
    LOCATE  "INDIVIDUAL.EMPLOYEES" IN Y.DATA.ENQ.AUX<1,1> SETTING Y.RISK.IN THEN
        DEL DATA.ENQ<1,Y.RISK.IN>
        DEL Y.DATA.ENQ.AUX<1,Y.RISK.IN>
    END
    LOCATE  "RISK.GROUP.SECURED" IN Y.DATA.ENQ.AUX<1,1> SETTING Y.RISK.GR.S THEN
        DEL DATA.ENQ<1,Y.RISK.GR.S>
        DEL Y.DATA.ENQ.AUX<1,Y.RISK.GR.S>
    END
    LOCATE  "RISK.GROUP.UNSECURED" IN Y.DATA.ENQ.AUX<1,1> SETTING Y.RISK.GR.U THEN
        DEL DATA.ENQ<1,Y.RISK.GR.U>
        DEL Y.DATA.ENQ.AUX<1,Y.RISK.GR.U>
    END
    LOCATE  "RISK.INDIV.SECURED" IN Y.DATA.ENQ.AUX<1,1> SETTING Y.RISK.IN.S THEN
        DEL DATA.ENQ<1,Y.RISK.IN.S>
        DEL Y.DATA.ENQ.AUX<1,Y.RISK.IN.S>
    END
    LOCATE  "RISK.INDIV.UNSECURED" IN Y.DATA.ENQ.AUX<1,1> SETTING Y.RISK.IN.U THEN
        DEL DATA.ENQ<1,Y.RISK.IN.U>
        DEL Y.DATA.ENQ.AUX<1,Y.RISK.IN.U>
    END

    LOCATE  "GLOBAL.EMPLOYEES" IN Y.DATA.ENQ.AUX<1,1> SETTING Y.E THEN
        DEL DATA.ENQ<1,Y.E>
        DEL Y.DATA.ENQ.AUX<1,Y.E>
    END
    LOCATE  "HOUSING.PLAN.APAP" IN Y.DATA.ENQ.AUX<1,1> SETTING Y.H.P THEN
        DEL DATA.ENQ<1,Y.H.P>
        DEL Y.DATA.ENQ.AUX<1,Y.H.P>
    END
    LOCATE  "RISK.INDIV.TOTAL" IN Y.DATA.ENQ.AUX<1,1> SETTING Y.RISK.IN.T THEN
        DEL DATA.ENQ<1,Y.RISK.IN.T>
        DEL Y.DATA.ENQ.AUX<1,Y.RISK.IN.T>
    END








    GOSUB PROCESS

RETURN

*------------------------
PROCESS:
*=========
    YPOS.TOPE="3"
    Y.COUNT.DATA = DCOUNT(DATA.ENQ,@VM)
    FOR Y.I = 1 TO Y.COUNT.DATA
        Y.LIMIT.KIND=FIELD(DATA.ENQ,@VM,Y.I)
        Y.TOT.TOM = FIELD(Y.LIMIT.KIND,"*",YPOS.TOPE)
        Y.TOT.TOM = TRIM(Y.TOT.TOM, "", "D")
        Y.TOT.TOM = TRIM(Y.TOT.TOM, ",", "A")

        Y.TOT.TOMADO += Y.TOT.TOM ;*R22 Auto code conversion

    NEXT Y.I


    VI.ARR = Y.TOT.TOMADO

RETURN
*------------------------
INITIALISE:
*=========
    PROCESS.GOAHEAD = 1

    Y.CUS.ID = AA.ID

    VI.ARR =''

    ID.CUST.LIM = System.getVariable("ID.CUST.LIM")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto code conversion-START
        ID.CUST.LIM = ""
    END ;*R22 Auto code conversion-END


    ID.CUST = System.getVariable("ID.CUST")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto code conversion-SATRT
        ID.CUST = ""
    END ;*R22 Auto code conversion-END


RETURN

*------------------------
OPEN.FILES:
*=========

RETURN
*------------
END
