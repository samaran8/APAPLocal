* @ValidationCode : MjotNDM0NDcwOTU4OkNwMTI1MjoxNjgyNTc4NjEwNzczOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 27 Apr 2023 12:26:50
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.VAL.CRED.COLL
*
* Subroutine Type : ROUTINE
* Attached to     : REDO.V.VAL.CRED.COLL
* Attached as     : ROUTINE
* Primary Purpose : CALCULATE THE SALD ARRANGEMENT AND JOIN THE ARRANGEMENT
*                   WITH THE COLLARERAL
* Incoming:
* ---------
*
* Outgoing:
* ---------
*
* Error Variables:
*-----------------------------------------------------------------------------------
* Modification History:
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Pablo Castillo De La Rosa - TAM Latin America
* Date            : DISPO VALUE FOR ARRANGEMENT
*Modification history
*Date                Who               Reference                  Description
*19-04-2023      conversion tool     R22 Auto code conversion     VM TO @VM,FM TO @FM,SM TO @SM,++ TO +=1
*19-04-2023      Mohanraj R          R22 Manual code conversion   No changes
*-----------------------------------------------------------------------------------
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COLLATERAL
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AC.BALANCE.TYPE
    $INSERT I_F.ACCT.ACTIVITY
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.FC.CL.BALANCE
    $INSERT I_F.AA.LIMIT
    $INSERT I_F.REDO.CREATE.ARRANGEMENT
    $INSERT I_F.COLLATERAL.RIGHT
    $INSERT I_F.ACCOUNT
    $USING APAP.TAM
*
    GOSUB INITIALISE
    GOSUB OPEN.FILES
*
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
* PACS00312875 - S
        IF Y.COLL.CODE EQ 350 OR Y.COLL.CODE EQ 450 THEN
            CALL RED.AUTH.APLY.COLL.AAA.VH
        END ELSE
            CALL RED.AUTH.APLY.COLL.AAA
        END
* PACS00312875 - E
    END

*   GOSUB COLLATERAL.RIGHT


RETURN          ;* Program RETURN
*-----------------------------------------------------------------------------------


PROCESS:
*======
*
    VAR.CRED   = R.NEW(COLL.LOCAL.REF)<1,WPOS.CRED>
    VAR.GARA   = ID.NEW

    IF NOT(VAR.CRED) THEN     ;*IF THE CREDIT DOESN'T EXISTE EXIT
        RETURN
    END
*
    GOSUB GET.AAID.SM         ;* PACS00307565 - S/E
*
*ERROR MESSAGE WHEN THE ARRANGEMETN DO NOT EXIST
    IF NOT(VAR.COD.BALANCE) THEN
        AF = COLL.LOCAL.REF
        AV = YPOS<1,1>
        AS = Y.VAR  ;* PACS00315670 - S/E
        ETEXT = 'ST-ERROR.SEL.CRED'
        CALL STORE.END.ERROR
        RETURN
    END
*
**-- GET THE COLLATERAL.RIGHT FOR THE ARRANGEMENT --**

    VAR.LENG = LEN(VAR.GARA)
    VAR.AUX  = VAR.GARA

    X.VAR =1 ;*R22 Auto code conversion
    P = 0
    LOOP
    WHILE X.VAR LE VAR.LENG ;*R22 Auto code conversion


        CARACTER = LEFT(VAR.AUX,1)
        VAR.AUX  = RIGHT(VAR.AUX,VAR.LENG - X.VAR) ;*R22 Auto code conversion
        IF CARACTER EQ  '.' THEN
            P+=1
        END

*COPY THE STRING BEFORE THE SECOND DOT
        IF (P EQ 2) AND CARACTER EQ  '.' THEN
            W.VAR = X.VAR -1 ;*R22 Auto code conversion
            VAR.LIM = LEFT(VAR.GARA,W.VAR) ;*R22 Auto code conversion
        END
        IF (P EQ 1) AND CARACTER EQ  '.' THEN
            W.VAR = X.VAR -1 ;*R22 Auto code conversion
            VAR.CLIE = LEFT(VAR.GARA,W.VAR) ;*R22 Auto code conversion
        END
        X.VAR+=1 ;*R22 Auto code conversion
    REPEAT


**INSERT THE JOIN BETWEEN COLLATERALS AND ARRANGEMENT TEMPLATE**

    GOSUB VERIFICA.TEMPLATE

    IF VAR.BANDERA EQ 'TRUE' THEN
        GOSUB INSERTAR
    END

RETURN
*----------------------------------------------------------------------------

GET.AAID.SM:
*===========
*
    Y.AA.NUM = DCOUNT(VAR.CRED,@SM)
    Y.VAR = 1
    LOOP
    WHILE Y.VAR LE Y.AA.NUM
        Y.AA.ID = FIELD(VAR.CRED,@SM,Y.VAR)
        GOSUB READ.AA.ACCT
        IF Y.ERR NE "" THEN
            BREAK
        END
*
        Y.VAR += 1
    REPEAT
*
RETURN
*
READ.AA.ACCT:
*===========
*
    IF Y.AA.ID MATCHES "AA..." THEN     ;*GET THE INFORMATION FOR THE SALD FOR ARRANGEMENT
        GOSUB READ.AA.ARRANGEMENT
    END
    ELSE
        GOSUB READ.ACCOUNT
    END
*
RETURN
*
INSERTAR:
*========
*INSERT ONE RECORD IN REDO.FC.CL.BALANCE JOIN THE ARRANGEMENT AND COLLATERAL
*
*    Y.AA.ID =  VAR.CRED ;* PACS00307565 - S/E
    CALL F.READ(FN.REDO.FC.CL.BALANCE,Y.AA.ID,R.REDO.FC.CL.BALANCE,F.REDO.FC.CL.BALANCE,Y.BALANCE.ERR.MSJ)

    IF Y.BALANCE.ERR.MSJ THEN
        RETURN
    END
    ELSE
        LOCATE VAR.GARA IN R.REDO.FC.CL.BALANCE<FC.CL.COLLATERAL.ID,1> SETTING Y.COLL.POS THEN
            RETURN
        END
        NUM.COL.ID = DCOUNT(R.REDO.FC.CL.BALANCE<FC.CL.COLLATERAL.ID>,@VM)
        NUM.COL.ID += 1

        Y.BAL.CRED = R.REDO.FC.CL.BALANCE<FC.CL.AA.BALANCE>
        Y.MG.ACT = SUM(R.REDO.FC.CL.BALANCE<FC.CL.MG.ACTUAL>)

        GOSUB GET.AVAL.USE

        R.REDO.FC.CL.BALANCE<FC.CL.COLLATERAL.RIGHT,NUM.COL.ID> = VAR.LIM
        R.REDO.FC.CL.BALANCE<FC.CL.COLLATERAL.ID,NUM.COL.ID>    = VAR.GARA

        IF Y.COLL.AVAL.USE GT Y.BAL.CRED THEN
            Y.MG.ACT = Y.COLL.AVAL.USE- (Y.COLL.AVAL.USE- Y.BAL.CRED)
        END
        ELSE
            Y.MG.ACT = Y.COLL.AVAL.USE
        END

        R.REDO.FC.CL.BALANCE<FC.CL.MG.ACTUAL,NUM.COL.ID>        = Y.MG.ACT
        R.REDO.FC.CL.BALANCE<FC.CL.MG.ORIGINAL,NUM.COL.ID>      = Y.MG.ACT      ;*R.NEW(COLL.NOMINAL.VALUE)
        CALL F.WRITE(FN.REDO.FC.CL.BALANCE,Y.AA.ID,R.REDO.FC.CL.BALANCE)
    END


*SET THE VALUES FOR LIMIT
    GOSUB SELECT.LIMIT
    GOSUB LIMIT.AA
RETURN
*--------------------------

LIMIT.AA:
*========
*SAVE THE DATA FOR LIMIT REDO.FC.LIMIT.AA

    Y.LIMIT.COT.FILE = VAR.ID.LIMIT
*AA Changes 20161013
*  Y.LIMIT.ID.COT.FILE = FMT(Y.LIMIT.COT.FILE,"10'0'R")
    Y.LIMIT.ID.COT.FILE = FMT(Y.LIMIT.COT.FILE,"7'0'R")
*  Y.AA.LIMIT = VAR.CLIE:".":Y.LIMIT.ID.COT.FILE
    Y.AA.LIMIT = VAR.CLIE:".":Y.LIMIT.ID.COT.FILE:".":Y.LIMIT.SERIAL
*AA Changes 20161013

    FN.REDO.FC.LIMIT.AA = "F.REDO.FC.LIMIT.AA"    ;* Nombre concat file
    CALL CONCAT.FILE.UPDATE(FN.REDO.FC.LIMIT.AA,Y.AA.LIMIT,Y.AA.ID,'I','AR')
RETURN
*-------------------------
SELECT.LIMIT:
*===========
*GET THE LIMIT ASSOCIATED WITH THE ARRANGEMENT

    VAR.CRED = Y.AA.ID        ;* PACS00307565 - S/E
    SELECT.STATEMENT = 'SELECT ':FN.AA.ARR.LIMIT:' WITH @ID LIKE ':VAR.CRED :'...'
    LOCK.LIST = ''
    LIST.NAME = ''
    SELECTED = ''
    SYSTEM.RETURN.CODE = ''
    Y.ID.AA.PRD = ''
    CALL EB.READLIST(SELECT.STATEMENT,LOCK.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)

*GET ALL VALUES FROM THE LIST AND ADD THE LOCK VALUE
    LOOP
        REMOVE Y.ID.AA.PRD FROM LOCK.LIST SETTING POS
    WHILE Y.ID.AA.PRD:POS

        CALL F.READ(FN.AA.ARR.LIMIT, Y.ID.AA.PRD, R.SALD,F.AA.ARR.LIMIT, Y.ERR)
        VAR.ID.LIMIT  =  R.SALD<AA.LIM.LIMIT.REFERENCE>
*AA Changes 20161013
        Y.LIMIT.SERIAL = R.SALD<AA.LIM.LIMIT.SERIAL>
*AA Changes 20161013
    REPEAT


RETURN
*---------------
VERIFICA.TEMPLATE:
*=================

    VAR.BANDERA = 'FALSE'
    VAR.CRED = Y.AA.ID        ;* PACS00307565 - S/E
    SELECT.STATEMENT = 'SELECT ':FN.TEMPLATE:' WITH ID.ARRANGEMENT EQ ':VAR.CRED
    LOCK.LIST = ''
    LIST.NAME = ''
    SELECTED = ''
    SYSTEM.RETURN.CODE = ''
    Y.ID.AA.PRD = ''
    CALL EB.READLIST(SELECT.STATEMENT,LOCK.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)

*GET ALL VALUES FROM THE LIST AND ADD THE LOCK VALUE
    LOOP
        REMOVE Y.ID.AA.PRD FROM LOCK.LIST SETTING POS
    WHILE Y.ID.AA.PRD:POS
        VAR.BANDERA = 'TRUE'
    REPEAT

RETURN
*----------------
COLLATERAL.RIGHT:
*================
**UPDATE INFORMATION FOR COLLATERAL.RIGHT

    GOSUB SELECT.LIMIT

    Y.LIMIT.COT.FILE = VAR.ID.LIMIT
    Y.LIMIT.ID.COT.FILE = FMT(Y.LIMIT.COT.FILE,"10'0'R")
    Y.AA.LIMIT = VAR.CLIE:".":Y.LIMIT.ID.COT.FILE

    VAR.COLL.RIGHT = VAR.LIM
*GET INFORMATION FOR COLLATERAL.RIGHT FOR UNDATE
    CALL F.READ(FN.COLL.RIGHT,Y.ID.AA.PRD,R.COLL.RIGHT,F.COLL.RIGHT,Y.BALANCE.ERR.MSJ)

    R.COLL.RIGHT<COLL.RIGHT.LIMIT.REFERENCE> = Y.AA.LIMIT
    R.COLL.RIGHT<COLL.RIGHT.LIMIT.ID> = Y.AA.LIMIT

    CALL F.WRITE(FN.COLL.RIGHT,Y.ID.AA.PRD,R.COLL.RIGHT)

RETURN
*
*----------------
INITIALISE:
*=========
    PROCESS.GOAHEAD = 1

    VAR.CRED   = ''
    VAR.GARA   = ''
    VAR.CLIE   = ''
    VAR.LIM.CRED = ''
    VAR.LIM.TIPO = ''

    FN.SALD  = 'F.EB.CONTRACT.BALANCES'
    F.SALD   = ''
    R.SALD   = ''

    FN.CRED  = 'F.AA.ARRANGEMENT'
    F.CRED   = ''
    R.CRED   = ''

    FN.AC.BALANCE.TYPE = 'F.AC.BALANCE.TYPE'
    F.AC.BALANCE.TYPE  = ''
    R.AC.BALANCE.TYPE  = ''

*Read the local fields
    WCAMPO    = "L.AC.LK.COL.ID"
    WCAMPO<2> = "L.COL.LN.MX.VAL"
    WCAMPO<3> = "L.COL.VAL.AVA"
    WCAMPO = CHANGE(WCAMPO,@FM,@VM)
    YPOS  =0

*Get the position for all fields
    CALL MULTI.GET.LOC.REF("COLLATERAL",WCAMPO,YPOS)
    WPOS.CRED  = YPOS<1,1>
    WPOS.MAX   = YPOS<1,2>
    WPOS.DISP  = YPOS<1,3>
*BALANCE
    FN.REDO.FC.CL.BALANCE = 'F.REDO.FC.CL.BALANCE'
    F.REDO.FC.CL.BALANCE  = ''
    R.REDO.FC.CL.BALANCE  = ''
*GET THE LIMIT ASSOCIATE  TO ARRAGEMENT
    FN.AA.ARR.LIMIT = 'F.AA.ARR.LIMIT'
    F.AA.ARR.LIMIT  = ''
    R.AA.ARR.LIMIT  = ''
*GET TEMPLATE INFORMATION
    FN.TEMPLATE = 'F.REDO.CREATE.ARRANGEMENT'
    F.TEMPLATE  = ''
    R.TEMPALTE  = ''
*GET COLLATERAL.RIGHT INFORMATION
    FN.COLL.RIGHT = 'F.COLLATERAL.RIGHT'
    F.COLL.RIGHT  = ''
    R.COLL.RIGHT  = ''

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    R.ACCOUNT = ''
    Y.PRODUCT.ID.CUR = ''

    Y.COLL.CODE = R.NEW(COLL.COLLATERAL.CODE)

RETURN

*------------------------
OPEN.FILES:
*=========
    CALL OPF(FN.SALD,F.SALD)
    CALL OPF(FN.CRED,F.CRED)
    CALL OPF(FN.AC.BALANCE.TYPE,F.AC.BALANCE.TYPE)
    CALL OPF(FN.REDO.FC.CL.BALANCE,F.REDO.FC.CL.BALANCE)
    CALL OPF(FN.AA.ARR.LIMIT,F.AA.ARR.LIMIT)
    CALL OPF(FN.TEMPLATE, F.TEMPLATE)
    CALL OPF(FN.COLL.RIGHT, F.COLL.RIGHT)
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

RETURN
*------------
READ.AA.ARRANGEMENT:
*------------
    CALL F.READ(FN.CRED, Y.AA.ID, R.CRED, F.CRED, Y.ERR)
    VAR.COD.BALANCE    =  R.CRED<AA.ARR.LINKED.APPL.ID>
    Y.PRODUCT.ID.CUR<1>  = R.CRED<AA.ARR.PRODUCT>
    Y.PRODUCT.ID.CUR<2> = R.CRED<AA.ARR.CURRENCY>
RETURN
*-----------------
READ.ACCOUNT:
*------------------
    CALL F.READ(FN.ACCOUNT,Y.AA.ID,R.ACCOUNT,F.ACCOUNT,Y.ERR)
    VAR.COD.BALANCE = Y.AA.ID ;* PACS00307565 - S/E
    Y.AA.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>
    GOSUB READ.AA.ARRANGEMENT
RETURN

*==================================
GET.AVAL.USE:
*==================================
    IF R.NEW(COLL.COLLATERAL.CODE) EQ 150 THEN
        Y.RISK.PERC = ''
        CALL APAP.TAM.redoColGetRiskDi(Y.PRODUCT.ID.CUR,Y.RISK.PERC) ;*R22 Manual code conversion      ;*Rutina para calcular el % de riesgo
        IF NOT(Y.RISK.PERC)THEN
            Y.RISK.PERC = 1
        END
        Y.COLL.AVAL.USE = (R.NEW(COLL.NOMINAL.VALUE)*100)/Y.RISK.PERC

    END
    ELSE
        Y.COLL.AVAL.USE = R.NEW(COLL.LOCAL.REF)<1,WPOS.MAX>

    END
RETURN

END
