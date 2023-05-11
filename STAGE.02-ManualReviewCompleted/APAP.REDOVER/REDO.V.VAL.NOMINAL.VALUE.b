* @ValidationCode : MjotNjcwNDk4MDAwOkNwMTI1MjoxNjgxNzMxOTAzMTk5OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 17:15:03
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
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.VAL.NOMINAL.VALUE
*
* ====================================================================================
*
*    - Gets the information related to the AA specified in input parameter
*
*    - Generates BULK OFS MESSAGES to apply payments to corresponding AA
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
*
*
*
* Outgoing:

* ---------
*
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for :
* Development by  :
* Date            :
*=======================================================================
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*17-04-2023            Conversion Tool             R22 Auto Code conversion                      FM TO @FM VM TO @VM,= TO EQ
*17-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*------------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COLLATERAL
    $INSERT I_GTS.COMMON

*
*************************************************************************


* =========
INITIALISE:
* =========

*For block change data when the user change esecution value 21/06/2012
*EXCECUTE ONLY IF PRESS HOT FIELD
    VAR.HOT = OFS$HOT.FIELD
    IF LEN(VAR.HOT) EQ 0 THEN
        RETURN
    END

    Y.NOMINAL.VALUE = COMI
    Y.EXE.VALUE = ''
    Y.EXE.VALUE = R.NEW(COLL.EXECUTION.VALUE)

*For when the user push validate button, some times 12/06/2012
    IF Y.NOMINAL.VALUE EQ '' THEN
        Y.NOMINAL.VALUE = 0
    END

*  IF LEN(TRIM(Y.EXE.VALUE)) EQ 0  THEN
    R.NEW(COLL.EXECUTION.VALUE) = Y.NOMINAL.VALUE   ;*** VALOR DE EJECUCION
*  END

    R.NEW(COLL.GEN.LEDGER.VALUE) = Y.NOMINAL.VALUE  ;*** VALOR LIBRO MAYOR
    R.NEW(COLL.MAXIMUM.VALUE) = Y.NOMINAL.VALUE     ;*** VALOR MAXIMO CORE

    WCAMPO = "L.COL.VAL.AVA"
    WCAMPO<2> = "L.COL.LN.MX.PER"
    WCAMPO<3> = "L.COL.LN.MX.VAL"
    WCAMPO<4> = "L.COL.TOT.VALUA"


    WCAMPO = CHANGE(WCAMPO,@FM,@VM)
    YPOS=0
    CALL MULTI.GET.LOC.REF("COLLATERAL",WCAMPO,YPOS)
    WPOSVALOR  = YPOS<1,1>
    WPOSPORC   = YPOS<1,2>
    WPOSMAX    = YPOS<1,3>
    WPOSTOTA   = YPOS<1,4>

*Verify if the % is Grate 0 for calculate el maximun value
*R.NEW(COLL.LOCAL.REF)<1,WPOSVALOR>= Y.NOMINAL.VALUE
    VAR.PORC =  R.NEW(COLL.LOCAL.REF)<1,WPOSPORC>

    IF VAR.PORC GT 0 THEN
        VAR.MAXIMO = (Y.NOMINAL.VALUE * VAR.PORC)/100
        R.NEW(COLL.LOCAL.REF)<1,WPOSMAX> = VAR.MAXIMO
        R.NEW(COLL.MAXIMUM.VALUE) = Y.NOMINAL.VALUE
    END

*SET VALUE FOR VH 14/05/2012
    VAR.TIPO = R.NEW(COLL.COLLATERAL.CODE)

    IF VAR.TIPO EQ 350 THEN
        R.NEW(COLL.LOCAL.REF)<1,WPOSTOTA> = Y.NOMINAL.VALUE
    END

RETURN

END
