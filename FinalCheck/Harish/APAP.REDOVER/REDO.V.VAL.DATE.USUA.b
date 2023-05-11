* @ValidationCode : MjoyMDk5MDA3NTY1OkNwMTI1MjoxNjgxMzgyMTYyNjIzOnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 16:06:02
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
SUBROUTINE REDO.V.VAL.DATE.USUA
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*
* Subroutine Type : ROUTINE
* Attached to     : REDO.V.VAL.DATE
* Attached as     : ROUTINE
* Primary Purpose :
*
* Incoming:
* ---------
*
*
* Outgoing:
* ---------
*
*
* Error Variables:
*------------------------------------------------------------------------------

* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Pablo Castillo De La Rosa - TAM Latin America
* Date            :
*
*------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*13-04-2023            Conversion Tool             R22 Auto Code conversion                      FM TO @FM VM TO @VM,F.READ TO CACHE.READ
*13-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*-------------------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COLLATERAL
    $INSERT I_F.USER
    GOSUB INITIALISE


    GOSUB OPEN.FILES

    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END

RETURN  ;* Program RETURN
*------------------------------------------------------------------------------

PROCESS:
*======
*
    Y.ACTUAL = TODAY

*Get the values for the  fields (Value Date)
    Y.FEC1 = R.NEW(COLL.VALUE.DATE)

*######### Get the current user and validate date #########
    Y.USR.ID = OPERATOR
    CALL CACHE.READ(FN.USR, Y.USR.ID, R.USR, USER.ERR)   ;*R22 AUTO CODE CONVERSION

*GET USER LOG ON

    VAR.BAN.DATE = R.USR<EB.USE.LOCAL.REF,WPOSUSER>

*VALIDATE THE PARAMETER IS ASIGNED
    IF (VAR.BAN.DATE EQ '') THEN
        AF = EB.USE.LOCAL.REF
*  AV = YPOSU<1,1>
        AV = LOC.REF.POS<2,1>
        ETEXT = 'ST-REDO.COLLA.PARAM.USER'
        CALL STORE.END.ERROR
    END

*VALIDATE THAT THE USER HAVE ACCESS TO MODIFY THE DATE
    IF (VAR.BAN.DATE EQ 2) AND (Y.FEC1 NE Y.ACTUAL) THEN
        AF = COLL.VALUE.DATE
        AV = LOC.REF.POS<2,1>
        ETEXT = 'ST-REDO.FEC.VAL.NO.PER'
        CALL STORE.END.ERROR
    END

*TEST THE DATE
    IF Y.FEC1 GT Y.ACTUAL THEN
        AF = COLL.VALUE.DATE
        ETEXT = 'ST-REDO.COLLA.VALI.DATE'
        CALL STORE.END.ERROR
    END
*
*VALIDATE THE DATE IS LESS THAN ACTUAL DATE
    IF PGM.VERSION MATCHES "...MODIFICA..." ELSE
        IF (Y.FEC1 LT Y.ACTUAL) AND (VAR.BAN.DATE EQ 1)  THEN
            TEXT = "COLL.VAL.FEC.USUA"
            M.CONT = DCOUNT(R.NEW(COLL.VALUE.DATE),@VM) + 1
            CALL STORE.OVERRIDE(M.CONT)
        END
    END
RETURN
*------------------------------------------------------------------------

INITIALISE:
*=========

    PROCESS.GOAHEAD = 1
*Set the local fild for read
*  WCAMPO     = "VALUE.DATE"
* WCAMPO<2>  = "L.COL.GT.DATE"
*  WCAMPO<3>  = "L.COL.EXE.DATE"

* WCAMPO = CHANGE(WCAMPO,VM,FM)
* YPOS=0

*Get the position for all fields
* CALL MULTI.GET.LOC.REF("COLLATERAL",WCAMPO,YPOS)

* WPOSVADA  = YPOS<1,1>
* WPOSCODA  = YPOS<1,2>
* WPOSFODA  = YPOS<1,3>

*#### GET THE USER ######
    FN.USR  = 'F.USER'
    F.USR   = ''
    R.USR   = ''

*Set the field validate user date
* WCAMPOU = "VAL.MODI.DATE"

* WCAMPOU = CHANGE(WCAMPOU,VM,FM)
* YPOSU=''

*Get the position for all fields
* CALL MULTI.GET.LOC.REF("USER",WCAMPOU,YPOSU)
* WPOSUSER  = YPOSU<1,1>

    LOC.REF.APPL='COLLATERAL':@FM:'USER'
    LOC.REF.FIELDS='VALUE.DATE':@VM:'L.COL.GT.DATE':@VM:'L.COL.EXE.DATE':@FM:'VAL.MODI.DATE'
    LOC.REF.POS=" "
    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,LOC.REF.POS)
    WPOSVADA  = LOC.REF.POS<1,1>
    WPOSCODA  = LOC.REF.POS<1,2>
    WPOSFODA  = LOC.REF.POS<1,3>
    WPOSUSER  = LOC.REF.POS<2,1>

RETURN
*------------------------
OPEN.FILES:
*=========
    CALL OPF(FN.USR,F.USR)
RETURN
*------------
END
