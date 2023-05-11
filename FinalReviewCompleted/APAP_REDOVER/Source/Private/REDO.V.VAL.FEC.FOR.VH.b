* @ValidationCode : MjotMjQzNTU0MDc4OkNwMTI1MjoxNjgyNDEyMzYwNzM1OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:16:00
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.VAL.FEC.FOR.VH
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*
* Subroutine Type : ROUTINE
* Attached to     : REDO.V.VAL.FEC.FOR.VH
* Attached as     : ROUTINE
* Primary Purpose : Verify the Date of credits (Fecha de formalizacion)
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
* Development for : APAP
* Development by  : Pablo Castillo De La Rosa RTAM.
* Date            : 29/02/2011
*
*------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*17-04-2023            Conversion Tool             R22 Auto Code conversion                      FM TO @FM VM TO @VM
*17-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*-------------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COLLATERAL

    GOSUB INITIALISE
    GOSUB OPEN.FILES

    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END


RETURN  ;* Program RETURN
*------------------------------------------------------------------------------

PROCESS:
*======

    Y.RAT.DATE = R.NEW(COLL.LOCAL.REF)<1,WPOS.RAT.DATE>
    Y.ACTUAL.DATE = R.NEW(COLL.VALUE.DATE)

    IF LEN(TRIM(Y.RAT.DATE)) GT 0 THEN
        IF Y.RAT.DATE LT Y.ACTUAL.DATE THEN
            AF = COLL.LOCAL.REF
            AV = YPOS<1,1>
            ETEXT = 'ST-DATE.LESS'
            CALL STORE.END.ERROR
        END
    END


*VERIFY THE APPRAISAL DATE -  FECHA TASACION

    Y.FEC.VENC = R.NEW(COLL.LOCAL.REF)<1,WPOS.FEC.VENC>
    IF Y.FEC.VENC  LT Y.ACTUAL.DATE THEN
        TEXT = "COLL.FEC.VENC.BR"
        M.CONT = DCOUNT(R.NEW(COLL.VALUE.DATE),@VM) + 1
        CALL STORE.OVERRIDE(M.CONT)
    END
RETURN
*------------------------------------------------------------------------

INITIALISE:
*=========

    PROCESS.GOAHEAD = 1
    Y.ACTUAL.DATE = TODAY

*Set the local fild for read
*FECHA TASACION - APPRAISAL DATE
    WCAMPO     = "L.COL.EXE.DATE"

*FECHA VENCIMIENTO  - EXPIRATION DATE
    WCAMPO<2>  = "L.COL.VA.DUE.DT"

    WCAMPO = CHANGE(WCAMPO,@FM,@VM)
    YPOS=0

*Get the position for all fields
    CALL MULTI.GET.LOC.REF("COLLATERAL",WCAMPO,YPOS)

    WPOS.RAT.DATE  = YPOS<1,1>
    WPOS.FEC.VENC  = YPOS<1,2>

    FN.COLLATERAL   = 'F.COLLATERAL'
    F.COLLATERAL    = ''
    R.COLLATERAL    = ''

RETURN
*------------


*------------------------
OPEN.FILES:
*=========
    CALL OPF(FN.COLLATERAL,F.COLLATERAL)
RETURN

END
